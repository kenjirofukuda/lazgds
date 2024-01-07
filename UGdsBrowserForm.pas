unit UGdsBrowserForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus, ExtCtrls, JSONPropStorage, PairSplitter, UGds, UGdsView, UGdsStation;

type

  { TGdsBrowserForm }

  TGdsBrowserForm = class(TForm)
    ContentsPanel: TPanel;
    ElementListBox: TListBox;
    JSONPropStorage: TJSONPropStorage;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    GdsFileNameLabel: TLabel;
    DebugMenu: TMenuItem;
    OpenSandboxMenuItem: TMenuItem;
    OpenGdsDialog: TOpenDialog;
    PairSplitter: TPairSplitter;
    PairSplitterSideLists: TPairSplitterSide;
    PairSplitterSideCanvas: TPairSplitterSide;
    DummyPanel: TPanel;
    StatusBar: TStatusBar;
    StructureListBox: TListBox;
    XYListView: TListView;
    { Menu Items }
    ChooseGdsMenuItem: TMenuItem;
    Separator1: TMenuItem;
    QuitMenuItem: TMenuItem;
  private
    FGdsInform: TGdsInform;
    FGdsView: TGdsView;
  published

    procedure OpenSandboxMenuItemClick(Sender: TObject);
    procedure PairSplitterSideCanvasMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure OpenGdsFile(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure HandleBytes(const ABytes: TBytes; Sender: TGdsInform);
    procedure StructureListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure ElementListBoxSelectionChange(Sender: TObject; User: boolean);
    property GdsView: TGdsView read FGdsView;
  public
  end;


var
  GdsBrowserForm: TGdsBrowserForm;

implementation

{$R *.lfm}

uses
  LazLogger, LazUtils, USandboxForm;

  { TGdsBrowserForm }

procedure TGdsBrowserForm.OpenGdsFile(Sender: TObject);
const
  GDS_PATH_KEY = 'LastOpendGdsFilePath';
var
  structure: TGdsStructure;
  lastOpendPath: string;
begin
  lastOpendPath := JSONPropStorage.ReadString(GDS_PATH_KEY, '');
  if lastOpendPath <> '' then
  begin
    OpenGdsDialog.InitialDir := ExtractFileDir(lastOpendPath);
  end;
  if OpenGdsDialog.Execute then
  begin
    GdsFileNameLabel.Caption := OpenGdsDialog.FileName;
    JSONPropStorage.WriteString(GDS_PATH_KEY, OpenGdsDialog.FileName);
  end
  else
    Exit;
  if not FileExists(OpenGdsDialog.FileName) then
    Exit;
  FGdsInform.FileName := OpenGdsDialog.FileName;
  FGdsInform.Execute;
  StructureListBox.Clear;
  ElementListBox.Clear;
  XYListView.Clear;
  if FGdsInform.GdsLibrary = nil then
    Exit;
  GdsStation.GdsLibrary := FGdsInform.GdsLibrary;
  for structure in FGdsInform.GdsLibrary.Structures do
  begin
    StructureListBox.AddItem(structure.Name, structure);
  end;
  Caption := Application.Title + ': ' + GdsStation.GdsLibrary.Name;
end;


procedure TGdsBrowserForm.QuitMenuItemClick(Sender: TObject);
begin
  GdsBrowserForm.Close;
end;


procedure TGdsBrowserForm.OpenSandboxMenuItemClick(Sender: TObject);
begin
  SandboxForm.Visible := True;
  {$IFDEF Windows}
  SandboxForm.TimerOnce.Enabled := True;
  {$ENDIF}
end;


procedure TGdsBrowserForm.PairSplitterSideCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;


function ConfigFilePath: string;
var
  path: string;
begin
  path := GetAppConfigFile(False, True);
  ForceDirectories(ExtractFileDir(path));
  Result := path;
end;


procedure TGdsBrowserForm.FormCreate(Sender: TObject);
begin
  FGdsInform := TGdsInform.Create;
  FGdsInform.OnBytes := @HandleBytes;
  FGdsView := TGdsView.Create(ContentsPanel);
  JSONPropStorage.JSONFileName := ConfigFilePath;
  with FGdsView do
  begin
    AnchorSideLeft.Control := PairSplitterSideCanvas;
    AnchorSideTop.Control := PairSplitterSideCanvas;
    AnchorSideRight.Control := PairSplitterSideCanvas;
    AnchorSideRight.Side := asrBottom;
    AnchorSideBottom.Control := PairSplitterSideCanvas;
    AnchorSideBottom.Side := asrBottom;
    Anchors := [akTop, akLeft, akRight, akBottom];
    Caption := 'GdsView';
    ParentBackground := False;
    ParentColor := False;
    Color := clYellow;
    Visible := True;
    Parent := PairSplitterSideCanvas;
  end;
  DummyPanel.Visible := False;
  {kill design layout color}
  StructureListBox.Color := clDefault;
  ElementListBox.Color := clDefault;
  XYListView.Color := clDefault;
  PairSplitter.Color := clDefault;
end;


procedure TGdsBrowserForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGdsInform);
end;


procedure TGdsBrowserForm.HandleBytes(const ABytes: TBytes; Sender: TGdsInform);
begin
  if ABytes[1] <> dtASCII then
    Exit;
  DebugLn(Sender.ExtractAscii(ABytes));
end;




procedure TGdsBrowserForm.StructureListBoxSelectionChange(Sender: TObject;
  User: boolean);
var
  i: integer;
  S: TGdsStructure;
  E: TGdsElement;
begin
  i := StructureListBox.ItemIndex;
  if i < 0 then
    Exit;
  S := StructureListBox.Items.Objects[i] as TGdsStructure;
  GdsStation.GdsStructure := S;
  GdsStation.GdsElement := nil;
  ElementListBox.Clear;
  XYListView.Clear;
  for E in S.Elements do
  begin
    ElementListBox.AddItem(E.ToString, E);
  end;
end;


procedure TGdsBrowserForm.ElementListBoxSelectionChange(Sender: TObject; User: boolean);
var
  i: integer;
  E: TGdsElement;
  AXY: TXY;
begin
  i := ElementListBox.ItemIndex;
  if i < 0 then
    Exit;
  E := ElementListBox.Items.Objects[i] as TGdsElement;
  GdsStation.GdsElement := E;
  FGdsView.Invalidate;
  XYListView.Clear;
  for AXY in E.Coords do
  begin
    with XYListView.Items.Add do
    begin
      SubItems.Add(AXY[0].ToString);
      SubItems.Add(AXY[1].ToString);
    end;
  end;
end;

end.
