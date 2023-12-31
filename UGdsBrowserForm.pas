unit UGdsBrowserForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus, ExtCtrls, UGds, UGdsView, UGdsStation;

type

  { TGdsBrowserForm }

  TGdsBrowserForm = class(TForm)
    BrowserPanel: TPanel;
    ElementListBox: TListBox;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    GdsFileNameLabel: TLabel;
    OpenGdsDialog: TOpenDialog;
    StatusBar: TStatusBar;
    StructureListBox: TListBox;
    XYListView: TListView;
    { Menu Items }
    ChooseGdsMenuItem: TMenuItem;
    Separator1: TMenuItem;
    QuitMenuItem: TMenuItem;

    procedure QuitMenuItemClick(Sender: TObject);
    procedure OpenGdsFile(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure HandleBytes(const ABytes: TBytes; Sender: TGdsInform);
    procedure StructureListBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure ElementListBoxSelectionChange(Sender: TObject; User: Boolean);

  private
    FGdsInform: TGdsInform;
    FGdsView: TGdsView;

  public

  end;


var
  GdsBrowserForm: TGdsBrowserForm;

implementation

{$R *.lfm}

uses
  LazLogger;

{ TGdsBrowserForm }

procedure TGdsBrowserForm.OpenGdsFile(Sender: TObject);
var
  structure: TGdsStructure;
begin
  if OpenGdsDialog.Execute then
    GdsFileNameLabel.Caption := OpenGdsDialog.FileName
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
  Caption := FGdsInform.GdsLibrary.Name;
end;


procedure TGdsBrowserForm.QuitMenuItemClick(Sender: TObject);
begin
  GdsBrowserForm.Close;
end;


procedure TGdsBrowserForm.FormCreate(Sender: TObject);
begin
  FGdsInform := TGdsInform.Create;
  FGdsInform.OnBytes := @HandleBytes;
  FGdsView := TGdsView.Create(BrowserPanel);
  with FGdsView do
  begin
    AnchorSideLeft.Control := XYListView;
    AnchorSideLeft.Side := asrBottom;
    AnchorSideTop.Control := BrowserPanel;
    AnchorSideRight.Control := BrowserPanel;
    AnchorSideRight.Side := asrBottom;
    AnchorSideBottom.Control := BrowserPanel;
    AnchorSideBottom.Side := asrBottom;
    Anchors := [akTop, akLeft, akRight, akBottom];
    Caption := 'GdsView';
    ParentBackground := False;
    ParentColor := False;
    Color := clYellow;
    Visible := True;
    Parent := BrowserPanel;
  end;
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
  User: Boolean);
var
  i: Integer;
  S: TGdsStructure;
  E: TGdsElement;
begin
  i := StructureListBox.ItemIndex;
  if i < 0 then
    Exit;
  S := StructureListBox.Items.Objects[i] as TGdsStructure;
  GdsStation.GdsStructure := S;
  FGdsView.Viewport.SetWorldBounds(S.GetExtentBounds);
  FGdsView.Invalidate;
  ElementListBox.Clear;
  XYListView.Clear;
  for E in S.Elements do
  begin
    ElementListBox.AddItem(E.ToString, E);
  end;
end;


procedure TGdsBrowserForm.ElementListBoxSelectionChange(Sender: TObject; User: Boolean);
var
  i: Integer;
  E: TGdsElement;
  AXY: TXY;
begin
  i := ElementListBox.ItemIndex;
  if i < 0 then
    Exit;
  E := ElementListBox.Items.Objects[i] as TGdsElement;
  GdsStation.GdsElement := E;
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
