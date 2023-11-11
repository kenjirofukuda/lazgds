unit UGdsBrowserForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  Menus, ExtCtrls, UGds;

type

  { TGdsBrowserForm }

  TGdsBrowserForm = class(TForm)
    BrowserPanel: TPanel;
    ElementListBox: TListBox;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ChooseGdsMenuItem: TMenuItem;
    GdsFileNameLabel: TLabel;
    OpenGdsDialog: TOpenDialog;
    StructureListBox: TListBox;
    XYListView: TListView;

    procedure BrowserPanelClick(Sender: TObject);
    procedure GdsFileNameLabelClick(Sender: TObject);
    procedure OpenGdsFile(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure HandleBytes(const ABytes: TBytes; Sender: TGdsInform);
    procedure StructureListBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure ElementListBoxSelectionChange(Sender: TObject; User: Boolean);

  private
    FGdsInform: TGdsInform;

  public

  end;


var
  GdsBrowserForm: TGdsBrowserForm;

implementation

{$R *.lfm}

uses
  UUtils, LazLogger;

{ TGdsBrowserForm }

procedure TGdsBrowserForm.OpenGdsFile(Sender: TObject);
var
  S: TGdsStructure;
begin
  if OpenGdsDialog.Execute then
    GdsFileNameLabel.Caption := OpenGdsDialog.FileName
  else
    Exit;
  if not FileExists(GdsFileNameLabel.Caption) then
    Exit;
  FGdsInform.FileName := GdsFileNameLabel.Caption;
  FGdsInform.Execute;
  StructureListBox.Clear;
  ElementListBox.Clear;
  XYListView.Clear;
  if FGdsInform.GdsLibrary = nil then
    Exit;
  for S in FGdsInform.GdsLibrary.Structures do
  begin
    StructureListBox.AddItem(S.Name, S);
  end;
  Caption := FGdsInform.GdsLibrary.Name;
end;

procedure TGdsBrowserForm.GdsFileNameLabelClick(Sender: TObject);
begin

end;

procedure TGdsBrowserForm.BrowserPanelClick(Sender: TObject);
begin

end;

procedure TGdsBrowserForm.FormCreate(Sender: TObject);
begin
  FGdsInform := TGdsInform.Create;
  FGdsInform.OnBytes := @HandleBytes;
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
  XYListView.Clear;
  for AXY in E.GetCoords do
  begin
    with XYListView.Items.Add do
    begin
      SubItems.Add(AXY[0].ToString);
      SubItems.Add(AXY[1].ToString);
    end;
  end;
  //for i := 0 to Length(E.XY) div 2 - 1 do
  //begin
  //  with XYListView.Items.Add do
  //  begin
  //    SubItems.Add(E.XY[i * 2 + 0].ToString);
  //    SubItems.Add(E.XY[i * 2 + 1].ToString);
  //  end;
  //end;
end;

end.
