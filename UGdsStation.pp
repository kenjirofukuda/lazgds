unit UGdsStation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UGds;

type
  TGdsStation = class
  private
    FGdsLibrary: TGdsLibrary;
    FGdsStructure: TGdsStructure;
    FGdsElement: TGdsElement;
  public
    destructor Destroy; override;
    property GdsLibrary: TGdsLibrary read FGdsLibrary write FGdsLibrary;
    property GdsStructure: TGdsStructure read FGdsStructure write FGdsStructure;
    property GdsElement: TGdselement read FGdsElement write FGdsElement;
  end;

var
  GdsStation: TGdsStation;

implementation

uses
  LazLogger;

destructor TGdsStation.Destroy;
begin
  DebugLn('TGdsStation.Destroy');
  FreeAndNil(FGdsLibrary);
end;

initialization
  GdsStation := TGdsStation.Create;

finalization
  FreeAndNil(GdsStation);
end.
