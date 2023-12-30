unit UGds;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, Types, fgl, UGeometryUtils;

const
  dtNODATA = $00;
  dtBITS16 = $01;
  dtINT2 = $02;
  dtINT4 = $03;
  dtREAL8 = $05;
  dtASCII = $06;

  htBGNLIB = $01;
  htLIBNAME = $02;
  htUNITS = $03;
  htBGNSTR = $05;
  htSTRNAME = $06;
  htENDSTR = $07;
  htBOUNDARY = $08;
  htPATH = $09;
  htSREF = $0A;
  htAREF = $0B;
  htTEXT = $0C;
  htNODE = $15;
  htBOX = $2D;

  htXY = $10;
  htSNAME = $12;
  htSTRING = $19;
  htENDEL = $11;

type
  TInt16s = array of int16;
  TInt32s = array of int32;
  TDoubles = array of double;
  TXY = array [0 .. 1] of double;
  TCoords = array of TXY;

  TDateTimeItems = record
    items: array [0 .. 5] of word;
  end;

  { TGdsObject }

  TGdsObject = class(TObject)
  private
    FParent: TGdsObject;
  protected
    function ShortClassName: string;
  public
    function ToString: string; override;
    function GetRoot: TGdsObject;
    property Parent: TGdsObject read FParent;
  end;



  TGdsElement = class(TGdsObject)
  private
    FXY: TInt32s;
    FCoords: TCoords;
    FExtentBounds: TRectangleF;
    FExtentBoundsPtr: ^TRectangleF;
  public
    class function CreateFromHeaderByte(AByte: byte): TGdsElement;
    destructor Destroy; override;
    function ToString: string; override;
    function GetCoords: TCoords;
    function GetExtentBounds: TRectangleF;
    function IsReference: boolean; virtual;
  public
    property Coords: TCoords read GetCoords;
  protected
    function LookupExtentBounds: TRectangleF; virtual;
  private
    function LookupCoords: TCoords;
  end;

  TGdsElementClass = class of TGdsElement;

  TGdsBoundary = class(TGdsElement)
  end;

  TGdsPath = class(TGdsElement)
  end;

  TGdsText = class(TGdsElement)
  private
    FContents: string;
  public
    function ToString: string; override;
    property Contents: string read FContents;
  end;

  TGdsStructure = class;

  TGdsSref = class(TGdsElement)
  private
    FRefName: string;
    FRefStructure: TGdsStructure;
  public
    function ToString: string; override;
    function IsReference: boolean; override;
    function GetRefStructure: TGdsStructure;
    property RefName: string read FRefName;
    property RefStructure: TGdsStructure read GetRefStructure;
  end;

  TGdsAref = class(TGdsSref)
  end;

  TGdsNode = class(TGdsElement)
  end;

  TGdsBox = class(TGdsElement)
  end;

  TGdsElements = specialize TFPGObjectList<TGdsElement>;

  TGdsStructure = class(TGdsObject)
  private
    FName: string;
    FCreated: TDateTimeItems;
    FLastModified: TDateTimeItems;
    FElements: TGdsElements;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName;
    property Elements: TGdsElements read FElements;
  public
    procedure SplitElements(Primitives: TGdsElements; Others: TGdsElements);
  end;

  TGdsStructures = specialize TFPGObjectList<TGdsStructure>;
  TGdsStructureMap = specialize TFPGMapObject<string, TGdsStructure>;

  TGdsLibrary = class(TGdsObject)
  private
    FName: string;
    FLastModified: TDateTimeItems;
    FLastAccessed: TDateTimeItems;
    FUserUnit: double;
    FMeterUnit: double;
    FStructures: TGdsStructures;
    FStructureMap: TGdsStructureMap;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Structure: TGdsStructure);
    function StructureNames: TStringArray;
    function StructureNamed(Name: string): TGdsStructure;
    property Name: string read FName;
    property Structures: TGdsStructures read FStructures;
  end;

  TGdsInform = class;

  TGdsInformBytesEvent = procedure(const ABytes: TBytes;
    ASender: TGdsInform) of object;

  TGdsInform = class(TObject)
  private
    FOnBytes: TGdsInformBytesEvent;
    FStream: TFileStream;
    FLibrary: TGdsLibrary;
    FStructure: TGdsStructure;
    FElement: TGdsElement;
    FFileName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property FileName: string read FFileName write FFileName;
    property OnBytes: TGdsInformBytesEvent read FOnBytes write FOnBytes;
    property GdsLibrary: TGdsLibrary read FLibrary;
    function ExtractAscii(const ABytes: TBytes): string;
  private
    procedure HandleRecord(const ABytes: TBytes);
    function ExtractInt2(const ABytes: TBytes): TInt16s;
    function ExtractInt4(const ABytes: TBytes): TInt32s;
    function ExtractReal8(const ABytes: TBytes): TDoubles;
  end;

function FileSizeEx(const AFileName: string): longint;
function CalcBounds(Coords: TCoords): TRectangleF;

implementation

uses
  UUtils, Math, LazLogger;

  { GDSreader }

// @note
// serbanp@ix.netcom.com's GDSreader.0.3.2
function GDSreadInt2(rec: pbyte): int16;
begin
  Result := rec[0];
  Result := Result shl 8;
  Inc(Result, rec[1]);
  if Result and $8000 <> 0 then
  begin
    Result := Result and $7FFF;
    Result := Result xor $7FFF;
    Inc(Result);
    Result := -Result;
  end;
end;


function GDSreadInt4(rec: pbyte): int32;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to 3 do
  begin
    Result := Result shl 8;
    Inc(Result, rec[i]);
  end;
  if Result and $80000000 <> 0 then
  begin
    Result := Result and $7FFFFFFF;
    Result := Result xor $7FFFFFFF;
    Inc(Result);
    Result := -Result;
  end;
end;


function GDSreadReal8(rec: pbyte): double;
var
  i, sign, exponent: integer;
  mantissa_int: uint64;
  mantissa_float: double;
begin
  sign := rec[0] and $80;
  exponent := (rec[0] and $7F) - 64;
  mantissa_int := 0;
  for i := 1 to 7 do
  begin
    mantissa_int := mantissa_int shl 8;
    Inc(mantissa_int, rec[i]);
  end;
  mantissa_float := double(mantissa_int) / Power(2, 56);
  Result := mantissa_float * Power(16, exponent);
  if sign <> 0 then
    Result := -Result;
end;


function CalcBounds(Coords: TCoords): TRectangleF;
var
  P: TPointF;
  i: integer;
begin
  Result := EmptyRectangleF;
  for i := 0 to High(Coords) do
  begin
    P := PointF(single(Coords[i][0]), single(Coords[i][1]));
    Result.Origin.x := Min(P.x, Result.Origin.x);
    Result.Origin.y := Min(P.y, Result.Origin.y);
    Result.Corner.x := Max(P.x, Result.Corner.x);
    Result.Corner.y := Max(P.x, Result.Corner.y);
  end;
end;
{ TGdsLibrary }

constructor TGdsLibrary.Create;
begin
  inherited;
  FStructures := TGdsStructures.Create;
  FStructureMap := TGdsStructureMap.Create;
end;


destructor TGdsLibrary.Destroy;
begin
  FreeAndNil(FStructureMap);
  FreeAndNil(FStructures);
  inherited;
end;


procedure TGdsLibrary.Add(Structure: TGdsStructure);
begin
  FStructures.Add(Structure);
  FStructureMap.Add(Structure.Name, Structure);
  Structure.FParent := self;
end;


function TGdsLibrary.StructureNames: TStringArray;
var
  names: TStringList;
  S: TGdsStructure;
begin
  names := TStringList.Create;
  for S in FStructures do
  begin
    names.Add(S.Name);
  end;
  Result := names.ToStringArray;
  FreeAndNil(names);
end;


function TGdsLibrary.StructureNamed(Name: string): TGdsStructure;
begin
  Result := FStructureMap.KeyData[Name];
end;

{ TGdsStructure }

constructor TGdsStructure.Create;
begin
  inherited;
  FElements := TGdsElements.Create;
end;


destructor TGdsStructure.Destroy;
begin
  FreeAndNil(FElements);
  inherited;
end;


procedure TGdsStructure.SplitElements(Primitives: TGdsElements; Others: TGdsElements);
var
  E: TGdsElement;
begin
  for E in FElements do
  begin
    if E.IsReference then
    begin
      if Others <> nil then
      begin
        Others.Add(E);
      end;
    end
    else
    begin
      if Primitives <> nil then
      begin
        Primitives.Add(E);
      end;
    end;
  end;
end;
{ TGdsInform }

constructor TGdsInform.Create;
begin
  inherited;
  WriteLn('TGdsInform.Create');
  FFileName := '';
  FStream := nil;
  FLibrary := nil;
  FElement := nil;
end;


destructor TGdsInform.Destroy;
begin
  WriteLn('TGdsInform.Destroy');
  inherited;
end;


procedure TGdsInform.Execute;
var
  recSize: integer;
  bytesSize: integer;
  buff: TBytes;
  numRead: integer;
  recCount: longint;
begin
  if not FileExists(FileName) then
  begin
    WriteLn('*** ERROR *** File not found.');
    Exit;
  end;
  if FileSizeEx(FileName) <= 6 then
  begin
    WriteLn('*** ERROR *** Size too small. Not A GDS FILE.');
    Exit;
  end;
  FStream := TFileStream.Create(FileName, fmOpenRead);
  try
    recCount := 0;
    repeat
      recSize := NtoBE(FStream.ReadWord);
      if (recCount = 0) and (recSize <> 6) then
      begin
        WriteLn('*** ERROR *** Not A GDS FILE');
        break;
      end;
      if recSize >= 2048 then
      begin
        WriteLn('*** ERROR *** Record so big');
        WriteLn(Format('Record[%d] Length is %d', [recCount, recSize]));
        break;
      end;
      DebugLn(Format('First Record Length is %d', [recSize]));
      bytesSize := recSize - 2;
      if bytesSize <= 0 then
        break;
      if bytesSize = 1 then
      begin
        WriteLn('*** ERROR *** broken header');
        break;
      end;
      SetLength(buff, bytesSize);
      FillByte(buff[0], bytesSize, 0);
      numRead := FStream.Read(buff[0], bytesSize);
      if numRead > 0 then
      begin
        if Assigned(FOnBytes) then
          FOnBytes(buff, Self);
        HandleRecord(buff);
        Inc(recCount);
      end;
    until (recSize = 0) or (numRead = 0);
    WriteLn('Count Records: ', recCount);
  finally
    FreeAndNil(FStream);
  end;
end;


procedure TGdsInform.HandleRecord(const ABytes: TBytes);
var
  int2array: TInt16s;
  doubleArray: TDoubles;


  procedure SetDateTimeItems(var ADateTime: TDateTimeItems; AInts: TInt16s;
    AOffset: integer);
  var
    i: integer;
  begin
    for i := 0 to 5 do
      ADateTime.items[i] := AInts[i + AOffset];
  end;

begin
  case ABytes[0] of
    htBGNLIB:
    begin
      FLibrary := TGdsLibrary.Create;
      int2array := ExtractInt2(ABytes);
      SetDateTimeItems(FLibrary.FLastModified, int2array, 0);
      SetDateTimeItems(FLibrary.FLastAccessed, int2array, 6);
    end;
    htLIBNAME:
      FLibrary.FName := ExtractAscii(ABytes);
    htUNITS:
    begin
      doubleArray := ExtractReal8(ABytes);
      FLibrary.FUserUnit := doubleArray[0];
      FLibrary.FMeterUnit := doubleArray[1];
    end;
    htBGNSTR:
    begin
      FStructure := TGdsStructure.Create;
      int2array := ExtractInt2(ABytes);
      SetDateTimeItems(FStructure.FCreated, int2array, 0);
      SetDateTimeItems(FStructure.FLastModified, int2array, 6);
    end;
    htSTRNAME:
      FStructure.FName := ExtractAscii(ABytes);
    htENDSTR:
    begin
      FLibrary.Add(FStructure);
      FStructure := nil;
    end;
    htBOUNDARY, htPATH, htSREF, htAREF, htTEXT, htNODE, htBOX:
    begin
      FElement := TGdsElement.CreateFromHeaderByte(ABytes[0]);
      DebugLn('NewElement: ', string(FElement.ClassName).Remove(0, 4).ToUpper);
    end;
    htSNAME:
      (FElement as TGdsSref).FRefName := ExtractAScii(ABytes);
    htSTRING:
      (FElement as TGdsText).FContents := ExtractAScii(ABytes);
    htXY:
      FElement.FXY := ExtractInt4(ABytes);
    htENDEL:
    begin
      if FElement <> nil then
      begin
        FStructure.FElements.Add(FElement);
        FElement.FParent := FStructure;
        FElement := nil;
      end;
    end;
    else
  end;
end;


function TGdsInform.ExtractInt2(const ABytes: TBytes): TInt16s;
var
  i: integer;
  bodySize: integer;
  numItems: integer;
begin
  Result := [];
  bodySize := Length(Abytes) - 2;
  numItems := bodySize div sizeof(int16);
  SetLength(Result, numItems);
  for i := 0 to numItems - 1 do
    Result[i] := GDSreadInt2(@ABytes[i * 2 + 2]);
end;


function TGdsInform.ExtractInt4(const ABytes: TBytes): TInt32s;
var
  i: integer;
  bodySize: integer;
  numItems: integer;
begin
  Result := [];
  bodySize := Length(Abytes) - 2;
  numItems := bodySize div sizeof(int32);
  SetLength(Result, numItems);
  for i := 0 to numItems - 1 do
    Result[i] := GDSreadInt4(@ABytes[i * 4 + 2]);
end;


function TGdsInform.ExtractReal8(const ABytes: TBytes): TDoubles;
var
  i: integer;
  bodySize: integer;
  numItems: integer;
begin
  Result := [];
  bodySize := Length(Abytes) - 2;
  numItems := bodySize div sizeof(double);
  SetLength(Result, numItems);
  for i := 0 to numItems - 1 do
    Result[i] := GDSreadReal8(@ABytes[i * 8 + 2]);
end;


function TGdsInform.ExtractAscii(const ABytes: TBytes): string;
var
  buff: array [0 .. 511] of byte;
begin
  Result := '';
  if ABytes[1] <> dtASCII then
    Exit;
  DebugLn('Buff: ', FormatByteArray(ABytes));
  FillByte(buff[0], sizeof(buff), 0);
  Move(ABytes[2], buff[0], Length(ABytes) - 2);
  Result := Format('%s', [PChar(@buff[0])]);
end;


function FileSizeEx(const AFileName: string): longint;
var
  F: file of byte;
begin
  Result := -1;
  if not FileExists(AFileName) then
    Exit;
  try
    AssignFile(F, AFileName);
    Reset(F);
    Result := FileSize(F);
  finally
    Close(F);
  end;
end;

{ TGdsObject }

function TGdsObject.ShortClassName: string;
begin
  Result := ClassName;
  if Result.StartsWith('TGds') then
    Result := Result.Remove(0, 4);
end;


function TGdsObject.ToString: string;
begin
  Result := 'a ' + ShortClassName;
end;


function TGdsObject.GetRoot: TGdsObject;
var
  o: TGdsObject;
begin
  o := Self;
  while True do
  begin
    if o.Parent = nil then
    begin
      break;
    end
    else
      o := o.Parent;
  end;
  Result := o;
end;

{ TGdsElement }

destructor TGdsElement.Destroy;
begin
  FreeAndNil(FXY);
  FreeAndNil(FCoords);
  inherited;
end;


function TGdsElement.ToString: string;
begin
  Result := ShortClassName.ToUpper;
end;


function TGdsElement.LookupCoords: TCoords;
var
  lib: TGdsLibrary;
  AXY: TXY;
  i: integer;
begin
  Result := [];
  lib := GetRoot as TGdsLibrary;
  SetLength(Result, Length(FXY) div 2);
  for i := 0 to High(Result) do
  begin
    AXY[0] := FXY[i * 2 + 0] * lib.FUserUnit;
    AXY[1] := FXY[i * 2 + 1] * lib.FUserUnit;
    Result[i] := AXY;
  end;
end;


function TGdsElement.LookupExtentBounds: TRectangleF;
begin
  Result := CalcBounds(FCoords);
end;


function TGdsElement.GetExtentBounds: TRectangleF;
begin
  if FExtentBoundsPtr = nil then
  begin
    FExtentBounds := LookupExtentBounds;
    FExtentBoundsPtr := @FExtentBounds;
  end;
  Result := FExtentBounds;
end;


function TGdsElement.GetCoords: TCoords;
begin
  if FCoords = nil then
  begin
    FCoords := LookupCoords;
  end;
  Result := FCoords;
end;


function TGdsElement.IsReference: boolean;
begin
  Result := False;
end;


class function TGdsElement.CreateFromHeaderByte(AByte: byte): TGdsElement;
var
  clazz: TGdsElementClass;
begin
  case AByte of
    htBOUNDARY: clazz := TGdsBoundary;
    htPATH: clazz := TGdsPath;
    htSREF: clazz := TGdsSref;
    htAREF: clazz := TGdsAref;
    htTEXT: clazz := TGdsText;
    htNODE: clazz := TGdsNode;
    htBOX: clazz := TGdsBox;
    else
      clazz := nil;
  end;
  if clazz = nil then
    Exit(nil);
  Result := clazz.Create;
end;

{ TGdsText }
function TGdsText.ToString: string;
begin
  Result := inherited + '(' + Contents + ')';
end;

{ TGdsSref }
function TGdsSref.ToString: string;
begin
  Result := inherited + '(' + RefName + ')';
end;


function TGdsSref.IsReference: boolean;
begin
  Result := True;
end;


function TGdsSref.GetRefStructure: TGdsStructure;
begin
  if FRefStructure = nil then
  begin
    FRefStructure := (Parent as TGdsLibrary).StructureNamed(RefName);
  end;
  Result := FRefStructure;
end;

end.
