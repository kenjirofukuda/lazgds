unit UGds;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, Types, fgl, BGRATransform, UGeometryUtils;

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
  htPATHTYPE = $21;
  htLAYER = $0D;
  htWIDTH = $0F;
  htSTRANS = $1A;
  htMAG = $1B;
  htANGLE = $1C;
  htCOLROW = $13;
  htDATATYPE = $0E;


const
  Pi_Half = 0.5 * Pi;
  Pi_Double = 2.0 * Pi;

  BUTT_END = 0;
  ROUND_END = 1;
  EXTENDED_END = 2;
  CUSTOMPLUS_END = 4;

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
    constructor Create; virtual;
    function ToString: string; override;
    function GetRoot: TGdsObject;
    property Parent: TGdsObject read FParent;
  end;


  { TGdsElement }

  TGdsElement = class(TGdsObject)
  private
    FCoords: TCoords;
    FLayer: integer;
    FDataType: integer;
    FExtentBounds: TRectangleF;
    FExtentBoundsPtr: ^TRectangleF;
  public
    class function CreateFromHeaderByte(AByte: byte): TGdsElement;
    constructor Create; override;
    destructor Destroy; override;
    function ToString: string; override;
    function GetExtentBounds: TRectangleF;
    function IsReference: boolean; virtual;
    procedure DebugStringsOn(AStrings: TStringList); virtual;
  public
    property Coords: TCoords read FCoords;
    property Layer: integer read FLayer;
  protected
    function LookupExtentBounds: TRectangleF; virtual;
  end;


  TGdsElementClass = class of TGdsElement;


  TGdsBoundary = class(TGdsElement)
  end;


  { TGdsPath }

  TGdsPath = class(TGdsElement)
  private
    FOutlineCoords: TCoords;
    FPathType: integer;
    FWidth: double;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DebugStringsOn(AStrings: TStringList); override;
    property PathType: integer read FPathType write FPathType;
    property Width: double read FWidth write FWidth;
    function OutlineCoords: TCoords;
  end;

  TGdsStructure = class;

  { TGdsSref }

  TGdsSref = class(TGdsElement)
  private
    FRefName: string;
    FRefStructure: TGdsStructure;
    FAngleDeg: double;
    FMag: double;
    FStrans: uint16;
    FTransform: TAffineMatrix;
    FTransformPtr: ^TAffineMatrix;
  public
    constructor Create; override;
    procedure DebugStringsOn(AStrings: TStringList); override;
    function LookupExtentBounds: TRectangleF; override;
    function ToString: string; override;
    function IsReference: boolean; override;
    function GetRefStructure: TGdsStructure;
    function GetTransform: TAffineMatrix;
    function IsRefrected: boolean;
    function IsAbsAngle: boolean;
    function IsAbsMag: boolean;
    property RefName: string read FRefName;
    property RefStructure: TGdsStructure read GetRefStructure;
  private
    function LookupTransform: TAffineMatrix;
  end;


  { TGdsAref }
  TAffineMatrixArray = array of TAffineMatrix;

  TGdsAref = class(TGdsSref)
  private
    FCols: integer;
    FRows: integer;
    FColStep: double;
    FRowStep: double;
    FRepeatedTransforms: TAffineMatrixArray;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure DebugStringsOn(AStrings: TStringList); override;
    function RepeatedTransforms: TAffineMatrixArray;
  private
    function LookupRepeatedTransforms: TAffineMatrixArray;
  end;


  { TGdsText }

  TGdsText = class(TGdsSref)
  private
    FContents: string;
  public
    function LookupExtentBounds: TRectangleF; override;
    function ToString: string; override;
    property Contents: string read FContents;
  end;


  TGdsNode = class(TGdsElement)
  end;


  TGdsBox = class(TGdsElement)
  end;


  TGdsElements = specialize TFPGObjectList<TGdsElement>;
  TLayerElementsMap = specialize TFPGMapObject<integer, TGdsElements>;
  TIntList = specialize TFPGList<integer>;
  { TGdsStructure }

  TGdsStructure = class(TGdsObject)
  private
    FName: string;
    FCreated: TDateTimeItems;
    FLastModified: TDateTimeItems;
    FElements: TGdsElements;
    FExtentBounds: TRectangleF;
    FExtentBoundsPtr: ^TRectangleF;
    FLayerElementsMap: TLayerElementsMap;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetExtentBounds: TRectangleF;
    property Name: string read FName;
    property Elements: TGdsElements read FElements;
  public
    procedure SplitElements(Primitives: TGdsElements; Others: TGdsElements);
  private
    function LookupExtentBounds: TRectangleF; virtual;
  end;

  TGdsStructures = specialize TFPGObjectList<TGdsStructure>;
  TGdsStructureMap = specialize TFPGMapObject<string, TGdsStructure>;

  { TGdsLibrary }

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
    function UsedLayerNumbers: TInt32s;
    property Name: string read FName;
    property Structures: TGdsStructures read FStructures;
  end;

  TGdsInform = class;

  TGdsInformBytesEvent = procedure(const ABytes: TBytes;
    ASender: TGdsInform) of object;

  { TGdsInform }

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
    function ExtractBitmask(const ABytes: TBytes): uint16;
    function ExtractInt2(const ABytes: TBytes): TInt16s;
    function ExtractInt4(const ABytes: TBytes): TInt32s;
    function ExtractReal8(const ABytes: TBytes): TDoubles;
  end;

function FileSizeEx(const AFileName: string): longint;
function CalcBounds(Coords: TCoords): TRectangleF;
function pathOutlineCoords(ACoords: TCoords; APathType: integer;
  AWidth: double): TCoords;
// function CoordsToPoints: array of TPointF;

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


function GDSreadBtimask(rec: pbyte): uint16;
begin
  Result := BEtoN(PUint16(rec)^);
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
  R: TRectangleF;
begin
  R := EmptyRectangleF;
  for i := 0 to High(Coords) do
  begin
    P := PointF(single(Coords[i][0]), single(Coords[i][1]));
    R.Origin.x := Min(P.x, R.Origin.x);
    R.Origin.y := Min(P.y, R.Origin.y);
    R.Corner.x := Max(P.x, R.Corner.x);
    R.Corner.y := Max(P.y, R.Corner.y);
  end;
  Result := R;
end;
{ TGdsLibrary }

constructor TGdsLibrary.Create;
begin
  inherited;
  FStructures := TGdsStructures.Create;
  FStructureMap := TGdsStructureMap.Create(False);
end;


destructor TGdsLibrary.Destroy;
begin
  DebugLn('TGdsLibrary.Destroy');
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


function compInteger(const Item1, Item2: integer): integer;
begin
  if Item1 > Item2 then
    Result := 1
  else if Item1 < Item2 then
    Result := -1
  else
    Result := 0;
end;


function TGdsLibrary.UsedLayerNumbers: TInt32s;
var
  S: TGdsStructure;
  E: TGdsElement;
  layerElementsMap: TLayerElementsMap;
  layer: integer;
  i: integer;
  layerList: TIntList;
  layers: TInt32s;
begin
  layerList := TIntList.Create;
  layerElementsMap := TLayerElementsMap.Create(False);
  try
    for S in Structures do
    begin
      for E in S.Elements do
      begin
        if E.Layer >= 0 then
        begin
          if layerElementsMap.IndexOf(E.Layer) < 0 then
            layerElementsMap[E.Layer] := TGdsElements.Create(True);
          layerElementsMap[E.Layer].Add(E);
        end;
      end;
    end;
    for i := 0 to layerElementsMap.Count - 1 do
    begin
      layer := layerElementsMap.Keys[i];
      DebugLn('Layer = %d', [layer]);
      layerList.Add(layer);
    end;
    layerList.Sort(@compInteger);
    SetLength(layers, layerList.Count);
    for i := Low(layers) to High(layers) do
    begin
      layers[i] := layerList[i];
    end;
    Result := layers;
  finally
    layerElementsMap.Free;
    layerList.Free;
  end;
end;

{ TGdsStructure }

constructor TGdsStructure.Create;
begin
  inherited;
  FExtentBoundsPtr := nil;
  FElements := TGdsElements.Create;
  FElements.FreeObjects := True;
  FLayerElementsMap := TLayerElementsMap.Create(False);
end;


destructor TGdsStructure.Destroy;
begin
  DebugLn('TGdsStructure.Destroy');
  FreeAndNil(FLayerElementsMap);
  FreeAndNil(FElements);
  inherited;
end;


function TGdsStructure.LookupExtentBounds: TRectangleF;
var
  E: TGdsElement;
  B: TRectangleF;
begin
  B := EmptyRectangleF;
  for E in FElements do
  begin
    B := B.Merge(E.GetExtentBounds);
  end;
  Result := B;
end;


function TGdsStructure.GetExtentBounds: TRectangleF;
begin
  if FExtentBoundsPtr = nil then
  begin
    FExtentBounds := LookupExtentBounds;
    FExtentBoundsPtr := @FExtentBounds;
  end;
  Result := FExtentBounds;
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


function InvertPoint(const tx: TAffineMatrix; pt: TXY): TXY;
var
  x, y, det, f: single;
begin
  x := pt[0] - tx[1, 3];
  y := pt[1] - tx[2, 3];
  det := tx[1, 1] * tx[2, 2] - tx[1, 2] * tx[2, 1];
  if det = 0 then
    raise Exception.Create('Not inversible');
  f := 1 / det;
  Result[0] := f * ((x * tx[2, 2]) - (tx[1, 2] * y));
  Result[1] := f * ((tx[1, 1] * y) - (x * tx[2, 1]));
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
    FLibrary.UsedLayerNumbers;
  finally
    FreeAndNil(FStream);
  end;
end;


procedure TGdsInform.HandleRecord(const ABytes: TBytes);
var
  int2Array: TInt16s;
  doubleArray: TDoubles;
  colPoint, rowPoint: TXY;
  intCoords: TInt32s;
  i: integer;
  eAref: TGdsAref;
  strictCheck: boolean;

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
    begin
      intCoords := ExtractInt4(ABytes);
      SetLength(FElement.FCoords, Length(intCoords) div 2);
      for i := Low(FElement.FCoords) to High(FElement.FCoords) do
      begin
        FElement.FCoords[i][0] := intCoords[i * 2 + 0] * FLibrary.FUserUnit;
        FElement.FCoords[i][1] := intCoords[i * 2 + 1] * FLibrary.FUserUnit;
      end;
      intCoords := nil;
      if FElement.ClassType = TGdsAref then
      begin
        strictCheck := False;
        eARef := (FElement as TGdsAref);
        colPoint := InvertPoint(eAref.GetTransform, eAref.FCoords[1]);
        if colPoint[0] < 0.0 then
          raise Exception.Create(
            'Error in AREF! Found a y-axis mirrored array. This is impossible so I''m exiting.');
        if strictCheck and (not SameValue(colPoint[1], 0.0)) then
          raise Exception.Create('Error in AREF! The second point in XY is broken.');
        rowPoint := InvertPoint(eAref.GetTransform, eAref.FCoords[2]);
        if strictCheck and (not SameValue(rowPoint[0], 0.0)) then
          raise Exception.Create('Error in AREF! The third point in XY is broken.');
        eAref.FColStep := colPoint[0] / eAref.FCols;
        eAref.FRowStep := rowPoint[1] / eAref.FRows;
        if strictCheck and (rowPoint[1] < 0.0) then
          eAref.FRowStep := eAref.FRowStep * -1.0;
        SetLength(FElement.FCoords, 1);
      end;
    end;
    htPATHTYPE:
      (FElement as TGdsPath).PathType := ExtractInt2(ABytes)[0];
    htDATATYPE:
      FElement.FDataType := ExtractInt2(ABytes)[0];
    htLAYER:
      FElement.FLayer := ExtractInt2(ABytes)[0];
    htWIDTH:
    begin
      (FElement as TGdsPath).Width := ExtractInt4(ABytes)[0] * FLibrary.FUserUnit;
    end;
    htSTRANS:
      (FElement as TGdsSref).FStrans := ExtractBitmask(ABytes);
    htMAG:
    begin
      (FElement as TGdsSref).FMag := ExtractReal8(ABytes)[0];
    end;
    htANGLE:
      (FElement as TGdsSref).FAngleDeg := ExtractReal8(ABytes)[0];
    htCOLROW:
    begin
      int2array := ExtractInt2(ABytes);
      (FElement as TGdsAref).FCols := int2array[0];
      (FElement as TGdsAref).FRows := int2array[1];
    end;
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


function TGdsInform.ExtractBitmask(const ABytes: TBytes): uint16;
begin
  Result := GDSreadBtimask(@ABytes[2]);
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


constructor TGdsObject.Create;
begin
  inherited Create;
  FParent := nil;
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

constructor TGdsElement.Create;
begin
  inherited Create;
  FExtentBoundsPtr := nil;
  FDataType := -1;
  FLayer := -1;
end;


destructor TGdsElement.Destroy;
begin
  DebugLn('TGdsElement.Destroy');
  FCoords := nil;
  inherited;
end;


function TGdsElement.ToString: string;
begin
  Result := ShortClassName.ToUpper;
end;


function TGdsElement.LookupExtentBounds: TRectangleF;
begin
  Result := CalcBounds(Coords);
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


function TGdsElement.IsReference: boolean;
begin
  Result := False;
end;


procedure TGdsElement.DebugStringsOn(AStrings: TStringList);
var
  i: integer;
begin
  for i := Low(Coords) to High(Coords) do
    AStrings.Add(Format('CE(%2d): %6.4f, %6.4f', [i, Coords[i][0], Coords[i][1]]));
  if FLayer >= 0 then
    AStrings.Add(Format('LAYER: %d', [FLayer]));
  if FDataType >= 0 then
    AStrings.Add(Format('DATATYPE: %d', [FDataType]));
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


{ TGdsPath }
constructor TGdsPath.Create;
begin
  inherited Create;
  FWidth := Math.NaN;
  FPathType := -1;
end;


destructor TGdsPath.Destroy;
begin
  FOutlineCoords := nil;
  inherited Destroy;
end;


procedure TGdsPath.DebugStringsOn(AStrings: TStringList);
begin
  inherited DebugStringsOn(AStrings);
  with AStrings do
  begin
    if not IsNan(FWidth) then
      Add(Format('WIDTH: %6.4f', [FWidth]));
    if FPathType >= 0 then;
    Add(Format('PATHTYPE: %d', [FPathType]));
  end;
end;


function TGdsPath.OutlineCoords: TCoords;
begin
  if FOutlineCoords = nil then
    FOutlineCoords := pathOutlineCoords(Coords, PathType, Width);
  Result := FOutlineCoords;
end;


function TGdsText.LookupExtentBounds: TRectangleF;
begin
  Result := CalcBounds(Coords);
end;


{ TGdsText }
function TGdsText.ToString: string;
begin
  Result := ShortClassName.ToUpper + '(' + Contents + ')';
end;


{ TGdsSref }
constructor TGdsSref.Create;
begin
  inherited Create;
  FAngleDeg := 0.0;
  FMag := 1.0;
end;


function TGdsSref.ToString: string;
begin
  Result := ShortClassName.ToUpper + '(' + RefName + ')';
end;


function TGdsSref.IsReference: boolean;
begin
  Result := True;
end;


function TGdsSref.GetRefStructure: TGdsStructure;
begin
  if FRefStructure = nil then
  begin
    FRefStructure := (GetRoot as TGdsLibrary).StructureNamed(RefName);
  end;
  Result := FRefStructure;
end;


function TGdsSref.GetTransform: TAffineMatrix; inline;
begin
  if not Assigned(FTransformPtr) then
  begin
    FTransform := LookupTransform;
    FTransformPtr := @FTransform;
  end;
  Result := FTransform;
end;


function TGdsSref.IsRefrected: boolean; inline;
begin
  Result := (FStrans and $8000) <> 0;
end;


function TGdsSref.IsAbsAngle: boolean; inline;
begin
  Result := (FStrans and $0001) <> 0;
end;


function TGdsSref.IsAbsMag: boolean; inline;
begin
  Result := (FStrans and $0002) <> 0;
end;


procedure TGdsSref.DebugStringsOn(AStrings: TStringList);
begin
  inherited DebugStringsOn(AStrings);
  with AStrings do
  begin
    if FRefName <> '' then
      Add(Format('SNAME: "%s"', [FRefName]));
    if FAngleDeg <> 0.0 then
      Add(Format('ANGLE: %6.4f', [FAngleDeg]));
    if FMag <> 1.0 then
      Add(Format('MAG: %6.4f', [FMag]));
    if IsAbsAngle then
      Add('ABSANGLE: YES');
    if IsAbsMag then
      Add('ABSMAG: YES');
    if IsRefrected then
      Add('REFLECTED: YES');
  end;
end;


function TGdsSref.LookupExtentBounds: TRectangleF;
var
  refBounds: TRectangleF;
  tx: TAffineMatrix;
  pt: TPointF;
  pointList: TXYPointList;
begin
  refBounds := RefStructure.GetExtentBounds;
  tx := GetTransform;
  pointList := TXYPointList.Create;
  for pt in refBounds.CornerPoints do
  begin
    pointList.Add(tx * pt);
  end;
  Result := CalcExtent(pointList);
  pointList.Free;
end;


function TGdsSref.LookupTransform: TAffineMatrix;
var
  tx: TAffineMatrix;
  rad: single;
begin
  rad := Math.DegToRad(FAngleDeg);
  tx := AffineMatrixTranslation(Coords[0][0], Coords[0][1]);
  tx *= AffineMatrixScale(FMag, FMag);
  tx *= AffineMatrix(cos(rad), -sin(rad), 0, sin(rad), cos(rad), 0);
  //  tx *= AffineMatrixRotationRad(Math.DegToRad(FAngleDeg));
  if IsRefrected then
  begin
    tx[1, 2] := -tx[1, 2];
    tx[2, 2] := -tx[2, 2];
  end;
  Result := tx;
end;

{ TGdsAref }

constructor TGdsAref.Create;
begin
  inherited Create;
  FRows := -1;
  FCols := -1;
  FColStep := Math.NaN;
  FRowStep := Math.NaN;
end;


destructor TGdsAref.Destroy;
begin
  FRepeatedTransforms := nil;
  inherited Destroy;
end;


procedure TGdsAref.DebugStringsOn(AStrings: TStringList);
begin
  inherited DebugStringsOn(AStrings);
  with AStrings do
  begin
    if FRows >= 0 then
      Add(Format('ROWS: %d', [FRows]));
    if FCols >= 0 then
      Add(Format('COLS: %d', [FCols]));
    if not Math.IsNaN(FRowStep) then
      Add(Format('ROWSTEP: %6.4f', [FRowStep]));
    if not Math.IsNaN(FColStep) then
      Add(Format('COLSTEP: %6.4f', [FColStep]));
  end;
end;


function TGdsAref.RepeatedTransforms: TAffineMatrixArray;
begin
  if FRepeatedTransforms = nil then
    FRepeatedTransforms := LookupRepeatedTransforms;
  Result := FRepeatedTransforms;
end;


function TGdsAref.LookupRepeatedTransforms: TAffineMatrixArray;
var
  otx: TAffineMatrix;
  ix, iy, n: integer;
begin
  Result := [];
  SetLength(Result, FCols * FRows);
  n := 0;
  for ix := 0 to FCols - 1 do
  begin
    for iy := 0 to FRows - 1 do
    begin
      otx := AffineMatrixTranslation(ix * FColStep, iy * FRowStep);
      Result[n] := GetTransform * otx;
      Inc(n);
    end;
  end;
end;


// https://gitlab.com/freepascal.org/fpc/source/-/issues/39861
function BugFixedArcTan2(y, x: float): float;
begin
  if (x = 0) then
  begin
    if y = 0 then
      Result := 0.0
    else if y > 0 then
      Result := pi / 2
    else
      Result := -pi / 2;
  end
  else
  begin
    if X > 0 then
      Result := ArcTan(y / x)
    else
    if Y < 0.0 then
      Result := ArcTan(y / x) - pi
    else
      Result := ArcTan(y / x) + pi;
  end;
end;

{$ifndef WINDOWS}
function BugFixedArcTan3(y, x: float): float; assembler;
asm
         FLDT    y
         FLDT    x
         FPATAN
         FWAIT
end;
{$ENDIF}

function getAngle(x1: double; y1: double; x2: double; y2: double): double;
var
  angle: double;
  direction: double;
begin
  if x1 = x2 then
  begin
    if y2 > y1 then
      direction := 1
    else
      direction := -1;
    angle := Pi_Half * direction;
  end
  else
  begin
    {$ifdef WINDOWS}
    angle := BugFixedArcTan3(abs(y2 - y1), abs(x2 - x1));
    {$ELSE}
    angle := Math.ArcTan2(abs(y2 - y1), abs(x2 - x1));
    {$ENDIF}

    if y2 >= y1 then
      if x2 >= x1 then
        angle := angle + 0.0
      else
        angle := Pi - angle
    else
    if x2 >= x1 then
      angle := Pi_Double - angle
    else
      angle := angle + Pi;
  end;
  Result := angle;
end;


function getDeltaXY(hw: double; p1: TXY; p2: TXY; p3: TXY): TXY;
const
  eps = 1e-8;
var
  alpha, beta, theta, r: double;
begin
  alpha := getAngle(p1[0], p1[1], p2[0], p2[1]);
  beta := getAngle(p2[0], p2[1], p3[0], p3[1]);
  theta := (alpha + beta + Pi) / 2.0;
  if Abs(Cos((alpha - beta) / 2.0)) < eps then
  begin
    raise Exception.Create('Internal algorithm error: cos((alpha - beta)/2) = 0');
    Result[0] := 0.0;
    Result[1] := 0.0;
    Exit(Result);
  end;
  r := hw / Cos((alpha - beta) / 2.0);
  Result[0] := r * Cos(theta);
  Result[1] := r * Sin(theta);
end;


function getEndDeltaXY(hw: double; p1: TXY; p2: TXY): TXY;
var
  alpha, theta, r: double;
begin
  alpha := getAngle(p1[0], p1[1], p2[0], p2[1]);
  theta := alpha;
  r := hw;
  Result[0] := -r * Sin(theta);
  Result[1] := r * Cos(theta);
end;


function pathOutlineCoords(ACoords: TCoords; APathType: integer;
  AWidth: double): TCoords;
var
  hw: double;
  i, numPoints: integer;
  points: TCoords;
  deltaxy: TXY;
begin
  points := [];
  hw := AWidth / 2.0;
  numPoints := Length(ACoords);
  if numPoints < 2 then
  begin
    DebugLn('pathOutlineCoords: don''t know to handle wires < 2 pts yet');
    Exit(nil);
  end;
  SetLength(points, (2 * numPoints + 1));
  deltaxy := getEndDeltaXY(hw, ACoords[0], ACoords[1]);
  if APathType = BUTT_END then
  begin
    points[0][0] := ACoords[0][0] + deltaxy[0];
    points[0][1] := ACoords[0][1] + deltaxy[1];
    points[2 * numPoints][0] := ACoords[0][0] + deltaxy[0];
    points[2 * numPoints][1] := ACoords[0][1] + deltaxy[1];
    points[2 * numPoints - 1][0] := ACoords[0][0] - deltaxy[0];
    points[2 * numPoints - 1][1] := ACoords[0][1] - deltaxy[1];
  end
  else
  begin
    points[0][0] := ACoords[0][0] + deltaxy[0] - deltaxy[1];
    points[0][1] := ACoords[0][1] + deltaxy[1] - deltaxy[0];
    points[2 * numPoints][0] := ACoords[0][0] + deltaxy[0] - deltaxy[1];
    points[2 * numPoints][1] := ACoords[0][1] + deltaxy[1] - deltaxy[0];
    points[2 * numPoints - 1][0] := ACoords[0][0] - deltaxy[0] - deltaxy[1];
    points[2 * numPoints - 1][1] := ACoords[0][1] - deltaxy[1] - deltaxy[0];
  end;

  for i := 1 to numPoints do
  begin
    deltaxy := getDeltaXY(hw, ACoords[i - 1], ACoords[i], ACoords[i + 1]);
    points[i][0] := ACoords[i][0] + deltaxy[0];
    points[i][1] := ACoords[i][1] + deltaxy[1];
    points[2 * numPoints - i - 1][0] := ACoords[i][0] - deltaxy[0];
    points[2 * numPoints - i - 1][1] := ACoords[i][1] - deltaxy[1];
  end;

  deltaxy := getEndDeltaXY(hw, ACoords[numPoints - 2], ACoords[numPoints - 1]);
  if APathType = BUTT_END then
  begin
    points[numPoints - 1][0] := ACoords[numPoints - 1][0] + deltaxy[0];
    points[numPoints - 1][1] := ACoords[numPoints - 1][1] + deltaxy[1];
    points[numPoints][0] := ACoords[numPoints - 1][0] - deltaxy[0];
    points[numPoints][1] := ACoords[numPoints - 1][1] - deltaxy[1];
  end
  else
  begin
    points[numPoints - 1][0] := ACoords[numPoints - 1][0] + deltaxy[0] + deltaxy[1];
    points[numPoints - 1][1] := ACoords[numPoints - 1][1] + deltaxy[1] + deltaxy[0];
    points[numPoints][0] := ACoords[numPoints - 1][0] - deltaxy[0] + deltaxy[1];
    points[numPoints][1] := ACoords[numPoints - 1][1] - deltaxy[1] + deltaxy[0];
  end;

  Result := points;
end;


end.
