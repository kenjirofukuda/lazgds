unit UGds;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, fgl;

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
  TInt16s = array of Int16;
  TInt32s = array of Int32;
  TDoubles = array of Double;
  TXY = array [0 .. 1] of Double;
  TCoords = array of TXY;

  TDateTimeItems = record
    items: array [0 .. 5] of Word;
  end;

  { TGdsObject }

  TGdsObject = class(TObject)
  private
    FParent: TGdsObject;
  protected
    function ShortClassName: String;
  public
    function ToString: String; override;
    function GetRoot: TGdsObject;
    property Parent: TGdsObject read FParent;
  end;

  {  }

  TGdsElement = class(TGdsObject)
  private
    FXY: TInt32s;
  public
    class function CreateFromHeaderByte(AByte: Byte): TGdsElement;
    function ToString: String; override;
    property XY: TInt32s read FXY;
    function GetCoords: TCoords;
  end;

  TGdsElementClass = class of TGdsElement;

  TGdsBoundary = class(TGdsElement)
  end;

  TGdsPath = class(TGdsElement)
  end;

  TGdsText = class(TGdsElement)
  private
    FContents: String;
  public
    function ToString: String; override;
    property Contents: String read FContents;
  end;

  TGdsSref = class(TGdsElement)
  private
    FRefName: String;
  public
    function ToString: String; override;
    property RefName: String read FRefName;
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
    FName: String;
    FCreated: TDateTimeItems;
    FLastModified: TDateTimeItems;
    FElements: TGdsElements;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: String read FName;
    property Elements: TGdsElements read FElements;
  end;

  TGdsStructures = specialize TFPGObjectList<TGdsStructure>;

  TGdsLibrary = class(TGdsObject)
  private
    FName: String;
    FLastModified: TDateTimeItems;
    FLastAccessed: TDateTimeItems;
    FUserUnit: Double;
    FMeterUnit: Double;
    FStructures: TGdsStructures;
  public
    constructor Create;
    destructor Destroy; override;
    function StructureNames: TStringArray;
    property Name: String read FName;
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
    FFileName: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    property FileName: String read FFileName write FFileName;
    property OnBytes: TGdsInformBytesEvent read FOnBytes write FOnBytes;
    property GdsLibrary: TGdsLibrary read FLibrary;
    function ExtractAscii(const ABytes: TBytes): String;
  private
    procedure HandleRecord(const ABytes: TBytes);
    function ExtractInt2(const ABytes: TBytes): TInt16s;
    function ExtractInt4(const ABytes: TBytes): TInt32s;
    function ExtractReal8(const ABytes: TBytes): TDoubles;
  end;

function FileSizeEx(const AFileName: String): Longint;

implementation

uses
  UUtils, Math, LazLogger;

{ GDSreader }

// @note
// serbanp@ix.netcom.com's GDSreader.0.3.2
function GDSreadInt2(rec: PByte): Int16;
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

function GDSreadInt4(rec: PByte): Int32;
var
  i: Integer;
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

function GDSreadReal8(rec: PByte): Double;
var
  i, sign, exponent: Integer;
  mantissa_int: Uint64;
  mantissa_float: Double;
begin
  sign := rec[0] and $80;
  exponent := (rec[0] and $7F) - 64;
  mantissa_int := 0;
  for i := 1 to 7 do
  begin
    mantissa_int := mantissa_int shl 8;
    Inc(mantissa_int, rec[i]);
  end;
  mantissa_float := Double(mantissa_int) / Power(2, 56);
  Result := mantissa_float * Power(16, exponent);
  if sign <> 0 then
    Result := -Result;
end;

{ TGdsLibrary }

constructor TGdsLibrary.Create;
begin
  inherited;
  FStructures := TGdsStructures.Create;
end;

destructor TGdsLibrary.Destroy;
begin
  FreeAndNil(FStructures);
  inherited;
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
  recSize: Integer;
  bytesSize: Integer;
  buff: TBytes;
  numRead: Integer;
  recCount: Longint;
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
    AOffset: Integer);
  var
    i: Integer;
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
      FLibrary.FStructures.Add(FStructure);
      FStructure.FParent := FLibrary;
      FStructure := nil;
    end;
    htBOUNDARY, htPATH, htSREF, htAREF, htTEXT, htNODE, htBOX:
    begin
      FElement := TGdsElement.CreateFromHeaderByte(ABytes[0]);
      DebugLn('NewElement: ', String(FElement.ClassName).Remove(0, 4).ToUpper);
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
  i: Integer;
  bodySize: Integer;
  numItems: Integer;
begin
  Result := [];
  bodySize := Length(Abytes) - 2;
  numItems := bodySize div sizeof(Int16);
  SetLength(Result, numItems);
  for i := 0 to numItems - 1 do
    Result[i] := GDSreadInt2(@ABytes[i * 2 + 2]);
end;

function TGdsInform.ExtractInt4(const ABytes: TBytes): TInt32s;
var
  i: Integer;
  bodySize: Integer;
  numItems: Integer;
begin
  Result := [];
  bodySize := Length(Abytes) - 2;
  numItems := bodySize div sizeof(Int32);
  SetLength(Result, numItems);
  for i := 0 to numItems - 1 do
    Result[i] := GDSreadInt4(@ABytes[i * 4 + 2]);
end;


function TGdsInform.ExtractReal8(const ABytes: TBytes): TDoubles;
var
  i: Integer;
  bodySize: Integer;
  numItems: Integer;
begin
  Result := [];
  bodySize := Length(Abytes) - 2;
  numItems := bodySize div sizeof(Double);
  SetLength(Result, numItems);
  for i := 0 to numItems - 1 do
    Result[i] := GDSreadReal8(@ABytes[i * 8 + 2]);
end;


function TGdsInform.ExtractAscii(const ABytes: TBytes): String;
var
  buff: array [0 .. 511] of Byte;
begin
  Result := '';
  if ABytes[1] <> dtASCII then
    Exit;
  DebugLn('Buff: ', FormatByteArray(ABytes));
  FillByte(buff[0], sizeof(buff), 0);
  Move(ABytes[2], buff[0], Length(ABytes) - 2);
  Result := Format('%s', [PChar(@buff[0])]);
end;

function FileSizeEx(const AFileName: String): Longint;
var
  F: file of Byte;
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

function TGdsObject.ShortClassName: String;
begin
  Result := ClassName;
  if Result.StartsWith('TGds') then
    Result := Result.Remove(0, 4);
end;

function TGdsObject.ToString: String;
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

function TGdsElement.ToString: String;
begin
  Result := ShortClassName.ToUpper;
end;

function TGdsElement.GetCoords: TCoords;
var
  lib: TGdsLibrary;
  AXY: TXY;
  i: Integer;
begin
  lib := GetRoot as TGdsLibrary;
  SetLength(Result, Length(FXY) div 2);
  for i := 0 to High(Result) do
  begin
    AXY[0] := FXY[i * 2 + 0] * lib.FUserUnit;
    AXY[1] := FXY[i * 2 + 1] * lib.FUserUnit;
    Result[i] := AXY;
  end;
end;


class function TGdsElement.CreateFromHeaderByte(AByte: Byte): TGdsElement;
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

function TGdsText.ToString: String;
begin
  Result := inherited + '(' + Contents + ')';
end;

{ TGdsSref }

function TGdsSref.ToString: String;
begin
  Result := inherited + '(' + RefName + ')';
end;


end.
