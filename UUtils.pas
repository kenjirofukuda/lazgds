unit UUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function FormatByteArray(const Bytes: TBytes): string;

implementation

function FormatByteArray(const Bytes: TBytes): string;
var
  strings: TStringList;
  i: integer;
begin
  strings := TStringList.Create;
  strings.Add('[');
  for i := Low(Bytes) to High(Bytes) do
  begin
    strings.Add(IntToStr(Bytes[i]));
    if i = High(Bytes) then
      break;
    strings.Add(',');
  end;
  strings.Add(']');
  Result := string.Join('', strings.ToStringArray);
  FreeAndNil(strings);
end;

end.

