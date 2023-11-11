unit UStrExt;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Strings;

type
  TStrExt = class helper for string
    function BeginsWith(const S: string): boolean;
  end;

implementation

function TStrExt.BeginsWith(const S: string): boolean;
begin
  Result := false;
end;

end.

