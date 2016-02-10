unit atbinhex_encoding;

interface

type
  TATEncoding = type Integer;

const
  //indexes in cCodepages list
  vEncANSI = 0;
  vEncOEM = 1;
  vEncMac = 2;
  vEncUnicodeLE = -1;
  vEncUnicodeBE = -2;

function IsCodepageSupported(Enc: TATEncoding): Boolean;
function SCodepageToUnicode(const S: AnsiString; Enc: TATEncoding): UnicodeString;


implementation

uses
  SysUtils;

function IsCodepageSupported(Enc: TATEncoding): Boolean;
begin
  Result:= false;
end;

function SCodepageToUnicode(const S: AnsiString; Enc: TATEncoding): UnicodeString;
begin
  Result:= S;
end;

end.
