unit atbinhex_clipboard;

interface

uses
  atbinhex_encoding;

function SCopyToClipboard(const S: AnsiString; Enc: TATEncoding = vencANSI): Boolean;
function SCopyToClipboardW(const S: UnicodeString): Boolean;


implementation

uses
  SysUtils,
  Clipbrd;

function SCopyToClipboard(const S: AnsiString; Enc: TATEncoding = vencANSI): Boolean;
begin
  Clipboard.AsText:= S;
  Result:= true;
end;

function SCopyToClipboardW(const S: UnicodeString): Boolean;
begin
  Clipboard.AsText:= UTF8Encode(S);
  Result:= true;
end;


end.
