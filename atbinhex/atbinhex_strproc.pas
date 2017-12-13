unit atbinhex_strproc;

interface

uses
  SysUtils, atbinhex_stringproc;

function SetStringW(Buf: Pointer; Len: Integer; SwapUnicode: boolean): UnicodeString;
function SFileExtensionMatch(const FN, ExtList: string): boolean;

type
  TStringDecodeRecW = record
    SFrom, STo: UnicodeString;
  end;

  TStringTabOptions = record
    TabSize: Integer;
    TabPosition: Integer;
    FontMonospaced: Boolean;
    NonPrintableShow: Boolean;
    NonPrintableChar: WideChar;
  end;

function SDecodeW(const S: UnicodeString; const Decode: array of TStringDecodeRecW): UnicodeString;
procedure SReplaceZeros(var S: AnsiString);
procedure SReplaceZerosW(var S: UnicodeString);
procedure SDelLastSpace(var S: AnsiString);
procedure SDelLastSpaceW(var S: UnicodeString);
function STabReplacement(const TabOptions: TStringTabOptions): UnicodeString;
procedure SReplaceTabsW(var S: UnicodeString; var TabOptions: TStringTabOptions);
function SCharCR(ch: WideChar): Boolean;
function SLastCharCR(const S: UnicodeString): Boolean;

function SFindText(const F, S: string; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;
function SFindTextW(const F, S: UnicodeString; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;

procedure ILimitMin(var N: Integer; Value: Integer);
procedure ILimitMax(var N: Integer; Value: Integer);
procedure I64LimitMin(var N: Int64; const Value: Int64);
procedure I64LimitMax(var N: Int64; const Value: Int64);

implementation

function SDecodeW(const S: UnicodeString; const Decode: array of TStringDecodeRecW): UnicodeString;
var
  i, j: Integer;
  DoDecode: Boolean;
begin
  Result := '';
  i := 1;
  repeat
    if i > Length(S) then Break;
    DoDecode := False;
    for j := Low(Decode) to High(Decode) do
      with Decode[j] do
        if SFrom = Copy(S, i, Length(SFrom)) then
        begin
          DoDecode := True;
          Result := Result + STo;
          Inc(i, Length(SFrom));
          Break
        end;
    if DoDecode then Continue;
    Result := Result + S[i];
    Inc(i);
  until False;
end;


function SDefaultDelimiters: string;
const
  cChars = ':;<=>?' + '@[\]^' + '`{|}~';
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Ord('/') do
    Result := Result + Chr(i);
  Result := Result + cChars;
end;

//--------------------------------------------------
function SFindText(const F, S: string; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;
var
  SBuf, FBuf, Delimiters: string;
  Match: Boolean;
  LastPos, LengthF, i: Integer;
begin
  Result := 0;

  if (S = '') or (F = '') then Exit;

  Delimiters := SDefaultDelimiters;

  SBuf := S;
  FBuf := F;
  if not fCaseSens then
  begin
    SBuf := LowerCase(SBuf);
    FBuf := LowerCase(FBuf);
  end;

  LengthF := Length(F);
  LastPos := Length(S) - LengthF + 1;

  if fForward then
    //Search forward
    for i := 1 to LastPos do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LengthF);

      if fWholeWords then
        Match := Match
          and (fLastBlock or (i < LastPos))
          and ((i <= 1) or (Pos(S[i - 1], Delimiters) > 0))
          and ((i >= LastPos) or (Pos(S[i + LengthF], Delimiters) > 0));

      if Match then
      begin
        Result := i;
        Break
      end;
    end
    else
    //Search backward
    for i := LastPos downto 1 do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LengthF);

      if fWholeWords then
        Match := Match
          and (fLastBlock or (i > 1))
          and ((i <= 1) or (Pos(S[i - 1], Delimiters) > 0))
          and ((i >= LastPos) or (Pos(S[i + LengthF], Delimiters) > 0));

      if Match then
      begin
        Result := i;
        Break
      end;
    end;
end;

//--------------------------------------------------
function SFindTextW(const F, S: UnicodeString; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;
var
  SBuf, FBuf, Delimiters: UnicodeString;
  Match: Boolean;
  LastPos, LengthF, i: Integer;
begin
  Result := 0;

  if (S = '') or (F = '') then Exit;

  Delimiters := SDefaultDelimiters;

  SBuf := S;
  FBuf := F;
  if not fCaseSens then
  begin
    SBuf := UnicodeLowerCase(SBuf);
    FBuf := UnicodeLowerCase(FBuf);
  end;

  LengthF := Length(F);
  LastPos := Length(S) - LengthF + 1;

  if fForward then
    //Search forward
    for i := 1 to LastPos do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LengthF * 2);

      if fWholeWords then
        Match := Match
          and (fLastBlock or (i < LastPos))
          and ((i <= 1) or (Pos(S[i - 1], Delimiters) > 0))
          and ((i >= LastPos) or (Pos(S[i + LengthF], Delimiters) > 0));

      if Match then
      begin
        Result := i;
        Break
      end;
    end
    else
    //Search backward
    for i := LastPos downto 1 do
    begin
      Match := CompareMem(@FBuf[1], @SBuf[i], LengthF * 2);

      if fWholeWords then
        Match := Match
          and (fLastBlock or (i > 1))
          and ((i <= 1) or (Pos(S[i - 1], Delimiters) > 0))
          and ((i >= LastPos) or (Pos(S[i + LengthF], Delimiters) > 0));

      if Match then
      begin
        Result := i;
        Break
      end;
    end;
end;

procedure ILimitMin(var N: Integer; Value: Integer);
begin
  if N < Value then
    N := Value;
end;

procedure ILimitMax(var N: Integer; Value: Integer);
begin
  if N > Value then
    N := Value;
end;

procedure I64LimitMin(var N: Int64; const Value: Int64);
begin
  if N < Value then
    N := Value;
end;

procedure I64LimitMax(var N: Int64; const Value: Int64);
begin
  if N > Value then
    N := Value;
end;


procedure SReplaceZeros(var S: AnsiString);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    if S[i] = #0 then
      S[i] := ' ';
end;

procedure SReplaceZerosW(var S: UnicodeString);
var
  i: Integer;
begin
  for i := 1 to Length(S) do
    if S[i] = #0 then
      S[i] := ' ';
end;

procedure SDelLastSpaceW(var S: UnicodeString);
begin
  if (S <> '') and ((S[Length(S)] = ' ') or (S[Length(S)] = #9)) then
    SetLength(S, Length(S) - 1);
end;

procedure SDelLastSpace(var S: AnsiString);
begin
  if (S <> '') and (S[Length(S)] = ' ') then
    SetLength(S, Length(S) - 1);
end;


function STabReplacement(const TabOptions: TStringTabOptions): UnicodeString;
var
  ASize: Integer;
  APos: Integer;
begin
  with TabOptions do
  begin
    Assert(TabSize > 0, 'Tab size too small');
    if FontMonospaced then
      ASize := TabSize - (TabPosition - 1) mod TabSize
    else
      ASize := TabSize;
    Result := StringOfChar(' ', ASize);
    APos := Length(Result) div 2 + 1;
    if NonPrintableShow then
      Result[APos] := NonPrintableChar;
  end;
end;

procedure SReplaceTabsW(var S: UnicodeString; var TabOptions: TStringTabOptions);
var
  N: Integer;
begin
  repeat
    N := Pos(#9, S);
    if N = 0 then Break;
    TabOptions.TabPosition := N;
    S := StringReplace(S, #9, STabReplacement(TabOptions), [rfReplaceAll]);
  until False;
end;


procedure SDeleteFromStrA(var S: AnsiString; const SubStr: AnsiString);
var
  N: Integer;
begin
  N := Pos(SubStr, S);
  if N > 0 then
    SetLength(S, N - 1);
end;

procedure SDeleteFromStrW(var S: UnicodeString; const SubStr: UnicodeString);
var
  N: Integer;
begin
  N := Pos(SubStr, S);
  if N > 0 then
    SetLength(S, N - 1);
end;

function SCharCR(ch: WideChar): Boolean;
begin
  Result := (ch = #13) or (ch = #10);
end;

function SLastCharCR(const S: UnicodeString): Boolean;
begin
  Result := (S <> '') and SCharCR(S[Length(S)]);
end;

function SetStringW(Buf: Pointer; Len: Integer; SwapUnicode: boolean): UnicodeString;
begin
  Result:= '';
  if Len<2 then Exit;

  SetLength(Result, Len div 2);
  Move(Buf^, Result[1], Len);
  if SwapUnicode then
    Result:= SSwapEndian(Result);
end;

function SFileExtensionMatch(const FN, ExtList: string): boolean;
begin
  Result:= false;
end;

end.
