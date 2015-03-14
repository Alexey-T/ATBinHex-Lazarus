unit ATxSProc;

interface

uses
  SysUtils, ATStringProc;

procedure SReplace(var S: string; const SFrom, STo: string);
procedure SReplaceW(var S: UnicodeString; const SFrom, STo: UnicodeString);
procedure SReplaceAll(var S: string; const SFrom, STo: string);
procedure SReplaceAllW(var S: UnicodeString; const SFrom, STo: UnicodeString);
procedure SReplaceIAll(var S: string; const SFrom, STo: string);

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
procedure SDelLastSlash(var S: AnsiString);
procedure SDelLastSlashW(var S: UnicodeString);
procedure SDelLastComma(var S: AnsiString);
function STabReplacement(const TabOptions: TStringTabOptions): UnicodeString;
procedure SReplaceTabsW(var S: UnicodeString; var TabOptions: TStringTabOptions);
function SCharCR(ch: WideChar): Boolean;
function SLastCharCR(const S: UnicodeString): Boolean;

procedure SDeleteFromStrA(var S: AnsiString; const SubStr: AnsiString);
procedure SDeleteFromStrW(var S: UnicodeString; const SubStr: UnicodeString);

function SFindText(const F, S: AnsiString; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;
function SFindTextW(const F, S: UnicodeString; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;

function IMin(N1, N2: Integer): Integer;
function IMax(N1, N2: Integer): Integer;
function WMin(N1, N2: Word): Word;
function WMax(N1, N2: Word): Word;
function I64Min(const N1, N2: Int64): Int64;
function I64Max(const N1, N2: Int64): Int64;

procedure ILimitMin(var N: Integer; Value: Integer);
procedure ILimitMax(var N: Integer; Value: Integer);
procedure I64LimitMin(var N: Int64; const Value: Int64);
procedure I64LimitMax(var N: Int64; const Value: Int64);

implementation

procedure SReplace(var S: string; const SFrom, STo: string);
var
  i: Integer;
begin
  i := Pos(SFrom, S);
  if i > 0 then
  begin
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  end;
end;

function SReplaceFunc(const S: string; const SFrom, STo: string): string;
begin
  Result := S;
  SReplace(Result, SFrom, STo);
end;

procedure SReplaceW(var S: UnicodeString; const SFrom, STo: UnicodeString);
var
  i: Integer;
begin
  i := Pos(SFrom, S);
  if i > 0 then
  begin
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  end;
end;

procedure SReplaceAll(var S: string; const SFrom, STo: string);
var
  i: Integer;
begin
  repeat
    i := Pos(SFrom, S);
    if i = 0 then Break;
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  until False;
end;

procedure SReplaceAllW(var S: UnicodeString; const SFrom, STo: UnicodeString);
var
  i: Integer;
begin
  repeat
    i := Pos(SFrom, S); 
    if i = 0 then Break;
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  until False;
end;

procedure SReplaceIAll(var S: string; const SFrom, STo: string);
var
  i: Integer;
begin
  repeat
    i := Pos(LowerCase(SFrom), LowerCase(S));
    if i = 0 then Break;
    Delete(S, i, Length(SFrom));
    Insert(STo, S, i);
  until False;
end;

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



function SDefaultDelimiters: AnsiString;
const
  Chars: PAnsiChar = ':;<=>?' + '@[\]^' + '`{|}~';
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Ord('/') do
    Result := Result + AnsiChar(Chr(i));
  Result := Result + Chars;
end;
//--------------------------------------------------
function SFindText(const F, S: AnsiString; fForward, fWholeWords, fCaseSens, fLastBlock: Boolean): Integer;
var
  SBuf, FBuf, Delimiters: AnsiString;
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
    SBuf := AnsiLowerCase(SBuf);
    FBuf := AnsiLowerCase(FBuf);
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


function IMin(N1, N2: Integer): Integer;
begin
  if N1 < N2 then
    Result := N1
  else
    Result := N2;
end;

function IMax(N1, N2: Integer): Integer;
begin
  if N1 > N2 then
    Result := N1
  else
    Result := N2;
end;

function WMin(N1, N2: Word): Word;
begin
  if N1 < N2 then
    Result := N1
  else
    Result := N2;
end;

function WMax(N1, N2: Word): Word;
begin
  if N1 > N2 then
    Result := N1
  else
    Result := N2;
end;

function I64Min(const N1, N2: Int64): Int64;
begin
  if N1 < N2 then
    Result := N1
  else
    Result := N2;
end;

function I64Max(const N1, N2: Int64): Int64;
begin
  if N1 > N2 then
    Result := N1
  else
    Result := N2;
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

{
procedure WLimitMin(var N: Word; Value: Word);
begin
  if N < Value then
    N := Value;
end;

procedure WLimitMax(var N: Word; Value: Word);
begin
  if N > Value then
    N := Value;
end;
}

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

procedure SDelLastSlashW(var S: UnicodeString);
begin
  if (S <> '') and (S[Length(S)] = '\') then
    SetLength(S, Length(S) - 1);
end;

procedure SDelLastSlash(var S: AnsiString);
begin
  if (S <> '') and (S[Length(S)] = '\') then
    SetLength(S, Length(S) - 1);
end;

procedure SDelLastComma(var S: AnsiString);
begin
  if (S <> '') and (S[Length(S)] = ',') then
    SetLength(S, Length(S) - 1);
end;


function SFillW(ch: WideChar; Count: Integer): UnicodeString;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 1 to Length(Result) do
    Result[i] := ch;
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
    Result := SFillW(' ', ASize);
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
    SReplaceW(S, #9, STabReplacement(TabOptions));
  until False;
end;


function SetStringW(Buffer: PAnsiChar; BufSize: Integer; SwapBytes: Boolean): UnicodeString;
var
  P: PAnsiChar;
  i, j: Integer;
  ch: AnsiChar;
begin
  Result := '';
  if BufSize < 2 then Exit;

  SetLength(Result, BufSize div 2);
  Move(Buffer^, Result[1], Length(Result) * 2);

  if SwapBytes then
  begin
    P := @Result[1];
    for i := 1 to Length(Result) do
    begin
      j := (i - 1) * 2;
      ch := P[j];
      P[j] := P[j + 1];
      P[j + 1] := ch;
    end;
  end;
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
