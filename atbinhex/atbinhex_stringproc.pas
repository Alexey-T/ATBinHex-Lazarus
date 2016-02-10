unit atbinhex_stringproc;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  atString = UnicodeString;
  atChar = WideChar;

const
  cMaxTabPositionToExpand = 1024;
  cCharScaleFullwidth = 1.7; //width of CJK chars

function IsWordChar(ch: atChar): boolean;
function IsEolCode(N: Word): boolean;
function IsAccentChar(ch: WideChar): boolean;
function BoolToPlusMinusOne(b: boolean): integer;

function SSwapEndian(const S: UnicodeString): UnicodeString;
function SGetIndentSize(const S: atString; ATabSize: integer): integer;

procedure SCalcCharOffsets(const AStr: atString; var AList: array of real; ATabSize: integer);
function SExpandTabulations(const S: atString; ATabSize: integer): atString;
function SFindWordWrapPosition(const S: atString; AColumns, ATabSize: integer): integer;
function SFindClickedPosition(const Str: atString;
  APixelsFromLeft, ACharSize, ATabSize: integer;
  AAllowVirtualPos: boolean): integer;


implementation

uses
  Dialogs, Math;

function IsEolCode(N: Word): boolean;
begin
  Result:= (N=10) or (N=13);
end;

function IsWordChar(ch: atChar): boolean;
begin
  Result:=
    ((ch>='0') and (ch<='9')) or
    ((ch>='a') and (ch<='z')) or
    ((ch>='A') and (ch<='Z')) or
    (ch='_');
end;

function IsSpaceChar(ch: atChar): boolean;
begin
  Result:= (ch=' ') or (ch=#9);
end;

procedure DoDebugOffsets(const List: array of real);
var
  i: integer;
  s: string;
begin
  s:= '';
  for i:= Low(List) to High(List) do
    s:= s+FloatToStr(List[i])+' ';
  showmessage('Offsets'#13+s);
end;

function SFindWordWrapPosition(const S: atString; AColumns, ATabSize: integer): integer;
var
  N, NAvg: integer;
  List: array of real;
begin
  if S='' then
    begin Result:= 0; Exit end;

  SetLength(List, Length(S));
  SCalcCharOffsets(S, List, ATabSize);

  if List[High(List)]<=AColumns then
  begin
    Result:= Length(S);
    Exit
  end;

  N:= Length(S)-1;
  while (N>1) and (List[N]>AColumns+1) do Dec(N);
  NAvg:= N;
  while (N>1) and IsWordChar(S[N]) and IsWordChar(S[N+1]) do Dec(N);

  if N>1 then
    Result:= N
  else
  if NAvg>1 then
    Result:= NAvg
  else
    Result:= Length(S);
end;

function SGetIndentSize(const S: atString; ATabSize: integer): integer;
var
  SIndent: atString;
begin
  Result:= 0;
  while (Result<Length(S)) and IsSpaceChar(S[Result+1]) do
    Inc(Result);
  SIndent:= Copy(S, 1, Result);
  Result:= Length(SExpandTabulations(SIndent, ATabSize));
end;

function SSwapEndian(const S: UnicodeString): UnicodeString;
var
  i: integer;
begin
  Result:= S;
  for i:= 1 to Length(Result) do
    Result[i]:= WideChar(SwapEndian(Ord(Result[i])));
end;

function SCalcTabulationSize(const ATabSize, APos: integer): integer;
begin
  if APos>cMaxTabPositionToExpand then
    Result:= 1
  else
  begin
    Result:= 0;
    repeat Inc(Result) until ((APos+Result-1) mod ATabSize)=0;
  end;
end;

function SExpandTabulations(const S: atString; ATabSize: integer): atString;
var
  N, NSize: integer;
begin
  Result:= S;
  repeat
    N:= Pos(#9, Result);
    if N=0 then Break;
    NSize:= SCalcTabulationSize(ATabSize, N);
    if NSize<=1 then
      Result[N]:= ' '
    else
    begin
      Delete(Result, N, 1);
      Insert(StringOfChar(' ', NSize), Result, N);
    end;
  until false;
end;

{
  http://en.wikipedia.org/wiki/Combining_character
  Combining Diacritical Marks (0300–036F), since version 1.0, with modifications in subsequent versions down to 4.1
  Combining Diacritical Marks Extended (1AB0–1AFF), version 7.0
  Combining Diacritical Marks Supplement (1DC0–1DFF), versions 4.1 to 5.2
  Combining Diacritical Marks for Symbols (20D0–20FF), since version 1.0, with modifications in subsequent versions down to 5.1
  Combining Half Marks (FE20–FE2F), versions 1.0, updates in 5.2
}
function IsAccentChar(ch: WideChar): boolean;
begin
  case Ord(ch) of
    $0300..$036F,
    $1AB0..$1AFF,
    $1DC0..$1DFF,
    $20D0..$20FF,
    $FE20..$FE2F:
      Result:= true;
    else
      Result:= false;
  end;
end;

{
Ranges that are FullWidth char
 1100  e1 84 80  ..  115F  e1 85 9f
 2329  e2 8c a9  ..  232A  e2 8c aa
 2E80  e2 ba 80  ..  303E  e3 80 be
 3041  e3 81 81  ..  33FF  e3 8f bf
 3400  e3 90 80  ..  4DB5  e4 b6 b5
 4E00  e4 b8 80  ..  9FC3  e9 bf 83
 A000  ea 80 80  ..  A4C6  ea 93 86
 AC00  ea b0 80  ..  D7A3  ed 9e a3
 F900  ef a4 80  ..  FAD9  ef ab 99
 FE10  ef b8 90  ..  FE19  ef b8 99
 FE30  ef b8 b0  ..  FE6B  ef b9 ab
 FF01  ef bc 81  ..  FF60  ef bd a0
 FFE0  ef bf a0  ..  FFE6  ef bf a6
20000  f0 a0 80 80  .. 2FFFD f0 af bf bd
30000  f0 b0 80 80  .. 3FFFD f0 bf bf bd
}
function IsCharFullWidth(ch: WideChar): boolean;
begin
  case Ord(ch) of
    $1100..$115F,
    $2329..$232A,
    $2E80..$303E,
    $3041..$33FF,
    $3400..$4DB5,
    $4E00..$9FC3,
    $A000..$A4C6,
    $AC00..$D7A3,
    $F900..$FAD9,
    $FE10..$FE19,
    $FE30..$FE6B,
    $FF01..$FF60,
    $FFE0..$FFE6:
      Result:= true;
    else
      Result:= false;
  end;
end;


procedure SCalcCharOffsets(const AStr: atString; var AList: array of real; ATabSize: integer);
const
  cScaleTest = 1.9; //debug, for test code, commented
var
  S: atString;
  NOffset, NTabSize, NListIndex, i: integer;
  Scale: real;
begin
  if Length(AList)<>Length(AStr) then
    raise Exception.Create('bad list parameter in CalcCharOffsets');
  if AStr='' then Exit;

  S:= AStr;
  i:= 0;
  NListIndex:= 0;

  repeat
    Inc(i);
    if i>Length(S) then Break;

    if IsCharFullWidth(S[i]) then
      Scale:= cCharScaleFullwidth
    else
      Scale:= 1.0;

    ////debug
    {
    if IsSpaceChar(S[i]) then
      Scale:= 1
    else
      Scale:= cTestScale;
      }

    if S[i]<>#9 then
      NOffset:= 1
    else
    begin
      NTabSize:= SCalcTabulationSize(ATabSize, i);
      NOffset:= NTabSize;
      S[i]:= ' ';
      if NTabSize>1 then
        Insert(StringOfChar(' ', NTabSize-1), S, i);
      Inc(i, NTabSize-1);
    end;

    if (i<Length(S)) and IsAccentChar(S[i+1]) then
    begin
      NOffset:= 0;
    end;

    if NListIndex=0 then
      AList[NListIndex]:= NOffset*Scale
    else
      AList[NListIndex]:= AList[NListIndex-1]+NOffset*Scale;

    Inc(NListIndex);
  until false;
end;


function SFindClickedPosition(const Str: atString;
  APixelsFromLeft, ACharSize, ATabSize: integer;
  AAllowVirtualPos: boolean): integer;
var
  ListReal: array of real;
  ListEnds, ListMid: array of integer;
  i: integer;
begin
  if Str='' then
  begin
    if AAllowVirtualPos then
      Result:= 1+APixelsFromLeft div ACharSize
    else
      Result:= 1;
    Exit;
  end;

  SetLength(ListReal, Length(Str));
  SetLength(ListEnds, Length(Str));
  SetLength(ListMid, Length(Str));
  SCalcCharOffsets(Str, ListReal, ATabSize);

  //positions of each char end
  for i:= 0 to High(ListEnds) do
    ListEnds[i]:= Trunc(ListReal[i]*ACharSize);

  //positions of each char middle
  for i:= 0 to High(ListEnds) do
    if i=0 then
      ListMid[i]:= ListEnds[i] div 2
    else
      ListMid[i]:= (ListEnds[i-1]+ListEnds[i]) div 2;

  for i:= 0 to High(ListEnds) do
    if APixelsFromLeft<ListMid[i] then
    begin
      Result:= i+1;
      Exit
    end;

  if AAllowVirtualPos then
    Result:= Length(Str)+1 + (APixelsFromLeft - ListEnds[High(ListEnds)]) div ACharSize
  else
    Result:= Length(Str)+1;
end;


function BoolToPlusMinusOne(b: boolean): integer;
begin
  if b then Result:= 1 else Result:= -1;
end;

end.

