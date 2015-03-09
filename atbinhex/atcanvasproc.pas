unit ATCanvasProc;

{$mode delphi}
//{$define win_fast} //use Windows api

interface

uses
  Classes, SysUtils, Graphics, Types,
  ATStringProc;

procedure CanvasTextOut(C: TCanvas; PosX, PosY: integer; const S: UnicodeString; ATabSize: integer; ACharSize: TPoint);
function CanvasTextSpaces(const S: atString; ATabSize: integer): real;
function CanvasTextWidth(C: TCanvas; const S: atString; ATabSize: integer; ACharSize: TPoint): integer;

function CanvasFontSizes(C: TCanvas): TSize;
procedure CanvasInvertRect(C: TCanvas; const R: TRect);


implementation

uses
  {$ifdef win_fast}
  Windows,
  {$endif}
  LCLIntf;

function CanvasFontSizes(C: TCanvas): TSize;
begin
  Result:= C.TextExtent('M');
end;

function CanvasTextSpaces(const S: atString; ATabSize: integer): real;
var
  List: array of real;
begin
  Result:= 0;
  if S='' then Exit;
  SetLength(List, Length(S));
  SCalcCharOffsets(S, List, ATabSize);
  Result:= List[High(List)];
end;

function CanvasTextWidth(C: TCanvas; const S: atString; ATabSize: integer; ACharSize: TPoint): integer;
begin
  Result:= Trunc(CanvasTextSpaces(S, ATabSize)*ACharSize.X);
end;

procedure CanvasTextOut(C: TCanvas; PosX, PosY: integer;
  const S: UnicodeString; ATabSize: integer; ACharSize: TPoint);
var
  ListReal: array of real;
  ListInt: array of Longint;
  Dx: array of Longint;
  i: integer;
  Buf: string;
begin
  if S='' then Exit;
  SetLength(ListReal, Length(S));
  SetLength(ListInt, Length(S));
  SetLength(Dx, Length(S));

  SCalcCharOffsets(S, ListReal, ATabSize);

  for i:= 0 to High(ListReal) do
    ListInt[i]:= Trunc(ListReal[i]*ACharSize.X);

  for i:= 0 to High(ListReal) do
    if i=0 then
      Dx[i]:= ListInt[i]
    else
      Dx[i]:= ListInt[i]-ListInt[i-1];

  {$ifdef win_fast}
  Windows.ExtTextOutW(C.Handle, PosX, PosY, 0, nil, PWideChar(S), Length(S), @Dx[0]);
  {$else}
  Buf:= UTF8Encode(S);
  ExtTextOut(C.Handle, PosX, PosY, 0, nil, PChar(Buf), Length(Buf), @Dx[0]);
  {$endif}
end;

(*
var
  _bmp: Graphics.TBitmap = nil;
const
  cInvertMaxX = 40;
  cInvertMaxY = 80;

procedure CanvasInvertRect_Universal(C: TCanvas; const R: TRect);
var
  sizeX, sizeY: integer;
  i, j: integer;
  Rbmp: TRect;
begin
  if not Assigned(_bmp) then
  begin
    _bmp:= Graphics.TBitmap.Create;
    _bmp.PixelFormat:= pf24bit;
    _bmp.SetSize(cInvertMaxX, cInvertMaxY);
  end;

  sizeX:= R.Right-R.Left;
  sizeY:= R.Bottom-R.Top;
  Rbmp:= Classes.Rect(0, 0, sizeX, sizeY);

  _bmp.Canvas.CopyRect(Rbmp, C, R);

  for j:= 0 to sizeY-1 do
    for i:= 0 to sizeX-1 do
      with _bmp.Canvas do
        Pixels[i, j]:= Pixels[i, j] xor $FFFFFF;

  C.CopyRect(R, _bmp.Canvas, Rbmp);
end;
*)

{$ifdef win_fast}
procedure CanvasInvertRect(C: TCanvas; const R: TRect);
begin
  Windows.InvertRect(C.Handle, R);
end;
{$else}
procedure CanvasInvertRect(C: TCanvas; const R: TRect);
begin
  C.Brush.Style:= bsSolid;
  C.Pen.Mode:= pmNot;
  C.Rectangle(R);
  C.Pen.Mode:= pmCopy;
end;
{$endif}

end.
