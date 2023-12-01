{***********************************}
{                                   }
{  ATBinHex Component               }
{  Copyright (C) Alexey Torgashin   }
{  http://uvviewsoft.com            }
{                                   }
{***********************************}
unit ATBinHex_CanvasProc;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, Types,
  atbinhex_stringproc;

procedure CanvasTextOut(C: TCanvas; PosX, PosY: integer; const S: UnicodeString);
function CanvasTextWidth(C: TCanvas; const S: atString): integer;
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);


implementation

uses
  {$ifdef windows}
  Windows,
  {$endif}
  LCLIntf;

function CanvasTextWidth(C: TCanvas; const S: atString): integer;
var
  List: array of integer;
begin
  Result:= 0;
  if S='' then Exit;
  SetLength(List, Length(S));
  SCalcCharOffsets(C, S, List);
  Result:= List[High(List)];
end;

function StringNeedsDxOffsets(const S: UnicodeString): boolean;
var
  i: integer;
begin
  for i:= 1 to Length(S) do
    if Ord(S[i])>$FF then
      exit(true);
  Result:= false;
end;

procedure CanvasTextOut(C: TCanvas; PosX, PosY: integer; const S: UnicodeString);
var
  {
  ListReal: array of integer;
  ListInt: array of Longint;
  Dx: array of Longint;
  }
  DxPtr: pointer;
  Buf: string;
  i: integer;
begin
  if S='' then Exit;

  DxPtr:= nil;

  {
  if not StringNeedsDxOffsets(S) then
    DxPtr:= nil
  else
  begin
    SetLength(ListReal, Length(S));
    SetLength(ListInt, Length(S));
    SetLength(Dx, Length(S));

    SCalcCharOffsets(S, ListReal, ATabSize);

    for i:= 0 to High(ListReal) do
      ListInt[i]:= ListReal[i]*ACharSize.X div 100;

    for i:= 0 to High(ListReal) do
      if i=0 then
        Dx[i]:= ListInt[i]
      else
        Dx[i]:= ListInt[i]-ListInt[i-1];

    DxPtr:= @Dx[0];
  end;
  }

  {$ifdef windows}
  Windows.ExtTextOutW(C.Handle, PosX, PosY, 0, nil, PWideChar(S), Length(S), DxPtr);
  {$else}
  Buf:= UTF8Encode(S);
  ExtTextOut(C.Handle, PosX, PosY, 0, nil, PChar(Buf), Length(Buf), DxPtr);
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

{$ifdef windows}
procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
begin
  Windows.InvertRect(C.Handle, R);
end;
{$else}

var
  _Pen: TPen = nil;

procedure CanvasInvertRect(C: TCanvas; const R: TRect; AColor: TColor);
var
  X: integer;
  AM: TAntialiasingMode;
begin
  if not Assigned(_Pen) then
    _Pen:= TPen.Create;

  AM:= C.AntialiasingMode;
  _Pen.Assign(C.Pen);

  X:= (R.Left+R.Right) div 2;
  C.Pen.Mode:= pmNotXor;
  C.Pen.Style:= psSolid;
  C.Pen.Color:= AColor;
  C.AntialiasingMode:= amOff;
  C.Pen.EndCap:= pecFlat;
  C.Pen.Width:= R.Right-R.Left;

  C.MoveTo(X, R.Top);
  C.LineTo(X, R.Bottom);

  C.Pen.Assign(_Pen);
  C.AntialiasingMode:= AM;
  C.Rectangle(0, 0, 0, 0); //apply pen
end;

finalization
  if Assigned(_Pen) then
    FreeAndNil(_Pen);

{$endif}

end.

