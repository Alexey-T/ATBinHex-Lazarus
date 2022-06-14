unit atbinhex_encoding;

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  SysUtils, Classes, Menus,
  EncConv;

function SCodepageToUTF8(const AStr: string; AEnc: TEncConvId): string;
function SCodepageFromUTF8(const AStr: string; AEnc: TEncConvId): string;

type
  TAppEncodingRecord = record
    Sub: string;
    Name: string;
    Id: TEncConvId
  end;

const
  AppEncodings: array[0..32] of TAppEncodingRecord = (
    (Sub: 'u'; Name: 'UTF-8'; Id: eidUTF8),
    (Sub: 'eu'; Name: 'cp1250'; Id: eidCP1250),
    (Sub: 'eu'; Name: 'cp1251'; Id: eidCP1251),
    (Sub: 'eu'; Name: 'cp1252'; Id: eidCP1252),
    (Sub: 'eu'; Name: 'cp1253'; Id: eidCP1253),
    (Sub: 'eu'; Name: 'cp1257'; Id: eidCP1257),
    (Sub: 'eu'; Name: '-'; Id: eidUTF8),
    (Sub: 'eu'; Name: 'cp437'; Id: eidCP437),
    (Sub: 'eu'; Name: 'cp850'; Id: eidCP850),
    (Sub: 'eu'; Name: 'cp852'; Id: eidCP852),
    (Sub: 'eu'; Name: 'cp866'; Id: eidCP866),
    (Sub: 'eu'; Name: '-'; Id: eidUTF8),
    (Sub: 'eu'; Name: 'iso-8859-1'; Id: eidISO1),
    (Sub: 'eu'; Name: 'iso-8859-2'; Id: eidISO2),
    (Sub: 'eu'; Name: 'iso-8859-5'; Id: eidISO5),
    (Sub: 'eu'; Name: 'iso-8859-9'; Id: eidISO9),
    (Sub: 'eu'; Name: 'iso-8859-14'; Id: eidISO14),
    (Sub: 'eu'; Name: 'iso-8859-15'; Id: eidISO15),
    (Sub: 'eu'; Name: 'iso-8859-16'; Id: eidISO16),
    (Sub: 'eu'; Name: 'mac'; Id: eidCPMac),
    (Sub: 'mi'; Name: 'cp1254'; Id: eidCP1254),
    (Sub: 'mi'; Name: 'cp1255'; Id: eidCP1255),
    (Sub: 'mi'; Name: 'cp1256'; Id: eidCP1256),
    (Sub: 'mi'; Name: '-'; Id: eidUTF8),
    (Sub: 'mi'; Name: 'koi8r'; Id: eidKOI8R),
    (Sub: 'mi'; Name: 'koi8u'; Id: eidKOI8U),
    (Sub: 'mi'; Name: 'koi8ru'; Id: eidKOI8RU),
    (Sub: 'as'; Name: 'cp874'; Id: eidCP874),
    (Sub: 'as'; Name: 'cp932'; Id: eidCP932),
    (Sub: 'as'; Name: 'cp936'; Id: eidCP936),
    (Sub: 'as'; Name: 'cp949'; Id: eidCP949),
    (Sub: 'as'; Name: 'cp950'; Id: eidCP950),
    (Sub: 'as'; Name: 'cp1258'; Id: eidCP1258)
  );


implementation

(*
function SConvertAnsiToUtf8(const SA: string): string;
begin
  {$ifdef windows}
  case Windows.GetACP of
    1250: Result:= CP1250ToUTF8(SA);
    1251: Result:= CP1251ToUTF8(SA);
    1252: Result:= CP1252ToUTF8(SA);
    1253: Result:= CP1253ToUTF8(SA);
    1254: Result:= CP1254ToUTF8(SA);
    1255: Result:= CP1255ToUTF8(SA);
    1256: Result:= CP1256ToUTF8(SA);
    1257: Result:= CP1257ToUTF8(SA);
    1258: Result:= CP1258ToUTF8(SA);
    437:  Result:= CP437ToUTF8(SA);
    else  Result:= CP1252ToUTF8(SA);
  end;
  {$else}
  Result:= CP1252ToUTF8(SA);
  {$endif}
end;

function SConvertUtf8ToAnsi(const SA: string): string;
begin
  {$ifdef windows}
  case Windows.GetACP of
    1250: Result:= UTF8ToCP1250(SA);
    1251: Result:= UTF8ToCP1251(SA);
    1252: Result:= UTF8ToCP1252(SA);
    1253: Result:= UTF8ToCP1253(SA);
    1254: Result:= UTF8ToCP1254(SA);
    1255: Result:= UTF8ToCP1255(SA);
    1256: Result:= UTF8ToCP1256(SA);
    1257: Result:= UTF8ToCP1257(SA);
    1258: Result:= UTF8ToCP1258(SA);
    437:  Result:= UTF8ToCP437(SA);
    else  Result:= UTF8ToCP1252(SA);
  end;
  {$else}
  Result:= UTF8ToCP1252(SA);
  {$endif}
end;
*)

function SCodepageToUTF8(const AStr: string; AEnc: TEncConvId): string;
begin
  Result:= EncConvertToUTF8(AStr, AEnc);
end;

function SCodepageFromUTF8(const AStr: string; AEnc: TEncConvId): string;
begin
  Result:= EncConvertFromUTF8(AStr, AEnc);
end;

end.

