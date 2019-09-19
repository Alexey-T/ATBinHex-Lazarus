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
    Sub,
    Name,
    ShortName: string;
  end;

const
  AppEncodings: array[0..22] of TAppEncodingRecord = (
    (Sub: 'eu'; Name: 'cp1250'; ShortName: 'cp1250'),
    (Sub: 'eu'; Name: 'cp1251'; ShortName: 'cp1251'),
    (Sub: 'eu'; Name: 'cp1252'; ShortName: 'cp1252'),
    (Sub: 'eu'; Name: 'cp1253'; ShortName: 'cp1253'),
    (Sub: 'eu'; Name: 'cp1257'; ShortName: 'cp1257'),
    (Sub: 'eu'; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: 'cp437'; ShortName: 'cp437'),
    (Sub: 'eu'; Name: 'cp850'; ShortName: 'cp850'),
    (Sub: 'eu'; Name: 'cp852'; ShortName: 'cp852'),
    (Sub: 'eu'; Name: 'cp866'; ShortName: 'cp866'),
    (Sub: 'eu'; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: 'iso88591'; ShortName: 'iso88591'),
    (Sub: 'eu'; Name: 'iso88592'; ShortName: 'iso88592'),
    (Sub: 'eu'; Name: 'mac'; ShortName: 'mac'),
    (Sub: 'mi'; Name: 'cp1254'; ShortName: 'cp1254'),
    (Sub: 'mi'; Name: 'cp1255'; ShortName: 'cp1255'),
    (Sub: 'mi'; Name: 'cp1256'; ShortName: 'cp1256'),
    (Sub: 'as'; Name: 'cp874'; ShortName: 'cp874'),
    (Sub: 'as'; Name: 'cp932'; ShortName: 'cp932'),
    (Sub: 'as'; Name: 'cp936'; ShortName: 'cp936'),
    (Sub: 'as'; Name: 'cp949'; ShortName: 'cp949'),
    (Sub: 'as'; Name: 'cp950'; ShortName: 'cp950'),
    (Sub: 'as'; Name: 'cp1258'; ShortName: 'cp1258')
  );


implementation

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

function SCodepageToUTF8(const AStr: string; AEnc: TEncConvId): string;
begin
  Result:= EncConvertToUTF8(AStr, AEnc);
end;

function SCodepageFromUTF8(const AStr: string; AEnc: TEncConvId): string;
begin
  Result:= EncConvertFromUTF8(AStr, AEnc);
end;

end.

