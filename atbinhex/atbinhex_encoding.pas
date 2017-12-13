unit atbinhex_encoding;

interface

uses
  {$ifdef windows}
  Windows,
  {$endif}
  SysUtils, Classes, Menus,
  LConvEncoding;

function SCodepageToUTF8(const AStr, AEnc: string): string;
function SCodepageFromUTF8(const AStr, AEnc: string): string;

const
  cEncNameAnsi = 'ANSI';
  cEncNameCP1250 = 'CP1250';
  cEncNameCP1251 = 'CP1251';
  cEncNameCP1252 = 'CP1252';
  cEncNameCP1253 = 'CP1253';
  cEncNameCP1254 = 'CP1254';
  cEncNameCP1255 = 'CP1255';
  cEncNameCP1256 = 'CP1256';
  cEncNameCP1257 = 'CP1257';
  cEncNameCP1258 = 'CP1258';
  cEncNameCP437 = 'CP437';
  cEncNameCP850 = 'CP850';
  cEncNameCP852 = 'CP852';
  cEncNameCP866 = 'CP866';
  cEncNameCP874 = 'CP874';
  cEncNameISO1 = 'ISO-8859-1';
  cEncNameISO2 = 'ISO-8859-2';
  cEncNameMac = 'Macintosh';
  cEncNameCP932 = 'CP932';
  cEncNameCP936 = 'CP936';
  cEncNameCP949 = 'CP949';
  cEncNameCP950 = 'CP950';

type
  TAppEncodingRecord = record
    Sub,
    Name,
    ShortName: string;
  end;

const
  AppEncodings: array[0..24] of TAppEncodingRecord = (
    (Sub: ''; Name: cEncNameAnsi; ShortName: 'ansi'),
    (Sub: ''; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: cEncNameCP1250; ShortName: cEncNameCP1250),
    (Sub: 'eu'; Name: cEncNameCP1251; ShortName: cEncNameCP1251),
    (Sub: 'eu'; Name: cEncNameCP1252; ShortName: cEncNameCP1252),
    (Sub: 'eu'; Name: cEncNameCP1253; ShortName: cEncNameCP1253),
    (Sub: 'eu'; Name: cEncNameCP1257; ShortName: cEncNameCP1257),
    (Sub: 'eu'; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: cEncNameCP437; ShortName: cEncNameCP437),
    (Sub: 'eu'; Name: cEncNameCP850; ShortName: cEncNameCP850),
    (Sub: 'eu'; Name: cEncNameCP852; ShortName: cEncNameCP852),
    (Sub: 'eu'; Name: cEncNameCP866; ShortName: cEncNameCP866),
    (Sub: 'eu'; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: cEncNameISO1; ShortName: cEncNameISO1),
    (Sub: 'eu'; Name: cEncNameISO2; ShortName: cEncNameISO2),
    (Sub: 'eu'; Name: cEncNameMac; ShortName: 'mac'),
    (Sub: 'mi'; Name: cEncNameCP1254; ShortName: cEncNameCP1254),
    (Sub: 'mi'; Name: cEncNameCP1255; ShortName: cEncNameCP1255),
    (Sub: 'mi'; Name: cEncNameCP1256; ShortName: cEncNameCP1256),
    (Sub: 'as'; Name: cEncNameCP874; ShortName: cEncNameCP874),
    (Sub: 'as'; Name: cEncNameCP932; ShortName: cEncNameCP932),
    (Sub: 'as'; Name: cEncNameCP936; ShortName: cEncNameCP936),
    (Sub: 'as'; Name: cEncNameCP949; ShortName: cEncNameCP949),
    (Sub: 'as'; Name: cEncNameCP950; ShortName: cEncNameCP950),
    (Sub: 'as'; Name: cEncNameCP1258; ShortName: cEncNameCP1258)
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
    437: Result:= CP437ToUTF8(SA);
    else Result:= CP1250ToUTF8(SA);
  end;
  {$else}
  Result:= CP1252ToUTF8(SA);
  {$endif}
end;

function SConvertUtf8ToAnsi(const SA: string): string;
begin
  {$ifdef windows}
  todo.... below
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
    437: Result:= CP437ToUTF8(SA);
    else Result:= CP1250ToUTF8(SA);
  end;
  {$else}
  Result:= UTF8ToCP1252(SA);
  {$endif}
end;

function SCodepageToUTF8(const AStr, AEnc: string): string;
var
  Ok: boolean;
begin
  Ok:= true;
  if (AEnc='') or (AEnc='ANSI') then
    Result:= SConvertAnsiToUtf8(AStr)
  else
    Result:= ConvertEncodingToUTF8(AStr, AEnc, Ok);
end;

function SCodepageFromUTF8(const AStr, AEnc: string): string;
var
  Ok: boolean;
begin
  Ok:= true;
  if (AEnc='') or (AEnc='ANSI') then
    Result:= SConvertUtf8ToAnsi(AStr)
  else
    Result:= ConvertEncodingFromUTF8(AStr, AEnc, Ok);
end;

end.

