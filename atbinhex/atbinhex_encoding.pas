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

type
  TAppEncodingRecord = record
    Sub,
    Name,
    ShortName: string;
  end;

const
  AppEncodings: array[0..22] of TAppEncodingRecord = (
    (Sub: 'eu'; Name: EncodingCP1250; ShortName: EncodingCP1250),
    (Sub: 'eu'; Name: EncodingCP1251; ShortName: EncodingCP1251),
    (Sub: 'eu'; Name: EncodingCP1252; ShortName: EncodingCP1252),
    (Sub: 'eu'; Name: EncodingCP1253; ShortName: EncodingCP1253),
    (Sub: 'eu'; Name: EncodingCP1257; ShortName: EncodingCP1257),
    (Sub: 'eu'; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: EncodingCP437; ShortName: EncodingCP437),
    (Sub: 'eu'; Name: EncodingCP850; ShortName: EncodingCP850),
    (Sub: 'eu'; Name: EncodingCP852; ShortName: EncodingCP852),
    (Sub: 'eu'; Name: EncodingCP866; ShortName: EncodingCP866),
    (Sub: 'eu'; Name: '-'; ShortName: ''),
    (Sub: 'eu'; Name: EncodingCPIso1; ShortName: EncodingCPIso1),
    (Sub: 'eu'; Name: EncodingCPIso2; ShortName: EncodingCPIso2),
    (Sub: 'eu'; Name: EncodingCPMac; ShortName: 'mac'),
    (Sub: 'mi'; Name: EncodingCP1254; ShortName: EncodingCP1254),
    (Sub: 'mi'; Name: EncodingCP1255; ShortName: EncodingCP1255),
    (Sub: 'mi'; Name: EncodingCP1256; ShortName: EncodingCP1256),
    (Sub: 'as'; Name: EncodingCP874; ShortName: EncodingCP874),
    (Sub: 'as'; Name: EncodingCP932; ShortName: EncodingCP932),
    (Sub: 'as'; Name: EncodingCP936; ShortName: EncodingCP936),
    (Sub: 'as'; Name: EncodingCP949; ShortName: EncodingCP949),
    (Sub: 'as'; Name: EncodingCP950; ShortName: EncodingCP950),
    (Sub: 'as'; Name: EncodingCP1258; ShortName: EncodingCP1258)
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

function SCodepageToUTF8(const AStr, AEnc: string): string;
var
  Ok: boolean;
begin
  Ok:= true;
  if (AEnc='') then
    Result:= SConvertAnsiToUtf8(AStr)
  else
    Result:= ConvertEncodingToUTF8(AStr, AEnc, Ok);
end;

function SCodepageFromUTF8(const AStr, AEnc: string): string;
var
  Ok: boolean;
begin
  Ok:= true;
  if (AEnc='') then
    Result:= SConvertUtf8ToAnsi(AStr)
  else
    Result:= ConvertEncodingFromUTF8(AStr, AEnc, Ok);
end;

end.

