unit atbinhex_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATBinHex;

procedure Register;

implementation

{$R atbinhex.dcr}

{ Registration }
procedure Register;
begin
  RegisterComponents('AT Controls', [TATBinHex]);
end;

end.

