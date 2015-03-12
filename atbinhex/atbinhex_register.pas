  unit atbinhex_register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATBinHex;

procedure Register;

implementation

{ Registration }
procedure Register;
begin
  RegisterComponents('Misc', [TATBinHex]);
end;

end.

