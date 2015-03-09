program demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, formmain,
  ATCanvasProc, atstreamsearch, atstringproc, atviewermsg,
  ATxClipboard, ATxCodepages, ATxSHex, ATxSProc, ATBinHex;

{$R *.res}

begin
  RequireDerivedFormResource:= True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

