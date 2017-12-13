unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ComCtrls, ATBinHex;

type
  { TfmMain }

  TfmMain = class(TForm)
    bOpen: TButton;
    bFont: TButton;
    bGoto: TButton;
    chkEnSel: TCheckBox;
    chkEn: TCheckBox;
    chkWrap: TCheckBox;
    chkUnpr: TCheckBox;
    chkGutter: TCheckBox;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    PanelOpt: TPanel;
    bText: TRadioButton;
    bBin: TRadioButton;
    bHex: TRadioButton;
    bUni: TRadioButton;
    bUniHex: TRadioButton;
    edBin: TSpinEdit;
    edTabsize: TSpinEdit;
    StatusBar1: TStatusBar;
    procedure bGotoClick(Sender: TObject);
    procedure bOpenClick(Sender: TObject);
    procedure bUniChange(Sender: TObject);
    procedure bUniHexChange(Sender: TObject);
    procedure bFontClick(Sender: TObject);
    procedure chkEnChange(Sender: TObject);
    procedure chkEnSelChange(Sender: TObject);
    procedure chkGutterChange(Sender: TObject);
    procedure chkUnprChange(Sender: TObject);
    procedure chkWrapChange(Sender: TObject);
    procedure edBinChange(Sender: TObject);
    procedure edTabsizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bTextChange(Sender: TObject);
    procedure bBinChange(Sender: TObject);
    procedure bHexChange(Sender: TObject);
  private
    { private declarations }
    procedure OpenFile(const Filename: string);
    procedure ViewerOptionsChange(Sender: TObject);
    procedure ViewerScroll(Sender: TObject);
  public
    { public declarations }
    V: TATBinHex;
    fs: TFileStream;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

{ TfmMain }

procedure TfmMain.FormCreate(Sender: TObject);
var
  fn: string;
begin
  fs:= nil;

  V:= TATBinHex.Create(Self);
  V.Parent:= Self;
  V.Align:= alClient;
  V.Font.Size:= 10;
  V.OnScroll:=@ViewerScroll;
  V.OnOptionsChange:=@ViewerOptionsChange;

  V.TextGutter:= true;
  V.TextGutterLinesStep:= 10;

  fn:= ExtractFilePath(Application.ExeName)+'formmain.pas';
  if FileExists(fn) then
    OpenFile(fn);
end;

procedure TfmMain.bTextChange(Sender: TObject);
begin
  V.Mode:= vbmodeText;
end;

procedure TfmMain.bBinChange(Sender: TObject);
begin
  V.Mode:= vbmodeBinary;
end;

procedure TfmMain.bHexChange(Sender: TObject);
begin
  V.Mode:= vbmodeHex;
end;

procedure TfmMain.bOpenClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      OpenFile(Filename);
end;

procedure TfmMain.bGotoClick(Sender: TObject);
var
  S: string;
  N: Int64;
begin
  S:= InputBox('Go to', 'Hex offset:', '0');
  if S='' then Exit;
  N:= StrToInt64Def('$'+S, -1);
  if N<0 then exit;
  if N>fs.Size-10 then
  begin
    ShowMessage('Too big pos, max is '+IntToStr(fs.Size));
    Exit
  end;
  V.PosAt(N);
end;

procedure TfmMain.OpenFile(const Filename: string);
begin
  if Assigned(fs) then
  begin
    V.OpenStream(nil);
    FreeAndNil(fs);
  end;
  fs:= TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  V.OpenStream(fs);
  V.Redraw;
end;

procedure TfmMain.ViewerOptionsChange(Sender: TObject);
begin
  StatusBar1.Panels[1].Text:= V.TextEncoding;
end;

procedure TfmMain.ViewerScroll(Sender: TObject);
begin
  StatusBar1.Panels[0].Text:= IntToStr(V.PosPercent)+'%';
end;

procedure TfmMain.bUniChange(Sender: TObject);
begin
  V.Mode:= vbmodeUnicode;
end;

procedure TfmMain.bUniHexChange(Sender: TObject);
begin
  V.Mode:= vbmodeUHex;
end;

procedure TfmMain.bFontClick(Sender: TObject);
begin
  with FontDialog1 do
    if Execute then
    begin
      V.Font:= Font;
      V.Redraw;
    end;
end;

procedure TfmMain.chkEnChange(Sender: TObject);
begin
  V.Enabled:= chkEn.Checked;
  V.Redraw;
end;

procedure TfmMain.chkEnSelChange(Sender: TObject);
begin
  V.TextEnableSel:= chkEnSel.Checked;
end;

procedure TfmMain.chkGutterChange(Sender: TObject);
begin
  V.TextGutter:= chkGutter.Checked;
  V.Redraw;
end;

procedure TfmMain.chkUnprChange(Sender: TObject);
begin
  V.TextNonPrintable:= chkUnpr.Checked;
end;

procedure TfmMain.chkWrapChange(Sender: TObject);
begin
  V.TextWrap:= chkWrap.Checked;
end;

procedure TfmMain.edBinChange(Sender: TObject);
begin
  V.TextWidth:= edBin.Value;
  V.Redraw;
end;

procedure TfmMain.edTabsizeChange(Sender: TObject);
begin
  V.TextTabSize:= edTabsize.Value;
  V.Redraw;
end;

end.

