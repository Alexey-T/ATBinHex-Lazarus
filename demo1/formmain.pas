unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ATBinHex;

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
    procedure Open(const Filename: string);
  public
    { public declarations }
    bh: TATBinHex;
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

  bh:= TATBinHex.Create(Self);
  bh.Parent:= Self;
  bh.Align:= alClient;

  bh.TextGutter:= true;
  bh.TextGutterLinesStep:= 10;

  fn:= ExtractFilePath(Application.ExeName)+'formmain.pas';
  if FileExists(fn) then
    Open(fn);
end;

procedure TfmMain.bTextChange(Sender: TObject);
begin
  bh.Mode:= vbmodeText;
end;

procedure TfmMain.bBinChange(Sender: TObject);
begin
  bh.Mode:= vbmodeBinary;
end;

procedure TfmMain.bHexChange(Sender: TObject);
begin
  bh.Mode:= vbmodeHex;
end;

procedure TfmMain.bOpenClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      Open(Filename);
end;

procedure TfmMain.bGotoClick(Sender: TObject);
var
  S: string;
  N: integer;
begin
  S:= InputBox('Go to', 'Offset:', '0');
  if S='' then Exit;
  N:= StrToInt(S);
  if N>fs.Size-10 then
  begin
    ShowMessage('Too big pos, max is '+IntToStr(fs.Size));
    Exit
  end;
  bh.Scroll(N, 3, 3);
end;

procedure TfmMain.Open(const Filename: string);
begin
  if Assigned(fs) then
  begin
    bh.OpenStream(nil);
    FreeAndNil(fs);
  end;
  fs:= TFileStream.Create(Filename, fmOpenRead);
  bh.OpenStream(fs);
  bh.Redraw;
end;

procedure TfmMain.bUniChange(Sender: TObject);
begin
  bh.Mode:= vbmodeUnicode;
end;

procedure TfmMain.bUniHexChange(Sender: TObject);
begin
  bh.Mode:= vbmodeUHex;
end;

procedure TfmMain.bFontClick(Sender: TObject);
begin
  with FontDialog1 do
    if Execute then
    begin
      bh.Font:= Font;
      bh.Redraw;
    end;
end;

procedure TfmMain.chkEnChange(Sender: TObject);
begin
  bh.Enabled:= chkEn.Checked;
  bh.Redraw;
end;

procedure TfmMain.chkEnSelChange(Sender: TObject);
begin
  bh.TextEnableSel:= chkEnSel.Checked;
end;

procedure TfmMain.chkGutterChange(Sender: TObject);
begin
  bh.TextGutter:= chkGutter.Checked;
  bh.Redraw;
end;

procedure TfmMain.chkUnprChange(Sender: TObject);
begin
  bh.TextNonPrintable:= chkUnpr.Checked;
end;

procedure TfmMain.chkWrapChange(Sender: TObject);
begin
  bh.TextWrap:= chkWrap.Checked;
end;

procedure TfmMain.edBinChange(Sender: TObject);
begin
  bh.TextWidth:= edBin.Value;
  bh.Redraw;
end;

procedure TfmMain.edTabsizeChange(Sender: TObject);
begin
  bh.TextTabSize:= edTabsize.Value;
  bh.Redraw;
end;

end.

