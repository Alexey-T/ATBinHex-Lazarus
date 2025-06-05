unit formmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, ComCtrls,
  EncConv,
  ATBinHex,
  ATStreamSearch,
  math;

type
  { TfmMain }

  TfmMain = class(TForm)
    btnOpen: TButton;
    btnFont: TButton;
    btnGoto: TButton;
    btnFind: TButton;
    btnFindNext: TButton;
    chkUTF8: TCheckBox;
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
    procedure btnFindClick(Sender: TObject);
    procedure btnFindNextClick(Sender: TObject);
    procedure btnGotoClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure bUniChange(Sender: TObject);
    procedure bUniHexChange(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure chkEnChange(Sender: TObject);
    procedure chkEnSelChange(Sender: TObject);
    procedure chkGutterChange(Sender: TObject);
    procedure chkUnprChange(Sender: TObject);
    procedure chkUTF8Change(Sender: TObject);
    procedure chkWrapChange(Sender: TObject);
    procedure edBinChange(Sender: TObject);
    procedure edTabsizeChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure bTextChange(Sender: TObject);
    procedure bBinChange(Sender: TObject);
    procedure bHexChange(Sender: TObject);
  private
    { private declarations }
    FPrevSearchText: string;
    procedure OpenFile(const Filename: string);
    procedure ViewerOptionsChange(Sender: TObject);
    procedure ViewerScroll(Sender: TObject);
  public
    { public declarations }
    FViewer: TATBinHex;
    FFileStream: TFileStream;
    FSearch: TATStreamSearch;
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
  FFileStream:= nil;

  FViewer:= TATBinHex.Create(Self);
  FViewer.Parent:= Self;
  FViewer.Align:= alClient;
  FViewer.Font.Size:= 10;
  FViewer.OnScroll:=@ViewerScroll;
  FViewer.OnOptionsChange:=@ViewerOptionsChange;

  FViewer.TextGutter:= true;
  FViewer.TextGutterLinesStep:= 10;

  fn:= ExtractFilePath(Application.ExeName)+'formmain.pas';
  if FileExists(fn) then
    OpenFile(fn);

  FSearch:= TATStreamSearch.Create(Self);
end;

procedure TfmMain.bTextChange(Sender: TObject);
begin
  FViewer.Mode:= vbmodeText;
end;

procedure TfmMain.bBinChange(Sender: TObject);
begin
  FViewer.Mode:= vbmodeBinary;
end;

procedure TfmMain.bHexChange(Sender: TObject);
begin
  FViewer.Mode:= vbmodeHex;
end;

procedure TfmMain.btnOpenClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      OpenFile(Filename);
end;

procedure TfmMain.btnGotoClick(Sender: TObject);
var
  S: string;
  N: Int64;
begin
  S:= InputBox('Go to', 'Hex offset:', '0');
  if S='' then Exit;
  N:= StrToInt64Def('$'+S, -1);
  if N<0 then exit;
  if N>FFileStream.Size-10 then
  begin
    ShowMessage('Too big pos, max is '+IntToStr(FFileStream.Size));
    Exit
  end;
  FViewer.PosAt(N);
end;

procedure TfmMain.btnFindClick(Sender: TObject);
var
  S: string;
  NCharSize: integer;
begin
  S:= InputBox('Find', 'String:', '');
  if S='' then exit;

  if FViewer.Mode in [vbmodeUnicode, vbmodeUHex] then
    NCharSize:= 2
  else
    NCharSize:= 1;

  FPrevSearchText:= S;

  FSearch.Stream:= FFileStream;
  if not FSearch.Find(S, 0, FFileStream.Size, FViewer.TextEncoding, NCharSize, []) then
    ShowMessage('Not found')
  else
    FViewer.SetSelection(FSearch.FoundStart, FSearch.FoundLength, true);

  btnFindNext.Enabled:= true;
end;

procedure TfmMain.btnFindNextClick(Sender: TObject);
var
  NCharSize: integer;
begin
  if FPrevSearchText='' then
  begin
    ShowMessage('Use "Find first" before "Find next"');
    exit;
  end;

  if FViewer.Mode in [vbmodeUnicode, vbmodeUHex] then
    NCharSize:= 2
  else
    NCharSize:= 1;

  FSearch.Stream:= FFileStream;
  if not FSearch.Find(FPrevSearchText, FSearch.FoundStart+NCharSize, FFileStream.Size, FViewer.TextEncoding, NCharSize, []) then
    ShowMessage('Not found')
  else
    FViewer.SetSelection(FSearch.FoundStart, FSearch.FoundLength, true);
end;

procedure TfmMain.OpenFile(const Filename: string);
begin
  if Assigned(FFileStream) then
  begin
    FViewer.OpenStream(nil);
    FreeAndNil(FFileStream);
  end;

  FFileStream:= TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  FViewer.OpenStream(FFileStream);
end;

procedure TfmMain.ViewerOptionsChange(Sender: TObject);
begin
  StatusBar1.Panels[1].Text:= cEncConvNames[FViewer.TextEncoding];
end;

procedure TfmMain.ViewerScroll(Sender: TObject);
begin
  StatusBar1.Panels[0].Text:= IntToStr(FViewer.PosPercent)+'%';
end;

procedure TfmMain.bUniChange(Sender: TObject);
begin
  FViewer.Mode:= vbmodeUnicode;
end;

procedure TfmMain.bUniHexChange(Sender: TObject);
begin
  FViewer.Mode:= vbmodeUHex;
end;

procedure TfmMain.btnFontClick(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    FViewer.Font:= FontDialog1.Font;
    FViewer.Invalidate;
  end;
end;

procedure TfmMain.chkEnChange(Sender: TObject);
begin
  FViewer.Enabled:= chkEn.Checked;
  FViewer.Invalidate;
end;

procedure TfmMain.chkEnSelChange(Sender: TObject);
begin
  FViewer.TextEnableSel:= chkEnSel.Checked;
end;

procedure TfmMain.chkGutterChange(Sender: TObject);
begin
  FViewer.TextGutter:= chkGutter.Checked;
  FViewer.Invalidate;
end;

procedure TfmMain.chkUnprChange(Sender: TObject);
begin
  FViewer.TextNonPrintable:= chkUnpr.Checked;
end;

procedure TfmMain.chkUTF8Change(Sender: TObject);
begin
  if chkUTF8.Checked then
    FViewer.TextEncoding:= eidUTF8
  else
    FViewer.TextEncoding:= eidCP1252;
end;

procedure TfmMain.chkWrapChange(Sender: TObject);
begin
  FViewer.TextWrap:= chkWrap.Checked;
end;

procedure TfmMain.edBinChange(Sender: TObject);
begin
  FViewer.TextWidth:= edBin.Value;
  FViewer.Invalidate;
end;

procedure TfmMain.edTabsizeChange(Sender: TObject);
begin
  FViewer.TextTabSize:= edTabsize.Value;
  FViewer.Invalidate;
end;

end.

