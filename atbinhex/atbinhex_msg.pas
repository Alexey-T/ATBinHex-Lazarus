unit atbinhex_msg;

interface

function MsgBox(const Msg, Title: UnicodeString; Flags: Integer; hWnd: THandle = 0): Integer;
procedure MsgInfo(const Msg: UnicodeString; hWnd: THandle = 0);
procedure MsgError(const Msg: UnicodeString; hWnd: THandle = 0);
procedure MsgWarning(const Msg: UnicodeString; hWnd: THandle = 0);

var
  ATViewerMessagesEnabled: Boolean = True;

var
  MsgViewerCaption: UnicodeString = 'Viewer';
  MsgViewerShowCfm: UnicodeString = 'Format unknown'#13'Click here to show binary dump';
  MsgViewerShowEmpty: UnicodeString = 'File is empty';
  MsgViewerErrCannotFindFile: UnicodeString = 'File not found: "%s"';
  MsgViewerErrCannotFindFolder: UnicodeString = 'Folder not found: "%s"';
  MsgViewerErrCannotOpenFile: UnicodeString = 'Cannot open file: "%s"';
  MsgViewerErrCannotLoadFile: UnicodeString = 'Cannot load file: "%s"';
  MsgViewerErrCannotReadFile: UnicodeString = 'Cannot read file: "%s"';
  MsgViewerErrCannotReadStream: UnicodeString = 'Cannot read stream';
  MsgViewerErrCannotReadPos: UnicodeString = 'Read error at offset %s';
  MsgViewerErrDetect: UnicodeString = 'Program could not detect file format'#13'Dump is shown';
  MsgViewerErrImage: UnicodeString = 'Unknown image format';
  MsgViewerErrMedia: UnicodeString = 'Unknown multimedia format';
  MsgViewerErrOffice: UnicodeString = 'MS Office module doesn''t support this file type';
  MsgViewerErrInitControl: UnicodeString = 'Cannot initialize %s';
  MsgViewerErrInitOffice: UnicodeString = 'Cannot initialize MS Office control';
  MsgViewerErrCannotCopyData: UnicodeString = 'Cannot copy data to Clipboard';
  MsgViewerWlxException: UnicodeString = 'Exception in plugin "%s" in function "%s"';
  MsgViewerWlxParentNotSpecified: UnicodeString = 'Cannot load plugins: parent form not specified';
  MsgViewerAniTitle: UnicodeString = 'Title: ';
  MsgViewerAniCreator: UnicodeString = 'Creator: ';
  MsgViewerPageHint: UnicodeString = 'Previous/Next page'#13'Current page: %d of %d';

implementation

uses
  SysUtils, Forms;

const
  IDCANCEL = 2;
  MB_ICONASTERISK = $40;
  MB_ICONEXCLAMATION = $30;
  MB_ICONWARNING = $30;
  MB_ICONERROR = $10;
  MB_ICONHAND = $10;
  MB_ICONQUESTION = $20;
  MB_OK = 0;
  MB_ICONINFORMATION = $40;
  MB_ICONSTOP = $10;
  MB_OKCANCEL = $1;

function MsgBox(const Msg, Title: UnicodeString; Flags: Integer; hWnd: THandle = 0): Integer;
begin
  if ATViewerMessagesEnabled then
    Result := Application.MessageBox(
      PChar(UTF8Encode(Msg)),
      PChar(UTF8Encode(Title)),
      Flags)
  else
    Result := IDCANCEL;
end;

procedure MsgInfo(const Msg: UnicodeString; hWnd: THandle = 0);
begin
  MsgBox(Msg, MsgViewerCaption, MB_OK or MB_ICONINFORMATION, hWnd);
end;

procedure MsgError(const Msg: UnicodeString; hWnd: THandle = 0);
begin
  MsgBox(Msg, MsgViewerCaption, MB_OK or MB_ICONERROR, hWnd);
end;

procedure MsgWarning(const Msg: UnicodeString; hWnd: THandle = 0);
begin
  MsgBox(Msg, MsgViewerCaption, MB_OK or MB_ICONEXCLAMATION, hWnd);
end;

end.
