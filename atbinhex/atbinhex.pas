{***********************************}
{                                   }
{  ATBinHex Component               }
{  Copyright (C) Alexey Torgashin   }
{  http://uvviewsoft.com            }
{                                   }
{***********************************}

{$OPTIMIZATION OFF} //Delphi 5 cannot compile this with optimization on
{$BOOLEVAL OFF}    //Short boolean evaluation required
{$RANGECHECKS OFF} //For assignment compatability between DWORD and Longint

{$I atbinhexoptions.inc}   //ATBinHex options
{$R res/atbinhexresources.res} //ATBinHex resources

{$mode delphi}

unit ATBinHex;

interface

uses
  Messages, SysUtils, Classes, Controls, Graphics,
  ExtCtrls,
  LMessages,
  EncConv,
  ATBinHex_Encoding,
  {$ifdef NOTIF} ATFileNotification, {$endif}
  {$ifdef NOTIF} ATFileNotificationSimple, {$endif}
  {$ifdef SEARCH} ATStreamSearch, {$endif}
  Math,
  Menus;


type
  TATBinHexMode = (
    vbmodeText,
    vbmodeBinary,
    vbmodeHex,
    vbmodeUnicode,
    vbmodeUHex
    );

  TATUnicodeFormat = (
    vbUnicodeFmtUnknown,
    vbUnicodeFmtLE,
    vbUnicodeFmtBE
    );

  TATDirection = (
    vdirUp,
    vdirDown
    );

  TATMouseRelativePosition = (
    vmPosInner,
    vmPosUpper,
    vmPosLower,
    vmPosLefter,
    vmPosRighter
    );

  TATFileSource = (
    vfSrcNone,
    vfSrcStream
    );

  TATLineType = (
    vbLineAll,
    vbLineWord,
    vbLineURL
    );

  TATPopupCommand = (
    vpCmdCopy,
    vpCmdCopyHex,
    vpCmdCopyLink,
    vpCmdSelectLine,
    vpCmdSelectAll,
    vpCmdEncMenu
    );

  TATPopupCommands = set of TATPopupCommand;

  TATBinHexOutputOptions = record
    ShowNonPrintable: Boolean; //"Show non-printable" mode is on
    ShowCR: Boolean;           //Current line has CR (not wrapped)
    IsFontFixed: Boolean;  //Current font has fixed width
    TabSize: Integer;      //"Tab size" value
    TextSize: TPoint;
  end;

  TATBinHexDrawLine = procedure(
    ASender: TObject;
    ACanvas: TCanvas;
    const AStr, AStrAll: UnicodeString;
    const ARect: TRect;
    const ATextPnt: TPoint;
    var ADone: Boolean) of object;

  TATBinHexDrawLine2 = procedure(
    ASender: TObject;
    ACanvas: TCanvas;
    const AStr: UnicodeString;
    const APnt: TPoint;
    const AOptions: TATBinHexOutputOptions) of object;

  TATBinHexClickURL = procedure(
    ASender: TObject;
    const AString: AnsiString) of object;

const
  cMaxLength = 2 * 1024; //Limits for "Maximal line length" value
  cMinLength = 2;        //
  cMaxLengthSel = 8 * 1024; //Max length for "select line" command
  cMaxURLs = 500;        //Max URLs count in buffer
  cFindGap = 30; //FindAll: offset below buffer position to find partial matches
  cFindMax = 1000; //FindAll: max matches

type
  TATStringExtent = array[0 .. cMaxLength] of Integer;
  TATUrlArray = array[1 .. cMaxURLs] of record
    FString: AnsiString;
    FPos: Int64;
  end;
  TATFindArray = array[1 .. cFindMax] of record
    FPos, FLen: Int64;
  end;

function StringExtent(
  ACanvas: TCanvas;
  const AStr: UnicodeString;
  out AExtent: TATStringExtent;
  const AOptions: TATBinHexOutputOptions): Boolean;


const
  cATBinHexCommandSet = [
    vpCmdCopy,
    vpCmdCopyHex,
    vpCmdCopyLink,
    vpCmdSelectLine,
    vpCmdSelectAll,
    vpCmdEncMenu];
  cATBinHexBkColor = $ECECEC;  //Default Hex mode back color

type
  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[1 .. 1000 * 1000] of Integer;

var
  TextRepFrom1: string = '';
  TextRepFrom2: string = '';
  TextRepFrom3: string = '';
  TextRepTo1: char = '.';
  TextRepTo2: char = '.';
  TextRepTo3: char = '.';

type

  { TATBinHex }

  TATBinHex = class(TCustomControl)
  private
    FFileName: string;
    FFileSize: Int64;
    FFileOK: Boolean;
    FFileUnicodeFmt: TATUnicodeFormat;
    FFileSourceType: TATFileSource;

    {$ifdef SEARCH}
    FSearch,
    FSearch2: TATStreamSearch;
    FSearchStarted: Boolean;
    {$endif}

    FLineSp: integer;
    FStream: TStream;
    FBuffer: PAnsiChar;
    FBufferMaxOffset: Integer;
    FBufferAllocSize: Integer;

    FLinesShow: Boolean;
    FLinesStep: Integer;
    FLinesBufSize: Integer;
    FLinesData: PIntegerArray;
    FLinesNum: Integer;
    FLinesCount: Integer;
    FLinesExtUse: Boolean;
    FLinesExtList: AnsiString;

    FBitmap: TBitmap;
    FTimerAutoScroll: TTimer;
    FTimerNiceScroll: TTimer;
    FStrings: TObject;
    FTextEncoding: TEncConvId;

    FMenu: TPopupMenu;
    FMenuItemCopy: TMenuItem;
    FMenuItemCopyHex: TMenuItem;
    FMenuItemCopyLink: TMenuItem;
    FMenuItemSelectLine: TMenuItem;
    FMenuItemSelectAll: TMenuItem;
    FMenuItemEncMenu: TMenuItem;
    FMenuItemSep1: TMenuItem;
    FMenuItemSep2: TMenuItem;
    FMenuEnc: TPopupMenu;

    {$ifdef NOTIF}
    FNotif: TATFileNotification;
    FNotif2: TATFileNotificationSimple;
    {$endif}

    FAutoReload: Boolean;
    FAutoReloadBeep: Boolean;
    FAutoReloadFollowTail: Boolean;
    FAutoReloadSimple: Boolean;
    FAutoReloadSimpleTime: Integer;

    FLockCount: Integer;
    FBufferPos: Int64;
    FViewPos: Int64; //Position of view area (bytes)
    FViewAtEnd: Boolean; //Shows if we are at the end of file, after redraw
    FViewPageSize: Int64; //Page size (number of bytes on screen), after redraw
    FHViewPos: Integer; //Horizontal scroll position (px)
    FHViewWidth: Integer; //Horizontal width of text on screen, after redraw
    FPrevViewPos: Int64;
    FPrevHViewPos: Integer;
    FResizeFollowTail: Boolean; //Notepad feature: when control increases height, it follows file tail

    FSelStart: Int64;
    FSelLength: Int64;
    FMode: TATBinHexMode;
    FUrlArray: TATUrlArray;
    FFindArray: TATFindArray;
    FUrlShow: Boolean;
    FTextWidth: Integer;
    FTextWidthHex: Integer;
    FTextWidthUHex: Integer;
    FTextWidthFit: Boolean;
    FTextWidthFitHex: Boolean;
    FTextWidthFitUHex: Boolean;
    FTextWrap: Boolean;
    FTextNonPrintable: Boolean;
    FTextOemSpecial: Boolean;
    FTextGutter: Boolean;
    FTextGutterWidth: Integer;
    FTextColorHex: TColor;
    FTextColorHex2: TColor;
    FTextColorHexBack: TColor;
    FTextColorLines: TColor;
    FTextColorError: TColor;
    FTextColorGutter: TColor;
    FTextColorURL: TColor;
    FTextColorHi: TColor;
    FSearchIndentVert: Integer;
    FSearchIndentHorz: Integer;
    FTabSize: Integer;
    FPopupCommands: TATPopupCommands;
    FEnabled2: Boolean;
    FEnableSel: Boolean;
    FMaxLength: Integer;
    FMaxLengths: array[TATBinHexMode] of Integer;
    FMaxClipboardDataSizeMb: Integer;
    FFontFooter: TFont;
    FFontGutter: TFont;
    FHexOffsetLen: Integer;
    FFontHeight: Integer;
    FFontFirstChar: AnsiChar;
    FFontWidthDigits: Integer;
    FFontMonospaced: Boolean;
    FMouseDown: Boolean;
    FMouseStart: Int64;
    FMouseStartShift: Int64;
    FMouseStartDbl: Int64;
    FMouseDblClick: Boolean;
    FMouseTriClick: Boolean;
    FMouseTriTime: DWORD;
    FMousePopupPos: TPoint;
    FMouseRelativePos: TATMouseRelativePosition;
    FMouseNiceScroll: Boolean;
    FMouseNiceScrollPos: TPoint;
    FClientHeight: Integer;
    FTextSize: TPoint;

    FOnSelectionChange: TNotifyEvent;
    FOnOptionsChange: TNotifyEvent;
    FOnScroll: TNotifyEvent;
    FOnDrawLine: TATBinHexDrawLine;
    FOnDrawLine2: TATBinHexDrawLine2;
    FOnClickURL: TATBinHexClickURL;

    {$ifdef NOTIF}
    FOnFileReload: TNotifyEvent;
    {$endif}

    {$ifdef PRINT}
    FMarginLeft: Double;
    FMarginTop: Double;
    FMarginRight: Double;
    FMarginBottom: Double;
    FPrintFooter: Boolean;
    {$endif}

    procedure AllocBuffer;
    function GetVertScroll_MaxPos: Int64;
    function GetVertScroll_PageSize: Int64;
    function SourceAssigned: Boolean;
    function ReadSource(const APos: Int64; ABuffer: Pointer; ABufferSize: DWORD; var AReadSize: DWORD): Boolean;
    procedure ReadBuffer(const APos: Int64 = -1);
    procedure InitData;
    procedure FreeData;
    function LoadFile(ANewFile: Boolean): Boolean;
    function LoadStream: Boolean;
    function PosBefore(const APos: Int64; ALineType: TATLineType; ADir: TATDirection): Int64;
    procedure ReadUnicodeFmt;
    procedure HideScrollbars;
    procedure UpdateMenuEncodings(AMenu: TMenuItem);
    procedure UpdateVertScrollbar;
    procedure UpdateHorzScrollbar;
    procedure SetMode(AMode: TATBinHexMode);
    procedure SetTextWidthTo(AValue: Integer; var AField: Integer);
    procedure SetTextWidthHexTo(AValue: Integer; var AField: Integer);
    procedure SetTextWidthUHexTo(AValue: Integer; var AField: Integer);
    procedure SetTextWidth(AValue: Integer);
    procedure SetTextWidthHex(AValue: Integer);
    procedure SetTextWidthUHex(AValue: Integer);
    procedure SetTextWidthFit(AValue: Boolean);
    procedure SetTextWidthFitHex(AValue: Boolean);
    procedure SetTextWidthFitUHex(AValue: Boolean);
    procedure SetTextWrap(AValue: Boolean);
    procedure SetTextNonPrintable(AValue: Boolean);
    procedure SetTextUrlHilight(AValue: Boolean);
    procedure SetTextEncoding(AValue: TEncConvId);
    procedure SetSearchIndentVert(AValue: Integer);
    procedure SetSearchIndentHorz(AValue: Integer);
    procedure SetFontFooter(AValue: TFont);
    procedure SetFontGutter(AValue: TFont);
    procedure SetLinesBufSize(AValue: Integer);
    procedure SetLinesCount(AValue: Integer);
    procedure SetLinesStep(AValue: Integer);
    procedure InitHexOffsetLen;
    procedure MsgReadError;
    function MsgReadRetry: Boolean;
    procedure MsgOpenError;
    function DrawOffsetX: Integer;
    function DrawOffsetY: Integer;
    procedure SetTextGutter(AValue: Boolean);
    function LinesNum(ABitmap: TBitmap = nil): Integer;
    function ColsNumFit(ABitmap: TBitmap = nil): Integer;
    function ColsNumHexFit(ABitmap: TBitmap = nil): Integer;
    function ColsNumUHexFit(ABitmap: TBitmap = nil): Integer;
    function ColsNum(ABitmap: TBitmap = nil): Integer;
    function PosBad(const APos: Int64): Boolean;
    procedure PosFixCRLF(var APos: Int64);
  public
    function PosMax: Int64;
    function PosLast: Int64;
    procedure PosAt(const APos: Int64; ARedraw: Boolean = True);
    procedure PosDec(const N: Int64);
    procedure PosInc(const N: Int64);
    procedure PosLineUp(ALines: Integer = 1); overload;
    procedure PosLineDown(ALines: Integer = 1); overload;
    procedure PosLineUp(AViewAtEnd: Boolean; ALines: Integer); overload;
    procedure PosLineDown(AViewAtEnd: Boolean; ALines: Integer); overload;
    procedure PosBegin;
    procedure PosEnd;
    procedure PosEndTry;
    procedure PosPageUp;
    procedure PosPageDown;
    procedure HPosAt(APos: Integer; ARedraw: Boolean = True);
    procedure HPosInc(N: Integer);
    procedure HPosDec(N: Integer);
    procedure HPosBegin;
    procedure HPosEnd;
    procedure HPosLeft;
    procedure HPosRight;
    procedure HPosPageLeft;
    procedure HPosPageRight;
    function HPosWidth: Integer;
    function HPosMax: Integer;
  private
    function LineWithCR(const APos: Int64; const ALine: UnicodeString): Boolean;
    function LineWithGutterDot(const APos: Int64): Boolean;
    function OutputOptions(AShowCR: Boolean = False): TATBinHexOutputOptions;

    function GetPosPercent: Integer;
    procedure SetPosPercent(APos: Integer);
    function GetPosLine: Integer;
    procedure SetPosLine(ALine: Integer);

    function GetPosOffset: Int64;
    procedure SetPosOffset(const APos: Int64);

    procedure MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    function MousePosition(AX, AY: Integer; AStrict: Boolean = False): Int64;
    procedure MouseMoveAction(AX, AY: Integer);
    procedure TimerAutoScrollTimer(Sender: TObject);
    procedure TimerNiceScrollTimer(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemCopyHexClick(Sender: TObject);
    procedure MenuItemCopyLinkClick(Sender: TObject);
    procedure MenuItemSelectLineClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure UpdateMenu(Sender: TObject);
    function GetTextPopupCaption(AIndex: TATPopupCommand): AnsiString;
    procedure SetTextPopupCaption(AIndex: TATPopupCommand; const AValue: AnsiString);
    procedure SetTabSize(AValue: Integer);
    procedure DoAddEncodingItem(AMenu: TMenuItem; Sub, SName: string; AEncId: TEncConvId);

    procedure InitURLs;
    procedure FindURLs(ABufSize: DWORD);
    function PosURL(const APos: Int64): AnsiString;
    function IsPosURL(const APos: Int64): Boolean;

    function FindLineLength(const AStartPos: Int64; ADir: TATDirection; var ALine: UnicodeString): Integer;
    function FindLinePos(const AStartPos: Int64; ADir: TATDirection; var ALine: UnicodeString; APassiveMove: Boolean = False): Int64;
    procedure PosNextLineFrom(const AStartPos: Int64; ALinesNum: Integer; ADir: TATDirection; APassiveMove: Boolean = False; ARedraw: Boolean = True);
    procedure PosNextLine(ALinesNum: Integer; ADir: TATDirection; AViewAtEnd: Boolean);
    function IsCharSpec(ch: WideChar): boolean;
    function GetChar(const ACharPos: Int64): WideChar;
    function GetHex(const ACharPos: Int64): UnicodeString;
    function DecodeString(const S: UnicodeString): UnicodeString;
    function CharSize: Integer;
    function IsFileEmpty: Boolean;
    function IsModeVariable: Boolean;
    function IsModeUnicode: Boolean;
    function IsUnicodeBE: Boolean;
    procedure NormalizePos(var APos: Int64);
    function NormalizedPos(const APos: Int64): Int64;
    procedure NextPos(var APos: Int64; ADir: TATDirection; AChars: Integer = 1);
    procedure SelectLineAtPos(const APos: Int64; ALineType: TATLineType);
    procedure ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    function ActiveFont: TFont;
    function ActiveLinesShow: Boolean;

    procedure DrawGutterTo(ABitmap: TBitmap);
    procedure DrawEmptyTo(
      ABitmap: TBitmap;
      APageWidth,
      APageHeight: Integer;
      APrintMode: Boolean);

    {$ifdef NOTIF}
    procedure NotifChanged(Sender: TObject);
    procedure DoFileReload;
    procedure SetAutoReload(AValue: Boolean);
    {$endif}

    procedure Lock;
    procedure Unlock;
    function Locked: Boolean;

    function GetSelTextRaw(AMaxSize: Integer = 0): AnsiString;
    function GetSelText: AnsiString;
    function GetSelTextShort: AnsiString;
    function GetSelTextW: UnicodeString;
    function GetSelTextShortW: UnicodeString;
    procedure DoSelectionChange;
    function GetMaxLengths(AIndex: TATBinHexMode): Integer;
    procedure SetMaxLengths(AIndex: TATBinHexMode; AValue: Integer);
    procedure SetMaxClipboardDataSizeMb(AValue: Integer);
    procedure SetEnabled2(AValue: Boolean);
    procedure SetMouseNiceScroll(AValue: Boolean);
    procedure DrawNiceScroll;
    property MouseNiceScroll: Boolean read FMouseNiceScroll write SetMouseNiceScroll;
    procedure ExitProc(Sender: TObject);
    procedure EncodingMenuItemClick(Sender: TObject);
    procedure SetFileUnicodeFmt(AValue: TATUnicodeFormat);
    procedure SetTextOemSpecial(AValue: Boolean);
    function CountLines(ABufSize: Integer): Boolean;
    function GetLineNumberOffset(ALine: Integer; AFindLine: Boolean; var ACurrentLine: Integer; var AOffset: Int64): Boolean;
    function FindLineNum(const AOffset: Int64): Integer;
    function StringAtPos(const APos: Int64): UnicodeString;

    {$ifdef SEARCH}
    procedure FindAll;
    function GetOnSearchProgress: TATStreamSearchProgress;
    procedure SetOnSearchProgress(AValue: TATStreamSearchProgress);
    function GetSearchResultStart: Int64;
    function GetSearchResultLength: Int64;
    function GetSearchString: string;
    {$endif}

    procedure DoOptionsChange;
    procedure DoOnScroll;
    procedure DoDrawLine(ACanvas: TCanvas; const AStr: UnicodeString; const APos: Int64;
      const ARect: TRect; const ATextPnt: TPoint; var ADone: Boolean);
    procedure DoDrawLine2(ACanvas: TCanvas; const AStr: UnicodeString;
      const APnt: TPoint; const AOptions: TATBinHexOutputOptions);
    procedure DoClickURL(const AMousePos: Int64);

  protected
    procedure Resize; override;
    procedure DblClick; override;
    procedure Paint; override;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMVScroll(var Message: TLMVScroll); message LM_VSCROLL;
    procedure WMHScroll(var Message: TLMHScroll); message LM_HSCROLL;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure SetEnabled(AValue: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //function Open(const AFileName: string; ARedraw: Boolean = True): Boolean;
    function OpenStream(AStream: TStream; ARedraw: Boolean = True): Boolean;
    procedure Reload;
    procedure Redraw(ARepaint: Boolean = True);

    {$ifdef SEARCH}
    function FindFirst(const AText: string; AOptions: TATStreamSearchOptions;
      const AFromPos: Int64 = -1): Boolean;
    function FindNext(AFindPrevious: Boolean = False): Boolean;
    property SearchResultStart: Int64 read GetSearchResultStart;
    property SearchResultLength: Int64 read GetSearchResultLength;
    property SearchStarted: Boolean read FSearchStarted;
    property SearchString: string read GetSearchString;
    {$endif}

    procedure IncreaseFontSize(AIncrement: Boolean);
    procedure CopyToClipboard(AAsHex: Boolean = False);
    property SelStart: Int64 read FSelStart;
    property SelLength: Int64 read FSelLength;
    property SelText: AnsiString read GetSelText;
    property SelTextShort: AnsiString read GetSelTextShort;
    property SelTextW: UnicodeString read GetSelTextW;
    property SelTextShortW: UnicodeString read GetSelTextShortW;
    procedure SetSelection(const AStart, ALength: Int64; AScroll: Boolean;
      AFireEvent: Boolean = True;
      ARedraw: Boolean = True);
    procedure Scroll(const APos: Int64; AIndentVert, AIndentHorz: Integer;
      ARedraw: Boolean = True);
    procedure SelectAll;
    procedure SelectNone(AFireEvent: Boolean = True);
    procedure DrawTo(
      ABitmap: TBitmap;
      APageWidth, APageHeight: Integer;
      AStringsObject: TObject;
      APrintMode: Boolean;
      const AFinalPos: Int64;
      var ATextWidth, ATextWidthHex, ATextWidthUHex: Integer;
      var AViewPageSize: Int64;
      var AViewAtEnd: Boolean);

    {$ifdef PRINT}
    function PrinterCaption: AnsiString;
    function PrinterFooter(APageNumber: Integer): UnicodeString;
    procedure PrintPreview;
    procedure PrintTo(
      ACanvas: TCanvas; //ACanvas may be assigned only for Print Preview
      APageWidth,
      APageHeight: Integer;
      APrintRange: TPrintRange;
      AFromPage,
      AToPage: Integer);
    procedure Print(
      APrintRange: TPrintRange;
      AFromPage: Integer = 1;
      AToPage: Integer = MaxInt;
      ACopies: Integer = 1;
      const ACaption: AnsiString = '');
    function MarginsRectPx(
      ATargetWidth,
      ATargetHeight: Integer;
      ATargetPPIX,
      ATargetPPIY: Integer): TRect;
    function MarginsRectRealPx: TRect;

    property MarginLeft: Double read FMarginLeft write FMarginLeft;
    property MarginTop: Double read FMarginTop write FMarginTop;
    property MarginRight: Double read FMarginRight write FMarginRight;
    property MarginBottom: Double read FMarginBottom write FMarginBottom;
    property PrintFooter: Boolean read FPrintFooter write FPrintFooter;
    {$endif}

    property PosPercent: Integer read GetPosPercent write SetPosPercent;
    property PosOffset: Int64 read GetPosOffset write SetPosOffset;
    property PosLine: Integer read GetPosLine write SetPosLine;
    property TextPopupCaption[AIndex: TATPopupCommand]: AnsiString read GetTextPopupCaption write SetTextPopupCaption;
    property MaxLengths[AIndex: TATBinHexMode]: Integer read GetMaxLengths write SetMaxLengths;
    property MaxClipboardDataSizeMb: Integer read FMaxClipboardDataSizeMb write SetMaxClipboardDataSizeMb;
    property FileName: string read FFileName;
    property FileSize: Int64 read FFileSize;
    property FileReadOK: Boolean read FFileOK;
    property FileUnicodeFormat: TATUnicodeFormat read FFileUnicodeFmt write SetFileUnicodeFmt;

    property TextEncoding: TEncConvId read FTextEncoding write SetTextEncoding;
    procedure TextEncodingsMenu(AX, AY: Integer);

    //Enabled2 is the same as Enabled, but also enables control redrawing:
    //we need to disable it during printing.
    property Enabled2: Boolean read FEnabled2 write SetEnabled2;
  published
    //inherited properties
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property Font;
    property ParentColor;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;

    property TextLineSpacing: integer read FLineSp write FLineSp default 0;
    property TextEnableSel: Boolean read FEnableSel write FEnableSel default True;
    property FontFooter: TFont read FFontFooter write SetFontFooter;
    property FontGutter: TFont read FFontGutter write SetFontGutter;
    property Mode: TATBinHexMode read FMode write SetMode default vbmodeText;
    property ResizeFollowTail: boolean read FResizeFollowTail write FResizeFollowTail default True;
    property TextWidth: Integer read FTextWidth write SetTextWidth default 80;
    property TextWidthHex: Integer read FTextWidthHex write SetTextWidthHex default 16;
    property TextWidthUHex: Integer read FTextWidthUHex write SetTextWidthUHex default 8;
    property TextWidthFit: Boolean read FTextWidthFit write SetTextWidthFit default False;
    property TextWidthFitHex: Boolean read FTextWidthFitHex write SetTextWidthFitHex default False;
    property TextWidthFitUHex: Boolean read FTextWidthFitUHex write SetTextWidthFitUHex default False;
    property TextWrap: Boolean read FTextWrap write SetTextWrap default False;
    property TextNonPrintable: Boolean read FTextNonPrintable write SetTextNonPrintable default False;
    property TextOemSpecial: Boolean read FTextOemSpecial write SetTextOemSpecial default False;
    property TextUrlHilight: Boolean read FUrlShow write SetTextUrlHilight default True;

    property TextGutter: Boolean read FTextGutter write SetTextGutter default False;
    property TextGutterLines: Boolean read FLinesShow write FLinesShow default True;
    property TextGutterLinesStep: Integer read FLinesStep write SetLinesStep default 5;
    property TextGutterLinesCount: Integer read FLinesCount write SetLinesCount stored False;
    property TextGutterLinesBufSize: Integer read FLinesBufSize write SetLinesBufSize stored False;
    property TextGutterLinesExtUse: Boolean read FLinesExtUse write FLinesExtUse default False;
    property TextGutterLinesExtList: AnsiString read FLinesExtList write FLinesExtList;

    property TextColorHex: TColor read FTextColorHex write FTextColorHex default clNavy;
    property TextColorHex2: TColor read FTextColorHex2 write FTextColorHex2 default clBlue;
    property TextColorHexBack: TColor read FTextColorHexBack write FTextColorHexBack default cATBinHexBkColor;
    property TextColorLines: TColor read FTextColorLines write FTextColorLines default clGray;
    property TextColorError: TColor read FTextColorError write FTextColorError default clRed;
    property TextColorGutter: TColor read FTextColorGutter write FTextColorGutter default clLtGray;
    property TextColorURL: TColor read FTextColorURL write FTextColorURL default clBlue;
    property TextColorHi: TColor read FTextColorHi write FTextColorHi default clYellow;

    property TextSearchIndentVert: Integer read FSearchIndentVert write SetSearchIndentVert default 5;
    property TextSearchIndentHorz: Integer read FSearchIndentHorz write SetSearchIndentHorz default 5;
    property TextTabSize: Integer read FTabSize write SetTabSize default 8;
    property TextPopupCommands: TATPopupCommands read FPopupCommands write FPopupCommands default cATBinHexCommandSet;

    {$ifdef NOTIF}
    property AutoReload: Boolean read FAutoReload write SetAutoReload default False;
    property AutoReloadBeep: Boolean read FAutoReloadBeep write FAutoReloadBeep default False;
    property AutoReloadFollowTail: Boolean read FAutoReloadFollowTail write FAutoReloadFollowTail default True;
    property AutoReloadSimple: Boolean read FAutoReloadSimple write FAutoReloadSimple default False;
    property AutoReloadSimpleTime: Integer read FAutoReloadSimpleTime write FAutoReloadSimpleTime default 1000;
    property OnFileReload: TNotifyEvent read FOnFileReload write FOnFileReload;
    {$endif}

    {$ifdef SEARCH}
    property OnSearchProgress: TATStreamSearchProgress read GetOnSearchProgress write SetOnSearchProgress;
    {$endif}

    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnOptionsChange: TNotifyEvent read FOnOptionsChange write FOnOptionsChange;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnDrawLine: TATBinHexDrawLine read FOnDrawLine write FOnDrawLine;
    property OnDrawLine2: TATBinHexDrawLine2 read FOnDrawLine2 write FOnDrawLine2;
    property OnClickURL: TATBinHexClickURL read FOnClickURL write FOnClickURL;
  end;

implementation

uses
  Forms,
  Types,
  Dialogs,
  LCLIntf,
  LCLType,
  {$ifdef PRINT} Printers, {$endif}
  {$ifdef PREVIEW} ATPrintPreview, ATxPrintProc, {$endif}
  {$ifdef TEST} TntStdCtrls, {$endif}
  {$ifdef TNT} TntClasses, {$endif}
  atbinhex_strproc,
  atbinhex_hexproc,
  atbinhex_clipboard,
  atbinhex_msg,
  atbinhex_stringproc,
  atbinhex_canvasproc;


{ Important constants: change with care }

const
  cMaxLengthDefault = 300; //Default value of "Maximal line length".
                   //See MaxLengths property description.
                   //Don't set too large value, it affects default file buffer size.

  cMaxLines = 150; //Maximal number of lines on screen supported.
                   //Don't set too large value because it affects file buffer size.
                   //Warning: It may be not enough for very high screen resolutions.
                   //150 should be enough for Height=1200, if we assume that minimal
                   //font height is 8.

{ Visual constants: may be changed freely }

const
  //what char-range to render as '?' char;
  //it's needed for Linux where some of these chars are rendered as 'rect with number'
  //and this breaks selection-highlight horz position
  cMaskHighFrom = $7F;
  cMaskHighTo = $FF;

  cReloadWithLMBPressed = False;  //User: allow auto-reload when LMouseBtn pressed
                                  //User: Regex for URL and email
  cReProt = '[a-z]{3,8}';
  cReUrl = '\b(' + cReProt + '://|www\.)[\w\d\.\-\?\#\+\{\}=~_$%;:&@,/]+';
  cReEmail = '\b[a-z\d\.\-_]+@[a-z\d\.\-]+\.[a-z]+\b';

  cCharSpecial = WideChar($B7);   //Draw: Char for control characters
                                  //Draw: Gutter:
  cGutterWidth = 10;              //  gutter min width (px)
  cGutterDotSize = 3;             //  gutter dot radius (px)
  cGutterIndent = 2;              //  gutter indent (px, right of line num)

                                  //User: Line numbers
  cLinesBufSizeMin = 1 * 1024;    //  buffer size: limits, default
  cLinesBufSizeDef = 300 * 1024;
  cLinesBufSizeMax = 20 * 1024 * 1024;
  cLinesCountMin = 10;            //  lines max count: limits, default
  cLinesCountDef = 2 * 1000;
  cLinesCountMax = 100 * 1000;
  cLinesStepMin = 1;              //  lines step limits
  cLinesStepMax = 10;
                                  //Draw: "Show non-printable" option:
  cCharNonPrintSpace    = #$B7;   //  - char for spaces
  cCharNonPrintSpaceOEM = #$FA;   //  - (for OEM font)
  cCharNonPrintTab      = #$BB;   //  - char for tabs
  cCharNonPrintTabOEM   = #$F9;   //  - (for OEM font)
  cCharNonPrintCR       = #$B6;   //  - char for CRs
  cCharNonPrintCROEM    = #$FE;   //  - (for OEM font)

  cDrawOffsetMinX = 2;            //Draw: Small offset (px) between text and left-top control corner
  cDrawOffsetMinY = 0;            //
  cDrawOffsetBelowY = 2;          //Draw: Small offset (0-2 px) subtracted from client height

                                  //Draw: Hex mode:
  cHexOffsetSep = ':';            //  separator between offset and digits (string, may be empty!)
  cHexLinesShow = True;           //  enable vertical lines
  cHexLinesWidth = 2;             //  width of vertical lines (px)
  cHexMaxDigits = 64;             //User: hex mode max width (for mouse selection)

                                  //Auto-scroll feature (mouse is out of control area):
  cMouseAutoScrollTime = 50;      //  - timer interval (ms)
  cMouseAutoScrollSpeedX = 20;    //  - horz. speed (px/tick)
  cMouseAutoScrollSpeedY = 1;     //  - vert. speed (lines/tick)

                                  //Nice scroll feature (middle mouse click):
  cMouseNiceScroll = True;        //  - enabled
  cMouseNiceScrollTime = 100;     //  - timer interval (ms)
  cMouseNiceScrollSpeedX = 8;     //  - horz. minimal speed (px/tick)
  cMouseNiceScrollSpeedY = 1;     //  - vert. minimal speed (lines/tick)

  cTabSizeMax = 16;               //User: Tabulation size limits (chars)
  cTabSizeMin = 2;
  cArrowScrollSize = 200;         //User: Keyboard scroll size (px) for Left/Right keys
  cSelectionByDoubleClick = True; //User: Feature: Double click selects current word
  cSelectionByTripleClick = True; //User: Feature: Triple click selects current line
  cSelectionByShiftClick = True;  //User: Feature: Click marks selection start, Shift+Click marks selection end
  cSelectionRightIndent = 8;      //User: Minimal space (px) before selection start and control right border
  cMaxShortLength = 256;          //User: Maximal length of string_ for SelTextShort/SelTextShortW properties
  cMaxClipboardDataSizeMb = 16;   //User: Maximal data size (Mb) for copying to Clipboard
  cMaxClipboardDataSizeMbMin = 8;      // (default, minimal, maximal)
  cMaxClipboardDataSizeMbMax = 256;
  cMaxFontSize = 72;              //User: Maximal font size for IncreaseFontSize method
  cMaxSearchIndent = 80;          //User: Maximal vert/horz search indent (avg. chars)
  cEncMenuOffsetY = 20;           //User: Offset of encodings menu above control center (px)

                                  //Draw: Colors:
  cColorDisabled = clGrayText;    //  text color for disabled state
  cColorPrintBack = clWhite;      //  grayscale colors for printing
  cColorPrintBackHex = clWhite;
  cColorPrintText = clBlack;
  cColorPrintTextHex1 = clBlack;
  cColorPrintTextHex2 = clBlack;
  cColorPrintLines = clGray;
  cColorPrintError = clBlack;
  cColorPrintURL = clGray;


{ Resources constants: don't change }

var
  FBitmapNiceScroll: TPortableNetworkGraphic = nil; //NiceScroll mode: bitmap that is drawn when mode is on

const
  cBitmapNiceScrollRadius = 16;     //NiceScroll mode: bitmap is actually a circle of specified radius

const
  crNiceScrollNone  = TCursor(-30); //NiceScroll mode: cursor IDs
  crNiceScrollUp    = TCursor(-31);
  crNiceScrollDown  = TCursor(-32);
  crNiceScrollLeft  = TCursor(-33);
  crNiceScrollRight = TCursor(-34);


procedure BitmapResize(b: TBitmap; W, H: integer);
begin
  {$ifdef fpc}
  b.SetSize(W, H);
  b.FreeImage; //recommended, otherwise black bitmap on big size
  {$else}
  b.Width:= W;
  b.Height:= H;
  {$endif}
end;

procedure BitmapResizeBySteps(b: TBitmap; W, H, StepW, StepH: integer);
var
  SizeX, SizeY: integer;
begin
  SizeX:= (W div StepW + 1)*StepW;
  SizeY:= (H div StepH + 1)*StepH;
  if (SizeX>b.Width) or
    (SizeY>b.Height) then
    BitmapResize(b, SizeX, SizeY);
end;

{ Debug form }

{$ifdef TEST}
var
  FDebugForm: TForm = nil;
  FDebugLabel1: TTntLabel = nil;
  FDebugLabel2: TTntLabel = nil;

procedure MsgDebug(const S1, S2: UnicodeString);
begin
  if Assigned(FDebugLabel1) and Assigned(FDebugLabel2) then
  begin
    FDebugLabel1.Caption := S1;
    FDebugLabel2.Caption := S2;
  end;
end;

function MsgDebugStr(const S: UnicodeString; Pos: Integer): UnicodeString;
begin
  Result := S;
  if Pos > 0 then
    Insert('>', Result, Pos);
end;

procedure InitDebugForm;
begin
  FDebugForm := TForm.Create(nil);
  with FDebugForm do
  begin
    Left := 0;
    Top := 0;
    Width := Screen.Width;
    ClientHeight := 25;
    Caption := 'Debug';
    BorderStyle := bsToolWindow;
    BorderIcons := [];
    FormStyle := fsStayOnTop;
    Font.Name := 'Tahoma';
    Font.Size := 8;
    Color := clWhite;
    Enabled := False;
    Show;
  end;

  FDebugLabel1 := TTntLabel.Create(FDebugForm);
  with FDebugLabel1 do
  begin
    Parent := FDebugForm;
    Left := 4;
    Top := 4;
  end;

  FDebugLabel2 := TTntLabel.Create(FDebugForm);
  with FDebugLabel2 do
  begin
    Parent := FDebugForm;
    Left := 4;
    Top := 18;
  end;

  MsgDebug('', '');
end;

procedure FreeDebugForm;
begin
  FDebugLabel1.Free;
  FDebugLabel2.Free;
  FDebugForm.Free;
end;
{$endif}


{ Helper functions }

procedure SwapInt64(var N1, N2: Int64);
var
  N: Int64;
begin
  N := N1;
  N1 := N2;
  N2 := N;
end;

procedure HiRect(ACanvas: TCanvas; const ARect: TRect);
begin
  ACanvas.Pen.Color := clRed;
  ACanvas.MoveTo(ARect.Left, ARect.Bottom - 2);
  ACanvas.LineTo(ARect.Right, ARect.Bottom - 2);
end;

function SConvertForOut(
  const S: UnicodeString;
  const AOptions: TATBinHexOutputOptions): UnicodeString;
var
  chSp,
  chTab,
  chCR: WideChar;
  TabOptions: TStringTabOptions;
  i: integer;
begin
  Result := S;

  chSp := cCharNonPrintSpace;
  chTab := cCharNonPrintTab;
  chCR := cCharNonPrintCR;

  if AOptions.ShowNonPrintable then
    for i := 1 to Length(Result) do
      if Result[i]=' ' then
        Result[i] := chSp;

  if not AOptions.ShowNonPrintable then
    for i := 1 to Length(Result) do
      if (Ord(Result[i])>=cMaskHighFrom) and (Ord(Result[i])<=cMaskHighTo) then
        Result[i] := '?';

  TabOptions.TabSize := AOptions.TabSize;
  TabOptions.TabPosition := 0;
  TabOptions.FontMonospaced := AOptions.IsFontFixed;
  TabOptions.NonPrintableShow := AOptions.ShowNonPrintable;
  TabOptions.NonPrintableChar := chTab;
  SReplaceTabsW(Result, TabOptions);

  if AOptions.ShowNonPrintable and AOptions.ShowCR then
    Result := Result + chCR;
end;

{$ifdef REGEX}
function RegExReplaceToChar(const Str, Re: string; ch: Char; ACaseSens: Boolean): string;
var
  RegEx: TDIRegEx;
  N_prev, N, i: Integer;
begin
  Result := Str;
  if (Str = '') or (Re = '') then Exit;

  RegEx := TDIPerlRegEx.Create(nil);
  try
    if ACaseSens then
      RegEx.CompileOptions := RegEx.CompileOptions - [coCaseLess]
    else
      RegEx.CompileOptions := RegEx.CompileOptions + [coCaseLess];
    RegEx.MatchPattern := Re;
    N_prev := -1;
    repeat
      RegEx.SetSubjectStr(Result);
      if RegEx.Match(0) < 0 then Break;
      N := RegEx.MatchedStrFirstCharPos + 1;
      if N = N_prev then Break;
      N_prev := N;
      for i := N to (N + RegEx.MatchedStrLength - 1) do
        Result[i] := ch;
    until False;
  finally
    RegEx.Free;
  end;
end;
{$endif}

procedure StringOut(
  ACanvas: TCanvas;
  AX, AY: Integer;
  const AStr: UnicodeString;
  const AOptions: TATBinHexOutputOptions);
var
  S: UnicodeString;
begin
  {$ifdef REGEX}
  if AStr <> '' then
  begin
    if TextRepFrom1 <> '' then
      AStr := RegExReplaceToChar(AStr, TextRepFrom1, TextRepTo1, False);
    if TextRepFrom2 <> '' then
      AStr := RegExReplaceToChar(AStr, TextRepFrom2, TextRepTo2, False);
    if TextRepFrom3 <> '' then
      AStr := RegExReplaceToChar(AStr, TextRepFrom3, TextRepTo3, False);
  end;
  {$endif}

  S:= SConvertForOut(AStr, AOptions);
  CanvasTextOut(ACanvas, AX, AY, S, AOptions.TabSize, AOptions.TextSize);
end;

function StringWidth(
  ACanvas: TCanvas;
  const AStr: UnicodeString;
  const AOptions: TATBinHexOutputOptions): Integer;
var
  S: UnicodeString;
begin
  S:= SConvertForOut(AStr, AOptions);
  Result := CanvasTextWidth(ACanvas, S, AOptions.TabSize, AOptions.TextSize);
end;

procedure DebugExtent(const AStr: UnicodeString; const A: TATStringExtent);
var
  i: integer;
  s: string;
begin
  s:= '';
  for i:= 0 to 10 do
    s:= s+inttostr(A[i])+' ';
  ShowMessage('Extent'#13+AStr+#13+s);
end;

function StringExtent(
  ACanvas: TCanvas;
  const AStr: UnicodeString;
  out AExtent: TATStringExtent;
  const AOptions: TATBinHexOutputOptions): Boolean;
var
  List: array of integer;
  i: Integer;
begin
  Result:= true;
  FillChar(AExtent, SizeOf(AExtent), 0);
  if AStr='' then Exit;

  SetLength(List, Length(AStr));
  SCalcCharOffsets(AStr, List, AOptions.TabSize);

  for i:= 0 to Length(List)-1 do
    AExtent[i+1]:= List[i]*AOptions.TextSize.X div 100;
end;

function IsSeparator(ch: WideChar): Boolean;
begin
  Result := (ch = ' ') or (ch = #9) or (ch = '\');
end;

function StringWrapPosition(
  const S: UnicodeString;
  AMaxLen: Integer): Integer;
var
  i: Integer;
begin
  for i := Min(AMaxLen + 1, Length(S)) downto 1 do
    if IsSeparator(S[i]) then
      begin Result := i; Exit end;
  Result := AMaxLen;
end;

function BoolToSign(AValue: Boolean): Integer;
const
  N: array[Boolean] of Integer = (-1, 1);
begin
  Result := N[AValue];
end;


{ TStrPositions }

type
  TStrPosRecord = record
    Str: UnicodeString;
    Pnt: TPoint;
    Pos: Int64;
  end;
  TStrPosArray = array[1 .. cMaxLines] of TStrPosRecord;
  TStrPosHex = array[0 .. Pred(cHexMaxDigits)] of Integer;

  TStrPositions = class(TObject)
  private
    FNum: Integer;
    FArray: TStrPosArray;
    FHex: TStrPosHex;
    FHexNum: Integer;
    FHexLen: Integer;
    FHexMargin: Integer;
    FCharSize: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear(ACharSize: Integer = 1);
    procedure Add(const AStr: UnicodeString; AX, AY: Integer; const APos: Int64);
    function GetPosFromCoord(ACanvas: TCanvas; AX, AY: Integer; const AOptions: TATBinHexOutputOptions; AStrict: Boolean = False): Int64;
    function GetCoordFromPos(ACanvas: TCanvas; const APos: Int64; const AOptions: TATBinHexOutputOptions; var AX, AY: Integer): Boolean;
    function GetScreenWidth(ACanvas: TCanvas; const AOptions: TATBinHexOutputOptions): Integer;
    procedure AddHex(APos, AX, ANum, ALen: Integer);
    procedure AddHexMargin(AX: Integer);
  end;

procedure TStrPositions.AddHex(APos, AX, ANum, ALen: Integer);
begin
  FHexNum := Min(ANum, cHexMaxDigits);
  FHexLen := ALen;
  if (APos >= Low(FHex)) and (APos <= High(FHex)) then
    FHex[APos] := AX;
end;

procedure TStrPositions.AddHexMargin(AX: Integer);
begin
  FHexMargin := AX;
end;

constructor TStrPositions.Create;
begin
  inherited Create;
  FillChar(FArray, SizeOf(FArray), 0);
  FillChar(FHex, SizeOf(FHex), 0);
  FNum := 0;
  FHexNum := 0;
  FHexLen := 0;
  FHexMargin := 0;
  FCharSize := 1;
end;

destructor TStrPositions.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TStrPositions.Clear(ACharSize: Integer = 1);
var
  i: Integer;
begin
  for i := FNum downto 1 do
    with FArray[i] do
    begin
      Str := '';
      Pnt := Point(0, 0);
      Pos := 0;
    end;
  FNum := 0;
  FHexNum := 0;
  FHexLen := 0;
  FHexMargin := 0;
  FillChar(FHex, SizeOf(FHex), 0);
  FCharSize := ACharSize;
end;

procedure TStrPositions.Add(const AStr: UnicodeString; AX, AY: Integer; const APos: Int64);
begin
  if FNum < High(TStrPosArray) then
  begin
    Inc(FNum);
    with FArray[FNum] do
      begin
      Str := AStr;
      Pnt := Point(AX, AY);
      Pos := APos;
      end;
  end;
end;

function TStrPositions.GetPosFromCoord(
  ACanvas: TCanvas;
  AX, AY: Integer;
  const AOptions: TATBinHexOutputOptions;
  AStrict: Boolean = False): Int64;
var
  YH: Integer;
  Num, i: Integer;
  Dx: TATStringExtent;
begin
  Result := -1;
  if FNum = 0 then Exit;

  {$ifdef TEST} MsgDebug('', ''); {$endif}

  //Mouse upper than first line
  with FArray[1] do
    if AY < Pnt.Y then
    begin
      {$ifdef TEST} MsgDebug('Upper than first line', ''); {$endif}
      if not AStrict then
        Result := Pos;
      Exit
    end;

  //Get line number into Num
  YH := AOptions.TextSize.Y;
  Num := 0;
  for i := 1 to FNum do
    with FArray[i] do
      if (AY >= Pnt.Y) and (AY < Pnt.Y + YH) then
      begin
        Num := i;
        Break
      end;

  //Mouse lower than last line
  if Num = 0 then
    with FArray[FNum] do
    begin
      {$ifdef TEST} MsgDebug('Lower than last line', ''); {$endif}
      if not AStrict then
        Result := Pos + Length(Str) * FCharSize;
      Exit
    end;

  //Mouse over a line #Num
  with FArray[Num] do
  begin
    //Mouse lefter than line
    if AX <= Pnt.X then
    begin
      {$ifdef TEST} MsgDebug(Format('Lefter than line %d', [Num]), MsgDebugStr(Str, 1)); {$endif}

      //Handle hex offsets
      if (FHexNum > 0) and (FHexMargin > 0) and (AX <= FHexMargin) then
      begin
        for i := Pred(FHexNum) downto 0 do
          if (FHex[i] > 0) and (AX >= FHex[i] + FHexLen div 2) then
          begin
            Result := Pos + (i + 1) * FCharSize;
            Exit
          end;
      end;

      if not AStrict then
        Result := Pos;
      Exit
    end;

    if StringExtent(ACanvas, Str, Dx, AOptions) then
    begin
      //Mouse inside line
      for i := 1 to Length(Str) do
      begin
        if (AX < Pnt.X + (Dx[i - 1] + Dx[i]) div 2) then
        begin
          {$ifdef TEST} MsgDebug(Format('Line %d, Char %d', [Num, i]), MsgDebugStr(Str, i)); {$endif}
          Result := Pos + (i - 1) * FCharSize;
          Exit
        end;
      end;

      //Mouse righter than line
      {$ifdef TEST} MsgDebug(Format('Righer than line %d', [Num]), ''); {$endif}
      if not AStrict then
        Result := Pos + Length(Str) * FCharSize;
    end;
  end;
end;

function TStrPositions.GetCoordFromPos(ACanvas: TCanvas; const APos: Int64; const AOptions: TATBinHexOutputOptions; var AX, AY: Integer): Boolean;
var
  i: Integer;
  Dx: TATStringExtent;
begin
  Result := False;

  AX := 0;
  AY := 0;

  for i := 1 to FNum do
    with FArray[i] do
      if (APos >= Pos) and (APos < Pos + Length(Str) * FCharSize) then
        if StringExtent(ACanvas, Str, Dx, AOptions) then
        begin
          Result := True;
          AX := Pnt.X + Dx[(APos - Pos) div FCharSize];
          AY := Pnt.Y;
          Break
        end;

  {
  //Debug
  if not Result then
  begin
    S := '';
    for i := 1 to FNum do
      with FArray[i] do
        S := S + Format('%d:  Pos: %d', [i, Pos]) + #13;
    S := S + #13 + Format('APos: %d', [APos]);
    MsgError(S);
  end;
  }
end;


function TStrPositions.GetScreenWidth(ACanvas: TCanvas; const AOptions: TATBinHexOutputOptions): Integer;
var
  i: Integer;
  AWidth: Integer;
begin
  Result := 0;
  for i := 1 to FNum do
    with FArray[i] do
    begin
      AWidth := Pnt.X + StringWidth(ACanvas, Str, AOptions);
      ILimitMin(Result, AWidth);
    end;
end;


{ TATBinHex }

procedure TATBinHex.AllocBuffer;
begin
  FMaxLength := FMaxLengths[FMode];

  //Buffer contains 3 offsets: offset below + 2 offsets above view position
  FBufferMaxOffset := FMaxLength * cMaxLines * CharSize;
  FBufferAllocSize := 3 * FBufferMaxOffset;

  GetMem(FBuffer, FBufferAllocSize);
  FillChar(FBuffer^, FBufferAllocSize, 0);
end;


constructor TATBinHex.Create(AOwner: TComponent);
var
  N: TATBinHexMode;
begin
  inherited Create(AOwner);

  FBitmap := TBitmap.Create;

  //Init inherited properties
  Caption := '';
  Width := 250;
  Height := 150;
  BorderStyle := bsSingle;
  Color := clWindow;
  Cursor := crIBeam;
  ControlStyle := ControlStyle + [csOpaque];

  Font.Name := 'Courier New';
  Font.Size := 9;
  Font.Color := clBlack;

  //Init fields
  FMode := vbmodeText;
  FTextWidth := 80;
  FTextWidthHex := 16;
  FTextWidthUHex := 8;
  FTextWidthFit := False;
  FTextWidthFitHex := False;
  FTextWidthFitUHex := False;
  FTextWrap := False;
  FTextNonPrintable := False;
  FTextOemSpecial := False;
  FTextEncoding := eidCP1252;

  FTextGutter := False;
  FTextGutterWidth := cGutterWidth;
  FLinesShow := True;
  FLinesStep := 5;
  FLinesBufSize := cLinesBufSizeDef;
  FLinesData := nil;
  FLinesNum := 0;
  FLinesCount := cLinesCountDef;
  FLinesExtUse := False;
  FLinesExtList := '';

  FTextColorHex := clNavy;
  FTextColorHex2 := clBlue;
  FTextColorHexBack := cATBinHexBkColor;
  FTextColorLines := clGray;
  FTextColorError := clRed;
  FTextColorGutter := clLtGray;
  FTextColorURL := clBlue;
  FTextColorHi := clYellow;
  FSearchIndentVert := 5;
  FSearchIndentHorz := 5;
  FTabSize := 8;
  FPopupCommands := cATBinHexCommandSet;
  FEnabled2 := True;
  FEnableSel := True;
  FResizeFollowTail := True;

  FAutoReload := False;
  FAutoReloadBeep := False;
  FAutoReloadFollowTail := True;
  FAutoReloadSimple := False;
  FAutoReloadSimpleTime := 1000;

  FUrlShow := True;
  InitURLs;

  FMaxLength := 0; //Initialized in AllocBuffer
  for N := Low(TATBinHexMode) to High(TATBinHexMode) do
    FMaxLengths[N] := cMaxLengthDefault;

  FMaxClipboardDataSizeMb := cMaxClipboardDataSizeMb;
  FHexOffsetLen := 8;
  FFontHeight := 8;
  FFontFirstChar := Chr($20);
  FFontWidthDigits := 4;
  FFontMonospaced := False;
  FLockCount := 0;
  FLineSp := 0;

  {$ifdef PRINT}
  FMarginLeft := 10;
  FMarginTop := 10;
  FMarginRight := 10;
  FMarginBottom := 10;
  FPrintFooter := True;
  {$endif}

  FOnSelectionChange := nil;
  FOnOptionsChange := nil;
  FOnScroll := nil;

  {$ifdef NOTIF}
  FOnFileReload := nil;
  {$endif}

  FFileName := '';
  FStream := nil;
  InitData;

  //Init objects

  {$ifdef SEARCH}
  FSearch := TATStreamSearch.Create(Self);
  FSearch2 := TATStreamSearch.Create(Self);
  FSearchStarted := False;
  {$endif}

  FFontFooter := TFont.Create;
  with FFontFooter do
  begin
    Name := 'Arial';
    Size := 9;
    Color := clBlack;
  end;

  FFontGutter := TFont.Create;
  with FFontGutter do
  begin
    Name := 'Courier New';
    Size := 9;
    Color := clBlack;
  end;

  FTimerAutoScroll := TTimer.Create(Self);
  with FTimerAutoScroll do
  begin
    Enabled := False;
    Interval := cMouseAutoScrollTime;
    OnTimer := TimerAutoScrollTimer;
  end;

  FTimerNiceScroll := TTimer.Create(Self);
  with FTimerNiceScroll do
  begin
    Enabled := False;
    Interval := cMouseNiceScrollTime;
    OnTimer := TimerNiceScrollTimer;
  end;

  FStrings := TStrPositions.Create;

  //Init popup menu
  FMenuItemCopy := TMenuItem.Create(Self);
  with FMenuItemCopy do
  begin
    Caption := 'Copy';
    OnClick := MenuItemCopyClick;
  end;

  FMenuItemCopyHex := TMenuItem.Create(Self);
  with FMenuItemCopyHex do
  begin
    Caption := 'Copy as hex';
    OnClick := MenuItemCopyHexClick;
  end;

  FMenuItemCopyLink := TMenuItem.Create(Self);
  with FMenuItemCopyLink do
  begin
    Caption := 'Copy link';
    OnClick := MenuItemCopyLinkClick;
  end;

  FMenuItemSelectLine := TMenuItem.Create(Self);
  with FMenuItemSelectLine do
  begin
    Caption := 'Select line';
    OnClick := MenuItemSelectLineClick;
  end;

  FMenuItemSelectAll := TMenuItem.Create(Self);
  with FMenuItemSelectAll do
  begin
    Caption := 'Select all';
    OnClick := MenuItemSelectAllClick;
  end;

  FMenuItemEncMenu:= TMenuItem.Create(Self);
  with FMenuItemEncMenu do
  begin
    Caption := 'Encoding';
    UpdateMenuEncodings(FMenuItemEncMenu);
    //OnClick := MenuItemEncMenuClick;
  end;

  FMenuItemSep1 := TMenuItem.Create(Self);
  with FMenuItemSep1 do
  begin
    Caption := '-';
  end;

  FMenuItemSep2 := TMenuItem.Create(Self);
  with FMenuItemSep2 do
  begin
    Caption := '-';
  end;

  FMenu := TPopupMenu.Create(Self);
  with FMenu do
  begin
    Items.Add(FMenuItemCopy);
    Items.Add(FMenuItemCopyHex);
    Items.Add(FMenuItemCopyLink);
    Items.Add(FMenuItemSep1);
    Items.Add(FMenuItemSelectLine);
    Items.Add(FMenuItemSelectAll);
    Items.Add(FMenuItemSep2);
    Items.Add(FMenuItemEncMenu);
    OnPopup := UpdateMenu;
  end;

  FMenuEnc := nil;
  PopupMenu := FMenu;

  //Init notification objects
  {$ifdef NOTIF}
  FNotif := TATFileNotification.Create(Self);
  with FNotif do
  begin
    Options := [foNotifyFilename, foNotifyLastWrite, foNotifySize];
    OnChanged := NotifChanged;
  end;

  FNotif2 := TATFileNotificationSimple.Create(Self);
  with FNotif2 do
  begin
    OnChanged := NotifChanged;
  end;
  {$endif}

  //Init event handlers
  OnMouseWheelUp := MouseWheelUp;
  OnMouseWheelDown := MouseWheelDown;
  OnContextPopup := ContextPopup;
  OnExit := ExitProc;

  //Init debug form
  {$ifdef TEST}
  InitDebugForm;
  {$endif}
end;

destructor TATBinHex.Destroy;
begin
  {$ifdef TEST}
  FreeDebugForm;
  {$endif}

  FreeData;
  FStrings.Free;
  FBitmap.Free;
  FFontFooter.Free;
  FFontGutter.Free;

  inherited Destroy;
end;


procedure TATBinHex.DrawGutterTo(ABitmap: TBitmap);
begin
  Assert(Assigned(ABitmap), 'Bitmap not assigned');

  with ABitmap do
    if FTextGutter then
    begin
      FTextGutterWidth := cGutterWidth;
      if ActiveLinesShow then
      begin
        Canvas.Font.Assign(FFontGutter);
        ILimitMin(FTextGutterWidth, Canvas.TextWidth(IntToStr(FLinesNum + 1)) + cGutterIndent);
      end;

      Canvas.Brush.Color := FTextColorGutter;
      Canvas.FillRect(Rect(0, 0, FTextGutterWidth, Height));
    end;
end;

procedure TATBinHex.DrawEmptyTo(
  ABitmap: TBitmap;
  APageWidth,
  APageHeight: Integer;
  APrintMode: Boolean);
var
  AColorBack: TColor;
begin
  Assert(Assigned(ABitmap), 'Bitmap not assigned');

  if APrintMode then
    AColorBack := cColorPrintBack
  else
    AColorBack := Color;

  with ABitmap do
  begin
    Canvas.Brush.Color := AColorBack;
    Canvas.FillRect(Rect(0, 0, Width, Height));
    DrawGutterTo(ABitmap);
    Canvas.Brush.Color := AColorBack;
  end;
end;


function TATBinHex.OutputOptions(AShowCR: Boolean = False): TATBinHexOutputOptions;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.ShowNonPrintable := FTextNonPrintable;
  Result.ShowCR := AShowCR;
  Result.IsFontFixed := FFontMonospaced;
  Result.TabSize := FTabSize;
  Result.TextSize := FTextSize;
end;

procedure TATBinHex.DrawTo(
  ABitmap: TBitmap;
  APageWidth, APageHeight: Integer;
  AStringsObject: TObject;
  APrintMode: Boolean;
  const AFinalPos: Int64;
  var ATextWidth, ATextWidthHex, ATextWidthUHex: Integer;
  var AViewPageSize: Int64;
  var AViewAtEnd: Boolean);
var
  Dx: TATStringExtent; //TATStringExtent is huge, so this isn't SelectLine local.
                       //Otherwise it slows down on Win9x.
  AColorBack,          //Actual colors used for drawing
  AColorBackHex,
  AColorText,
  AColorTextHex1,
  AColorTextHex2,
  AColorLines,
  AColorError,
  AColorURL: TColor;

  //hilight URLs and "Find all" results
  procedure HilightLine(
    const ALine: UnicodeString; AX, AY: Integer;
    const AFilePos: Int64);
  var
    nStart, nEnd: Int64;
    i: Integer;
  begin
    if StringExtent(FBitmap.Canvas, ALine, Dx, OutputOptions) then
    begin
      for i := Low(FUrlArray) to High(FUrlArray) do
        with FUrlArray[i] do
        begin
          if (FString = '') then Break;
          if ((FPos - AFilePos) div CharSize <= Length(ALine)) and
            ((FPos - AFilePos) div CharSize + Length(FString) >= 0) then
          begin
            nStart := (FPos - AFilePos) div CharSize;
            I64LimitMin(nStart, 0);

            nEnd := (FPos - AFilePos) div CharSize + Length(FString);
            I64LimitMax(nEnd, Length(ALine));

            {
            FBitmap.Canvas.Pen.Color := clRed;
            FBitmap.Canvas.Brush.Style := bsClear;
            FBitmap.Canvas.Rectangle(Rect(
              AX + Dx[nStart], AY, AX + Dx[nEnd], AY + FFontHeight));
            }
            FBitmap.Canvas.Font.Color := AColorURL;
            FBitmap.Canvas.Font.Style := ActiveFont.Style + [fsUnderline];
            StringOut(FBitmap.Canvas,
              AX + Dx[nStart], AY,
              Copy(ALine, nStart + 1, nEnd - nStart),
              OutputOptions);
            FBitmap.Canvas.Font.Color := AColorText;
            FBitmap.Canvas.Font.Style := ActiveFont.Style;
          end;
        end;

      for i := Low(FFindArray) to High(FFindArray) do
        with FFindArray[i] do
        begin
          if (FLen = 0) then Break;
          if ((FPos - AFilePos) div CharSize <= Length(ALine)) and
            ((FPos - AFilePos) div CharSize + FLen >= 0) then
          begin
            nStart := (FPos - AFilePos) div CharSize;
            I64LimitMin(nStart, 0);

            nEnd := (FPos - AFilePos) div CharSize + FLen;
            I64LimitMax(nEnd, Length(ALine));

            FBitmap.Canvas.Brush.Color := FTextColorHi;
            StringOut(FBitmap.Canvas,
              AX + Dx[nStart], AY,
              Copy(ALine, nStart + 1, nEnd - nStart),
              OutputOptions);
            FBitmap.Canvas.Brush.Color := Color;
          end;
        end;
    end;
  end;

  procedure SelectLine(
    const ALine: UnicodeString; AX, AY: Integer;
    const AFilePos: Int64;
    ASelectAll: Boolean = False; AHilight: Boolean = False);
  var
    Len, YHeight: Integer;
    nStart, nEnd: Int64;
    InvRect: TRect;
  begin
    if AHilight then
      HilightLine(ALine, AX, AY, AFilePos);

    if ASelectAll then
      Len := 1
    else
      Len := Length(ALine);

    if (FSelStart > AFilePos + (Len - 1) * CharSize) or
      (FSelStart + FSelLength - 1 * CharSize < AFilePos) then Exit;

    if (AX >= FBitmap.Width) or (AY >= FBitmap.Height) then Exit;

    YHeight := FFontHeight;

    if StringExtent(FBitmap.Canvas, ALine, Dx, OutputOptions) then
    begin
      if ASelectAll then
        InvRect:= Rect(AX, AY, AX + Dx[Length(ALine)], AY + YHeight)
      else
      begin
        nStart := (FSelStart - AFilePos) div CharSize;
        I64LimitMin(nStart, 0);

        nEnd:= (FSelStart + FSelLength - AFilePos) div CharSize;
        I64LimitMax(nEnd, Length(ALine));

        InvRect:= Rect(AX + Dx[nStart], AY, AX + Dx[nEnd], AY + YHeight);
      end;

      CanvasInvertRect(FBitmap.Canvas, InvRect, clBlack);
      Invalidate;
    end;
  end;

  function ActiveColor(AColor: TColor): TColor;
  begin
    if Enabled then
      Result := AColor
    else
      Result := cColorDisabled;
  end;

var
  AStrings: TStrPositions;
  X, Y, Y2: Integer;
  APos, APosEnd, ACurrentPos: Int64;
  LineA: AnsiString;
  LineW, LineText: UnicodeString;
  APosTextX, APosTextY: Integer;
  ALines, ACols: Integer;
  ALineNum: Integer;
  i, j: Integer;
  WithCR, WithDot: Boolean;
  PosOk, ADone: Boolean;
  Size: TSize;
begin
  PosOk :=
    (FBufferPos >= 0) and
    (FBufferPos <= PosLast) and
    (FViewPos >= FBufferPos) and
    (FViewPos <= FBufferPos + 2 * FBufferMaxOffset);

  Assert(PosOk,
    Format('Positions out of range: DrawTo'#13+
    'BufferPos: %d, ViewPos: %d, BufMaxOffset: %d',
    [FBufferPos, FViewPos, FBufferMaxOffset]));
  if not PosOk then Exit;

  ATextWidth := FTextWidth;
  ATextWidthHex := FTextWidthHex;
  ATextWidthUHex := FTextWidthUHex;
  AViewPageSize := 0;
  AViewAtEnd := False;

  if APrintMode then
  begin
    AColorBack := cColorPrintBack;
    AColorBackHex := cColorPrintBackHex;
    AColorText := cColorPrintText;
    AColorTextHex1 := cColorPrintTextHex1;
    AColorTextHex2 := cColorPrintTextHex2;
    AColorLines := cColorPrintLines;
    AColorError := cColorPrintError;
    AColorURL := cColorPrintURL;
  end
  else
  begin
    AColorBack := Color;
    AColorBackHex := FTextColorHexBack;
    AColorText := ActiveColor(ActiveFont.Color);
    AColorTextHex1 := ActiveColor(FTextColorHex);
    AColorTextHex2 := ActiveColor(FTextColorHex2);
    AColorLines := ActiveColor(FTextColorLines);
    AColorError := ActiveColor(FTextColorError);
    AColorURL := ActiveColor(FTextColorURL);
  end;

  DrawEmptyTo(ABitmap, APageWidth, APageHeight, APrintMode);

  AStrings := TStrPositions(AStringsObject);
  if Assigned(AStrings) then
    AStrings.Clear(CharSize);

  with ABitmap do
  begin
    Canvas.Font := ActiveFont;
    Canvas.Font.Color := AColorText;

    Size:= Canvas.TextExtent('N');
    FTextSize.X:= Size.cx;
    FTextSize.Y:= Size.cy;

    FFontHeight := FTextSize.Y;
    FFontWidthDigits := FTextSize.X;
    FFontFirstChar := ' ';
    FFontMonospaced := true;
    Inc(FFontHeight, FLineSp);

    if FTextWidthFit then SetTextWidthTo(ColsNumFit(ABitmap), ATextWidth);
    if FTextWidthFitHex then SetTextWidthHexTo(ColsNumHexFit(ABitmap), ATextWidthHex);
    if FTextWidthFitUHex then SetTextWidthUHexTo(ColsNumUHexFit(ABitmap), ATextWidthUHex);

    //Calculate fixed page size. In Text/Unicode modes it will be recalculated
    //and will contain variable page size.
    ALines := LinesNum(ABitmap);
    ACols := ColsNum(ABitmap);
    AViewPageSize := ALines * ACols;

    if FFileOK then
    begin
      case FMode of
        vbmodeText,
        vbmodeUnicode:
          begin
            APos := FViewPos;
            for i := 1 to Min(ALines + 1, cMaxLines) do
            begin
              //Find line
              APosEnd := FindLinePos(APos, vdirDown, LineW);
              WithCR := LineWithCR(APos, LineW);
              WithDot := LineWithGutterDot(APos);

              //Draw line
              LineText := LineW;
              APosTextX := DrawOffsetX;
              APosTextY := DrawOffsetY + (i - 1) * FFontHeight;
              ADone := False;

              if not APrintMode then
              begin
                Canvas.Font.Color := AColorText;
                Canvas.Brush.Color := AColorBack;
                DoDrawLine(Canvas, LineText, APos,
                  Rect(0, APosTextY, ClientWidth, APosTextY + FFontHeight),
                  Point(APosTextX, APosTextY),
                  ADone);
              end;

              if not ADone then
              begin
                {
                //(not finished)
                //To not output BOM ($FFFE, $FEFF) characters:
                if IsModeUnicode then
                  if (APos = 0) and (LineText <> '') then
                    if (LineText[1] = #$FEFF) or (LineText[1] = #$FFFE) then
                    begin
                      Delete(LineText, 1, 1);
                      APos := 2;
                    end;
                    }

                StringOut(Canvas, APosTextX - FHViewPos, APosTextY, LineText, OutputOptions(WithCR));
                SelectLine(LineText, APosTextX - FHViewPos, APosTextY, APos, False{SelectAll}, True{Hilight});
                if Assigned(AStrings) then
                  AStrings.Add(LineText, APosTextX - FHViewPos, APosTextY, APos);

                DoDrawLine2(Canvas, LineText,
                  Point(APosTextX - FHViewPos, APosTextY),
                  OutputOptions(WithCR));
              end;

              //Draw gutter dot
              if FTextGutter then
              begin
                Canvas.Brush.Color := FTextColorGutter;
                Canvas.FillRect(Rect(0, APosTextY, FTextGutterWidth, APosTextY + FFontHeight));
                if WithDot then
                begin
                  ALineNum := FindLineNum(APos);
                  if ActiveLinesShow and
                    (ALineNum > 0) and
                    (ALineNum mod FLinesStep = 0) then
                  begin
                    Canvas.Font.Assign(FFontGutter);
                    Canvas.TextOut(
                      (FTextGutterWidth - Canvas.TextWidth(IntToStr(ALineNum)) - cGutterIndent),
                      (FFontHeight - FTextSize.Y) div 2 + APosTextY,
                      IntToStr(ALineNum));
                    Canvas.Font.Assign(ActiveFont);
                  end
                  else
                  begin
                    Canvas.Brush.Color := AColorBack;
                    Canvas.Pen.Color := AColorText;
                    Canvas.Pen.Width := 1;
                    Canvas.Ellipse(
                      FTextGutterWidth div 2 - cGutterDotSize, APosTextY + FFontHeight div 2 - cGutterDotSize,
                      FTextGutterWidth div 2 + cGutterDotSize, APosTextY + FFontHeight div 2 + cGutterDotSize);
                  end;
                end;
                Canvas.Brush.Color := AColorBack;
              end;

              //Move to the next line
              APos := APosEnd;

              //Calculate the following flags only for fully visible lines
              if (i <= ALines) then
              begin
                //Calculate variable page size
                //(it is next/last position minus view position)
                if APos >= 0 then
                  AViewPageSize := APos - FViewPos
                else
                  AViewPageSize := FFileSize - FViewPos;

                //Calculate "at the end" flag
                AViewAtEnd := APos < 0;
              end;

              //Stop at the EOF
              if (APos < 0) then
                Break;

              //Stop after AFinalPos
              if (AFinalPos >= 0) and (APos > AFinalPos) then
                Break;
            end;
          end;

        vbmodeHex:
          begin
            for i := 1 to Min(ALines + 1, cMaxLines) do
            begin
              ACurrentPos := FViewPos + (i - 1) * ATextWidthHex;
              APos := ACurrentPos - FBufferPos;

              //Stop at the EOF
              if FBufferPos + APos >= FFileSize then Break;

              //Stop after AFinalPos
              if (AFinalPos >= 0) and (ACurrentPos > AFinalPos) then Break;

              Y := DrawOffsetY + (i - 1) * FFontHeight;
              Y2 := Y + FFontHeight;

              //Draw offset
              X := DrawOffsetX;
              LineA := IntToHex(FBufferPos + APos, FHexOffsetLen) + cHexOffsetSep;
              Canvas.Font.Color := AColorText;
              StringOut(Canvas, X - FHViewPos, Y, LineA, OutputOptions);

              //Draw hex background
              Inc(X, (Length(LineA) + 1{space}) * FFontWidthDigits);

              Canvas.Brush.Color := AColorBackHex;
              Canvas.FillRect(Rect(
                X - FHViewPos,
                Y,
                X - FHViewPos + FFontWidthDigits * (ATextWidthHex * 3 + 2),
                Y2 + (cHexLinesWidth div 2)));

              //Draw hex digits
              Inc(X, FFontWidthDigits);

              for j := 0 to ATextWidthHex - 1 do
              begin
                APosEnd := FBufferPos + APos + j;
                if APosEnd < FFileSize then
                begin
                  if (j mod 4) < 2 then
                    Canvas.Font.Color := AColorTextHex1
                  else
                    Canvas.Font.Color := AColorTextHex2;

                  LineW := GetHex(APosEnd);
                  StringOut(Canvas, X - FHViewPos, Y, LineW, OutputOptions);
                  SelectLine(LineW, X - FHViewPos, Y, FBufferPos + APos + j, True);

                  //Save hex offsets
                  TStrPositions(FStrings).AddHex(
                    j, X - FHViewPos,
                    ATextWidthHex, 3 * FFontWidthDigits);

                  //Inc hex offset
                  Inc(X, 3 * FFontWidthDigits); //3 spaces per byte
                  if j = (ATextWidthHex div 2 - 1) then
                    Inc(X, FFontWidthDigits); //Space in the middle
                end;
              end;

              //Draw text
              X := DrawOffsetX + (FHexOffsetLen + Length(cHexOffsetSep) + 3{3 spaces} + ATextWidthHex * 3) * FFontWidthDigits;
              TStrPositions(FStrings).AddHexMargin(X);
              Inc(X, FFontWidthDigits);

              Canvas.Brush.Color := AColorBack;
              Canvas.Font.Color := AColorText;
              LineW := '';

              for j := 0 to ATextWidthHex - 1 do
              begin
                APosEnd := FBufferPos + APos + j;
                if APosEnd < FFileSize then
                  LineW := LineW + GetChar(APosEnd);
              end;

              LineText := DecodeString(LineW);
              APosTextX := X;
              APosTextY := Y;
              StringOut(Canvas, APosTextX - FHViewPos, APosTextY, LineText, OutputOptions);
              SelectLine(LineText, APosTextX - FHViewPos, APosTextY, FBufferPos + APos, False{SelectAll}, True{Hilight});
              if Assigned(AStrings) then
                AStrings.Add(LineText, APosTextX - FHViewPos, APosTextY, FBufferPos + APos);

              //Draw lines
              if cHexLinesShow then
              begin
                Canvas.Pen.Color := AColorLines;
                Canvas.Pen.Width := cHexLinesWidth;

                X := DrawOffsetX + (FHexOffsetLen + Length(cHexOffsetSep) + 1{1 space}) * FFontWidthDigits;
                Canvas.MoveTo(X - FHViewPos, Y);
                Canvas.LineTo(X - FHViewPos, Y2);

                X := DrawOffsetX + (FHexOffsetLen + Length(cHexOffsetSep) + 2{2 spaces} + (ATextWidthHex div 2) * 3) * FFontWidthDigits;
                Canvas.MoveTo(X - FHViewPos, Y);
                Canvas.LineTo(X - FHViewPos, Y2);

                X := DrawOffsetX + (FHexOffsetLen + Length(cHexOffsetSep) + 3{3 spaces} + ATextWidthHex * 3) * FFontWidthDigits;
                Canvas.MoveTo(X - FHViewPos, Y);
                Canvas.LineTo(X - FHViewPos, Y2);
              end;
            end;

            DrawGutterTo(ABitmap);
            AViewAtEnd := FViewPos >= (FFileSize - ALines * ACols);
          end;

        vbmodeUHex:
          begin
            for i := 1 to Min(ALines + 1, cMaxLines) do
            begin
              ACurrentPos := FViewPos + (i - 1) * ATextWidthUHex * 2;
              APos := ACurrentPos - FBufferPos;

              //Stop at the EOF
              if FBufferPos + APos >= FFileSize then Break;

              //Stop after AFinalPos
              if (AFinalPos >= 0) and (ACurrentPos > AFinalPos) then Break;

              Y := DrawOffsetY + (i - 1) * FFontHeight;
              Y2 := Y + FFontHeight;

              //Draw offset
              X := DrawOffsetX;
              LineA := IntToHex(FBufferPos + APos, FHexOffsetLen) + cHexOffsetSep;
              Canvas.Font.Color := AColorText;
              StringOut(Canvas, X - FHViewPos, Y, LineA, OutputOptions);

              //Draw hex background
              Inc(X, (Length(LineA) + 1{space}) * FFontWidthDigits);

              Canvas.Brush.Color := AColorBackHex;
              Canvas.FillRect(Rect(
                X - FHViewPos,
                Y,
                X - FHViewPos + FFontWidthDigits * (ATextWidthUHex * 5 + 2),
                Y2 + (cHexLinesWidth div 2)));

              //Draw hex digits
              Inc(X, FFontWidthDigits);

              for j := 0 to ATextWidthUHex - 1 do
              begin
                APosEnd := FBufferPos + APos + 2 * j;
                if APosEnd + 1 < FFileSize then
                begin
                  if (j mod 4) < 2 then
                    Canvas.Font.Color := AColorTextHex1
                  else
                    Canvas.Font.Color := AColorTextHex2;

                  LineW := GetHex(APosEnd);
                  StringOut(Canvas, X - FHViewPos, Y, LineW, OutputOptions);
                  SelectLine(LineW, X - FHViewPos, Y, FBufferPos + APos + 2 * j, True);

                  //Save hex offset
                  TStrPositions(FStrings).AddHex(
                    j, X - FHViewPos,
                    ATextWidthUHex, 5 * FFontWidthDigits);

                  //Inc hex offset
                  Inc(X, 5 * FFontWidthDigits); //5 spaces per word
                  if j = (ATextWidthUHex div 2 - 1) then
                    Inc(X, FFontWidthDigits); //Space in the middle
                end;
              end;

              //Draw text
              X := DrawOffsetX + (FHexOffsetLen + Length(cHexOffsetSep) + 3{3 spaces} + ATextWidthUHex * 5) * FFontWidthDigits;
              TStrPositions(FStrings).AddHexMargin(X);
              Inc(X, FFontWidthDigits);

              Canvas.Brush.Color := AColorBack;
              Canvas.Font.Color := AColorText;
              LineW := '';

              for j := 0 to ATextWidthUHex - 1 do
              begin
                APosEnd := FBufferPos + APos + 2 * j;
                if APosEnd + 1 < FFileSize then
                  LineW := LineW + GetChar(APosEnd);
              end;

              LineText := DecodeString(LineW);
              APosTextX := X;
              APosTextY := Y;
              StringOut(Canvas, APosTextX - FHViewPos, APosTextY, LineText, OutputOptions);
              SelectLine(LineText, APosTextX - FHViewPos, APosTextY, FBufferPos + APos, False{SelectAll}, True{Hilight});
              if Assigned(AStrings) then
                AStrings.Add(LineText, APosTextX - FHViewPos, APosTextY, FBufferPos + APos);

              //Draw lines
              if cHexLinesShow then
              begin
                Canvas.Pen.Color := AColorLines;
                Canvas.Pen.Width := cHexLinesWidth;

                X := DrawOffsetX + (FHexOffsetLen + Length(cHexOffsetSep) + 1{1 space}) * FFontWidthDigits;
                Canvas.MoveTo(X - FHViewPos, Y);
                Canvas.LineTo(X - FHViewPos, Y2);

                X := DrawOffsetX + (FHexOffsetLen + Length(cHexOffsetSep) + 2{2 spaces} + (ATextWidthUHex div 2) * 5) * FFontWidthDigits;
                Canvas.MoveTo(X - FHViewPos, Y);
                Canvas.LineTo(X - FHViewPos, Y2);

                X := DrawOffsetX + (FHexOffsetLen + Length(cHexOffsetSep) + 3{3 spaces} + ATextWidthUHex * 5) * FFontWidthDigits;
                Canvas.MoveTo(X - FHViewPos, Y);
                Canvas.LineTo(X - FHViewPos, Y2);
              end;
            end;

            DrawGutterTo(ABitmap);
            AViewAtEnd := FViewPos >= (FFileSize - ALines * ACols);
          end;

        vbmodeBinary:
          begin
            for i := 1 to Min(ALines + 1, cMaxLines) do
            begin
              ACurrentPos := FViewPos + (i - 1) * ATextWidth;
              APos := ACurrentPos - FBufferPos;

              //Stop at the EOF
              if FBufferPos + APos >= FFileSize then Break;

              //Stop after AFinalPos
              if (AFinalPos >= 0) and (ACurrentPos > AFinalPos) then Break;

              LineW := '';
              for j := 0 to ATextWidth - 1 do
              begin
                APosEnd := FBufferPos + APos + j;
                if APosEnd < FFileSize then
                  LineW := LineW + GetChar(APosEnd);
              end;

              LineText := DecodeString(LineW);
              APosTextX := DrawOffsetX;
              APosTextY := DrawOffsetY + (i - 1) * FFontHeight;
              StringOut(Canvas, APosTextX - FHViewPos, APosTextY, LineText, OutputOptions);
              SelectLine(LineText, APosTextX - FHViewPos, APosTextY, FBufferPos + APos, False{SelectAll}, True{Hilight});
              if Assigned(AStrings) then
                AStrings.Add(LineText, APosTextX - FHViewPos, APosTextY, FBufferPos + APos);
            end;

            DrawGutterTo(ABitmap);
            AViewAtEnd := FViewPos >= (FFileSize - ALines * ACols);
          end;
      end; //case FMode
    end //if FFileOK
    else
      //Handle read error
      begin
        LineA := Format(MsgViewerErrCannotReadPos, [IntToHex(FViewPos, FHexOffsetLen)]);
        X := (Width - StringWidth(Canvas, LineA, OutputOptions)) div 2;
        Y := (Height - FFontHeight) div 2;
        ILimitMin(X, DrawOffsetX);
        ILimitMin(Y, DrawOffsetY);
        Canvas.Font.Color := AColorError;
        StringOut(Canvas, X, Y, LineA, OutputOptions);
      end;
  end;
end;

procedure TATBinHex.TextEncodingsMenu(AX, AY: Integer);
begin
  if IsModeUnicode then exit;
  //was in Delphi version, menu to choose UTF16 LE/BE, deleted here

  if not Assigned(FMenuEnc) then
  begin
    FMenuEnc := TPopupMenu.Create(Self);
    UpdateMenuEncodings(FMenuEnc.Items);
  end;
  FMenuEnc.Popup(AX, AY);
end;


procedure TATBinHex.Redraw(ARepaint: Boolean);
begin
  if (FPrevViewPos<>FViewPos) or
    (FPrevHViewPos<>FHViewPos) then
    DoOnScroll;
  FPrevViewPos:= FViewPos;
  FPrevHViewPos:= FHViewPos;

  if FEnabled2 then //Enabled2 enables control redrawing
    try
      Lock;

      //If file is empty, clear and quit
      if IsFileEmpty then
      begin
        HideScrollbars;
        DrawEmptyTo(FBitmap, ClientWidth, ClientHeight, False);
        if ARepaint then
          Paint;
        Exit;
      end;

      //Find matches
      {$ifdef search}
      FindAll;
      {$endif}

      //Do drawing
      DrawTo(
        FBitmap,
        ClientWidth,
        ClientHeight,
        FStrings, //AStringsObject
        False, //APrintMode
        -1, //AFinalPos not needed
        FTextWidth,
        FTextWidthHex,
        FTextWidthUHex,
        FViewPageSize,
        FViewAtEnd);

      {
      //Debug for TStrPositions.GetCoordFromPos:
      if TStrPositions(FStrings).GetCoordFromPos(FBitmap.Canvas, 60, FTabSize, IsAnsiDecode, DebugX, DebugY) then
      begin
        FBitmap.Canvas.Pen.Color := clRed;
        FBitmap.Canvas.MoveTo(DebugX, DebugY);
        FBitmap.Canvas.LineTo(DebugX, DebugY + 20);
      end;
      }

      //Update scrollbars and force paint
      UpdateVertScrollbar;
      UpdateHorzScrollbar;
      if ARepaint then
        Paint;
    finally
      Unlock;
    end;
  Invalidate;
end;

procedure TATBinHex.HideScrollbars;
var
  si: TScrollInfo;
begin
  FillChar(si, SizeOf(si), 0);
  with si do
  begin
    cbSize := SizeOf(si);
    fMask := SIF_ALL;
  end;
  SetScrollInfo(Handle, SB_VERT, si, True);
  SetScrollInfo(Handle, SB_HORZ, si, True);
end;

function TATBinHex.GetVertScroll_MaxPos: Int64;
begin
  Result := FFileSize div ColsNum;
  I64LimitMin(Result, 4); //Limit for small files
  I64LimitMax(Result, MAXSHORT);
end;

function TATBinHex.GetVertScroll_PageSize: Int64;
begin
  Result:= LinesNum*ColsNum;
end;

procedure TATBinHex.UpdateVertScrollbar;
var
  AHide: Boolean;
  APageSize,
  AMax, APos, APage: Int64;
  si: TScrollInfo;
begin
  //Calculate "page size":
  APageSize := GetVertScroll_PageSize;

  //debug
  ////Application.MainForm.Caption :=
  ////  Format('FileSize: %d, PageSize: %d', [FFileSize, FViewPageSize]);

  //Hide scrollbar in the following cases:
  AHide :=
    (not FFileOK) or  // - Read error occurs
    (IsFileEmpty) or  // - File is empty
    (IsModeVariable and (FViewPos = 0) and (FFileSize <= FViewPageSize));
                      // - File too small

  if AHide then
  begin
    AMax := 0;
    APos := 0;
    APage := 0;
  end
  else
  begin
    AMax := GetVertScroll_MaxPos;

    APos := AMax * FViewPos div FFileSize;
    I64LimitMax(APos, AMax);

    APage := AMax * APageSize div FFileSize;
    I64LimitMin(APage, 1);
    if APage >= AMax then
      APage := AMax + 1;
    //I64LimitMax(APage, MAXSHORT); //win32 delphi rudiment

    //Disable variable pagesize in Text mode,
    //otherwise pagesize will be small and unusable:
    if IsModeVariable then
      APage := 0;
  end;

  FillChar(si, SizeOf(si), 0);
  with si do
  begin
    cbSize := SizeOf(si);
    fMask := SIF_ALL;
    nMin := 0;
    nMax := AMax;
    nPage := APage;
    nPos := APos;
  end;

  SetScrollInfo(Handle, SB_VERT, si, True);
end;

procedure TATBinHex.UpdateHorzScrollbar;
var
  AHide: Boolean;
  AMax, APage, APos, AWidth: Integer;
  si: TScrollInfo;
begin
  //Hide scrollbar in the following cases:
  AHide :=
    (not FFileOK) or // - Read error occurs
    (IsFileEmpty) or // - File is empty
    (IsModeVariable and FTextWrap) or
                     // - Variable modes when TextWrap is on
    (FMode = vbmodeHex) or
    (FMode = vbmodeUHex) or
    ((FMode = vbmodeBinary) and FTextWidthFit and FFontMonospaced);
                     // - Binary mode when TextWidthFit is on and font is monospaced

  if AHide then
  begin
    AMax := 0;
    APage := 0;
    APos := 0;
  end
  else
  begin
    AWidth := Max(HPosWidth, FHViewPos + ClientWidth);

    {$ifdef SCROLL}
    //Remember max width, so scrollbar won't disappear
    ILimitMin(FHViewWidth, AWidth);
    {$else}
    FHViewWidth := AWidth;
    {$endif}

    AMax := FHViewWidth;
    APage := ClientWidth + 1;
    APos := FHViewPos;
  end;

  FillChar(si, SizeOf(si), 0);
  with si do
  begin
    cbSize := SizeOf(si);
    fMask := SIF_ALL;
    nMin := 0;
    nMax := AMax;
    nPage := APage;
    nPos := APos;
  end;

  SetScrollInfo(Handle, SB_HORZ, si, True);
end;

procedure TATBinHex.Paint;
begin
  //handle=0 paints on Windows desktop
  if not HandleAllocated then
    exit;

  Canvas.Draw(0, 0, FBitmap);
  DrawNiceScroll;
end;


function TATBinHex.PosBefore(const APos: Int64; ALineType: TATLineType; ADir: TATDirection): Int64;
const
  cSepWord = ' !"#$%&''()*+,-./:;<=>?@[\]^`{|}~';
  cSepUrl = ' ()<>{}"''';
var
  PosTemp: Int64;
  i: Integer;
  ch: WideChar;
begin
  Result := APos;
  NormalizePos(Result);
  PosTemp := Result;
  for i := 1 to cMaxLengthSel do
  begin
    NextPos(PosTemp, ADir);
    if PosBad(PosTemp) then Break;
    ch := GetChar(PosTemp);
    if ch=cCharSpecial then Break;

    case ALineType of
      vbLineAll:
        begin
          if (ch=#10) or (ch=#13) then Break;
        end;
      vbLineWord:
        begin
          if Ord(ch)<$20 then Break;
          if Pos(ch, cSepWord)>0 then Break;
        end;
      vbLineURL:
        begin
          if Ord(ch)<$20 then Break;
          if Pos(ch, cSepUrl)>0 then Break;
        end;
    end;

    Result := PosTemp;
  end;
end;

procedure TATBinHex.SelectLineAtPos(const APos: Int64; ALineType: TATLineType);
var
  APosStart, APosEnd: Int64;
begin
  APosStart := PosBefore(APos, ALineType, vdirUp);
  APosEnd := PosBefore(APos, ALineType, vdirDown);
  SetSelection(APosStart, APosEnd - APosStart + CharSize, False);
end;


function TATBinHex.StringAtPos(const APos: Int64): UnicodeString;
var
  APosStart, APosEnd: Int64;
  S: AnsiString;
begin
  Result := '';
  APosStart := PosBefore(APos, vbLineAll, vdirUp);
  APosEnd := PosBefore(APos, vbLineAll, vdirDown);
  if PosBad(APosStart) or PosBad(APosEnd) then Exit;

  SetString(S,
    PAnsiChar(@FBuffer[APosStart - FBufferPos]),
    APosEnd - APosStart + CharSize);

  if S <> '' then
    if IsModeUnicode then
      Result := SetStringW(@S[1], Length(S), IsUnicodeBE)
    else
      Result := UTF8Decode(SCodepageToUTF8(S, FTextEncoding));
end;

procedure TATBinHex.DblClick;
begin
  if FMouseStartDbl < 0 then Exit;

  FMouseDblClick := True;
  FMouseTriClick := False;
  FMouseTriTime := 0;
end;

procedure TATBinHex.ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  FMousePopupPos := MousePos;
end;


function TATBinHex.SourceAssigned: Boolean;
begin
  case FFileSourceType of
    vfSrcStream:
      Result := Assigned(FStream);
    else
      Result := False;
  end;
end;

function TATBinHex.ReadSource(
  const APos: Int64;
  ABuffer: Pointer;
  ABufferSize: DWORD;
  var AReadSize: DWORD): Boolean;
begin
  Result := False;
  FillChar(ABuffer^, ABufferSize, 0);

  Assert(Assigned(ABuffer), 'Buffer not allocated: ReadSource');
  Assert(SourceAssigned, 'Source not assigned: ReadSource');

  case FFileSourceType of
    vfSrcStream:
      try
        FStream.Position := APos;
        AReadSize := FStream.Read(ABuffer^, ABufferSize);
        Result := True;
      except
      end;
  end;
end;

procedure TATBinHex.MsgReadError;
begin
  case FFileSourceType of
    vfSrcStream:
      MsgError(MsgViewerErrCannotReadStream);
  end;
end;

function TATBinHex.MsgReadRetry: Boolean;
begin
  case FFileSourceType of
    vfSrcStream:
      Result := MsgBox(
        MsgViewerErrCannotReadStream,
        MsgViewerCaption, MB_RETRYCANCEL or MB_ICONERROR) = IDRETRY;
    else
      Result := False;
  end;
end;

procedure TATBinHex.MsgOpenError;
begin
  MsgError(Format(MsgViewerErrCannotOpenFile, [FFileName]));
end;

procedure TATBinHex.ReadBuffer(const APos: Int64 = -1);
var
  ARead: DWORD;
  ABufSize: Integer;
begin
  if SourceAssigned then
    if not ((APos >= FBufferPos) and (APos < FBufferPos + 2 * FBufferMaxOffset)) then
    begin
      FBufferPos := APos - FBufferMaxOffset;
      I64LimitMin(FBufferPos, 0);
      NormalizePos(FBufferPos);

      repeat
        FFileOK := ReadSource(FBufferPos, FBuffer, FBufferAllocSize, ARead);
        if FFileOK then Break;
        if not MsgReadRetry then Exit;
      until False;

      ReadUnicodeFmt;

      if ActiveLinesShow then
        if (FBufferPos <= FLinesBufSize) then
        begin
          ABufSize := Min(
            FLinesBufSize,
            FBufferPos + FBufferAllocSize);
          CountLines(ABufSize);
        end;

      if FUrlShow then
        FindURLs(ARead)
      else
        InitURLs;
    end;
end;

(*
function TATBinHex.Open(const AFileName: string; ARedraw: Boolean = True): Boolean;
begin
  Result := True;
  if FFileName <> AFileName then
  begin
    FFileName := AFileName;
    Result := LoadFile(True);
    if ARedraw then
      Redraw;
  end;
end;
*)

function TATBinHex.OpenStream(AStream: TStream; ARedraw: Boolean = True): Boolean;
begin
  Result := True;
  if FStream <> AStream then
  begin
    FStream := AStream;
    Result := LoadStream;
    if ARedraw then
      Redraw;
  end;
end;

function TATBinHex.LinesNum(ABitmap: TBitmap = nil): Integer;
var
  AHeight: Integer;
begin
  if Assigned(ABitmap) then
    AHeight := ABitmap.Height
  else
    AHeight := FBitmap.Height;

  Result := (AHeight - DrawOffsetY - cDrawOffsetBelowY) div FFontHeight;
  ILimitMin(Result, 0);
  ILimitMax(Result, cMaxLines);
end;

function TATBinHex.ColsNumFit(ABitmap: TBitmap = nil): Integer;
var
  AWidth: Integer;
begin
  if Assigned(ABitmap) then
    AWidth := ABitmap.Width
  else
    AWidth := FBitmap.Width;

  Result := (AWidth - DrawOffsetX) div FFontWidthDigits;
  ILimitMin(Result, cMinLength);
  ILimitMax(Result, FMaxLength);
end;

function TATBinHex.ColsNumHexFit(ABitmap: TBitmap = nil): Integer;
const
  //Take 4 spaces for each byte:
  cSpacesPerChar = 4;
begin
  Result := (ColsNumFit(ABitmap) - (FHexOffsetLen + Length(cHexOffsetSep) + 4{4 inner spaces})) div cSpacesPerChar;
  ILimitMin(Result, cMinLength);
  ILimitMax(Result, FMaxLength);
end;

function TATBinHex.ColsNumUHexFit(ABitmap: TBitmap = nil): Integer;
const
  //Take (6 + ~0.8) = ~7 spaces for each word
  //(~0.8 because wide ieroglyphs take about ~1.8 spaces).
  //Take 6 as it looks nicer:
  cSpacesPerChar = 6;
begin
  Result := (ColsNumFit(ABitmap) - (FHexOffsetLen + Length(cHexOffsetSep) + 4{4 inner spaces})) div cSpacesPerChar;
  ILimitMin(Result, cMinLength);
  ILimitMax(Result, FMaxLength);
end;

function TATBinHex.ColsNum(ABitmap: TBitmap = nil): Integer;
begin
  case FMode of
    vbmodeBinary:
      Result := FTextWidth;
    vbmodeHex:
      Result := FTextWidthHex;
    vbmodeUHex:
      Result := FTextWidthUHex * CharSize;
    else
      Result := CharSize; //Stub for variable modes
  end;
end;

function TATBinHex.PosBad(const APos: Int64): Boolean;
begin
  Result := not (
    (APos >= 0) and
    (APos <= PosLast) and
    (APos - FBufferPos >= 0) and
    (APos - FBufferPos < FBufferAllocSize)
    );
end;

//Max position regarding page size.
//Used only in Binary/Hex modes.
function TATBinHex.PosMax: Int64;
var
  ACols: Integer;
begin
  ACols := ColsNum;
  Result := FFileSize div ACols * ACols;
  if Result = FFileSize then
    Dec(Result, ACols);
  //Dec(Result, (LinesNum - 1) * ACols); //removed to fix https://github.com/Alexey-T/CudaText/issues/2960
  I64LimitMin(Result, 0);
end;

//Max position at the very end of file.
function TATBinHex.PosLast: Int64;
begin
  Result := FFileSize;
  NormalizePos(Result);
  Dec(Result, CharSize);
  I64LimitMin(Result, 0);
end;


//If we are at CR-LF middle (at LF) move up to CR:
procedure TATBinHex.PosFixCRLF(var APos: Int64);
begin
  if IsModeVariable and
    (GetChar(APos) = #10) and
    (GetChar(APos - CharSize) = #13) then
    NextPos(APos, vdirUp);
end;


//Used (with one exception) only in Binary/Hex modes.
procedure TATBinHex.PosAt(const APos: Int64; ARedraw: Boolean = True);
var
  ACols: Integer;
begin
  if (APos <> FViewPos) and (APos >= 0) then
  begin
    FViewPos := APos;
    I64LimitMax(FViewPos, PosLast);

    ACols := ColsNum;
    FViewPos := FViewPos div ACols * ACols;

    ReadBuffer(FViewPos);

    if ARedraw then
      Redraw;
  end;
end;

//Used only in Binary/Hex modes.
procedure TATBinHex.PosDec(const N: Int64);
begin
  if (FViewPos - N >= 0) then
    PosAt(FViewPos - N)
  else
    PosBegin;
end;

//Used only in Binary/Hex modes.
procedure TATBinHex.PosInc(const N: Int64);
begin
  if (FViewPos < PosMax) then
    PosAt(FViewPos + N);
end;

procedure TATBinHex.PosLineUp(ALines: Integer = 1);
begin
  PosLineUp(FViewAtEnd, ALines);
end;

procedure TATBinHex.PosLineDown(ALines: Integer = 1);
begin
  PosLineDown(FViewAtEnd, ALines);
end;

procedure TATBinHex.PosLineUp(AViewAtEnd: Boolean; ALines: Integer);
begin
  if IsModeVariable then
    PosNextLine(ALines, vdirUp, AViewAtEnd)
  else
    PosDec(ALines * ColsNum);
end;

procedure TATBinHex.PosLineDown(AViewAtEnd: Boolean; ALines: Integer);
begin
  if IsModeVariable then
    PosNextLine(ALines, vdirDown, AViewAtEnd)
  else
    PosInc(ALines * ColsNum);
end;

procedure TATBinHex.PosPageUp;
begin
  PosLineUp(LinesNum);
end;

procedure TATBinHex.PosPageDown;
begin
  PosLineDown(LinesNum);
end;

procedure TATBinHex.PosBegin;
begin
  HPosAt(0, False);
  PosAt(0);
end;

procedure TATBinHex.PosEndTry;
begin
  HPosAt(0, False);
  if IsModeVariable then
    PosNextLineFrom(FFileSize, LinesNum, vdirUp)
  else
    PosAt(PosMax);
end;

procedure TATBinHex.PosEnd;
begin
  //First scroll to end. If then scrollbar appears (the last line can be long),
  //then we need to scroll to end **again*.
  PosEndTry;
  if not FViewAtEnd then
    PosEndTry;
end;

function TATBinHex.GetPosPercent: Integer;
begin
  if IsFileEmpty then
    Result := 0
  else
    Result := FViewPos * 100 div FFileSize;
end;

procedure TATBinHex.SetPosPercent(APos: Integer);
begin
  if APos <= 0 then PosBegin else
    if APos >= 100 then PosEnd else
      SetPosOffset(FFileSize * APos div 100);
end;

function TATBinHex.GetPosOffset: Int64;
begin
  Result := FViewPos;
end;

procedure TATBinHex.SetPosOffset(const APos: Int64);
begin
  if APos <= 0 then PosBegin else
    if APos >= PosLast then PosEnd else
    begin
      if IsModeVariable then
        PosNextLineFrom(APos, 1, vdirUp, True{APassiveMove})
      else
        PosAt(APos);
    end;
end;


procedure TATBinHex.InitData;
begin
  FFileSize := 0;
  FFileOK := True;
  FFileUnicodeFmt := vbUnicodeFmtUnknown;
  FFileSourceType := vfSrcNone;

  FBuffer := nil;
  FBufferMaxOffset := 0;
  FBufferAllocSize := 0;
  FBufferPos := 0;

  if Assigned(FLinesData) then
    FreeMem(FLinesData);
  FLinesData := nil;
  FLinesNum := 0;

  FViewPos := 0;
  FViewAtEnd := False;
  FViewPageSize := 0;
  FHViewPos := 0;
  FHViewWidth := 0;
  FSelStart := 0;
  FSelLength := 0;
  FMouseDown := False;
  FMouseStart := -1;
  FMouseStartShift := -1;
  FMouseStartDbl := -1;
  FMouseDblClick := False;
  FMouseTriClick := False;
  FMouseTriTime := 0;
  FMousePopupPos := Point(-1, -1);
  FMouseRelativePos := vmPosInner;
  FMouseNiceScroll := False;
  FMouseNiceScrollPos := Point(0, 0);
  FClientHeight := 0;
end;

procedure TATBinHex.FreeData;
begin
  {$ifdef SEARCH}
  case FFileSourceType of
    //vfSrcFile:
    //  FSearch.FileName := '';
    vfSrcStream:
      FSearch.Stream := nil;
  end;
  FSearchStarted := False;
  {$endif}

  if Assigned(FBuffer) then
  begin
    FreeMem(FBuffer);
    FBuffer := nil;
    FBufferMaxOffset := 0;
    FBufferAllocSize := 0;
  end;

  InitData;

  FTimerAutoScroll.Enabled := False;
  FTimerNiceScroll.Enabled := False;
end;

function TATBinHex.LoadFile(ANewFile: Boolean): Boolean;
var
  OldViewPos: Int64;
  OldViewHPos: Integer;
  OldAtEnd: Boolean;
  OldSelStart,
  OldSelLength: Int64;
  NeedToRestorePos: Boolean;
begin
  Result := False;

  {$ifdef NOTIF}
  if ANewFile or (not FAutoReload) then
  begin
    FNotif.Stop;
    FNotif2.Timer.Enabled := False;
  end;
  {$endif}

  OldViewPos := FViewPos;
  OldViewHPos := FHViewPos;
  OldAtEnd := FViewAtEnd;
  OldSelStart := FSelStart;
  OldSelLength := FSelLength;
  NeedToRestorePos := not ANewFile;

  FreeData;

  if FFileName = '' then
  begin
    Result := True;
    Exit
  end;

  FFileSize := 0;
  FFileSourceType := vfSrcStream;

  AllocBuffer;
  ReadBuffer;
  InitHexOffsetLen;

  //Restore selection
  if NeedToRestorePos then
    SetSelection(OldSelStart, OldSelLength,
      False{No scroll}, False{No event}, False{No redraw});

  {$ifdef NOTIF}
  if FAutoReload then
  begin
    //Restore pos, with tailing
    if NeedToRestorePos then
      if FAutoReloadFollowTail and OldAtEnd then
      begin
        PosEnd;
        NeedToRestorePos := False; //don't restore later
      end;

    //Start watching
    if FAutoReloadSimple then
    begin
      FNotif2.Timer.Enabled := False;
      FNotif2.Timer.Interval := FAutoReloadSimpleTime;
      FNotif2.FileName := FFileName;
      FNotif2.Timer.Enabled := True;
    end
    else
    begin
      FNotif.FileName := FFileName;
      FNotif.Start;
    end;
  end;
  {$endif}

  if NeedToRestorePos then
  begin
    SetPosOffset(OldViewPos);
    HPosAt(OldViewHPos);
  end;

  Result := True;
end;

function TATBinHex.LoadStream: Boolean;
begin
  Result := True;

  {$ifdef NOTIF}
  FNotif.Stop;
  FNotif2.Timer.Enabled := False;
  {$endif}

  FreeData;

  if not Assigned(FStream) then Exit;

  FFileSize := FStream.Size;
  FFileSourceType := vfSrcStream;

  AllocBuffer;
  ReadBuffer;
  InitHexOffsetLen;
end;

procedure TATBinHex.InitHexOffsetLen;
begin
  if IsFileEmpty then
    FHexOffsetLen := 0
  else
  begin
    FHexOffsetLen := Trunc(Ln((FFileSize + 0.0)) / Ln(16.0)) + 1;
    ILimitMin(FHexOffsetLen, 8);
    if (FHexOffsetLen mod 2) > 0 then
      Inc(FHexOffsetLen);
  end;
end;

procedure TATBinHex.SetMode(AMode: TATBinHexMode);
var
  FPrevPos: Int64;
begin
  FPrevPos := PosOffset;
  FMode := AMode;
  MouseNiceScroll := False;

  case FFileSourceType of
    vfSrcStream:
      LoadStream;
  end;

  if SourceAssigned then
  begin
    PosOffset := FPrevPos;
    Redraw;
  end;
end;

procedure TATBinHex.SetTextEncoding(AValue: TEncConvId);
begin
  if AValue <> FTextEncoding then
  begin
    FTextEncoding := AValue;
    Redraw;
  end;
end;

procedure TATBinHex.SetTextWidthTo(AValue: Integer; var AField: Integer);
begin
  AField := AValue;
  ILimitMin(AField, cMinLength);
  ILimitMax(AField, FMaxLengths[vbmodeBinary]);
end;

procedure TATBinHex.SetTextWidth(AValue: Integer);
begin
  SetTextWidthTo(AValue, FTextWidth);
end;

procedure TATBinHex.SetTextWidthHexTo(AValue: Integer; var AField: Integer);
begin
  AField := AValue;
  ILimitMin(AField, cMinLength);
  ILimitMax(AField, FMaxLengths[vbmodeHex]);
  AField := AField div 4 * 4;
  ILimitMin(AField, cMinLength);
end;

procedure TATBinHex.SetTextWidthUHexTo(AValue: Integer; var AField: Integer);
begin
  AField := AValue;
  ILimitMin(AField, cMinLength);
  ILimitMax(AField, FMaxLengths[vbmodeUHex]);
  AField := AField div 4 * 4;
  ILimitMin(AField, cMinLength);
end;

procedure TATBinHex.SetTextWidthHex(AValue: Integer);
begin
  SetTextWidthHexTo(AValue, FTextWidthHex);
end;

procedure TATBinHex.SetTextWidthUHex(AValue: Integer);
begin
  SetTextWidthUHexTo(AValue, FTextWidthUHex);
end;

procedure TATBinHex.SetTextWidthFit(AValue: Boolean);
begin
  FTextWidthFit := AValue;
end;

procedure TATBinHex.SetTextWidthFitHex(AValue: Boolean);
begin
  if AValue <> FTextWidthFitHex then
  begin
    FTextWidthFitHex := AValue;
    if not FTextWidthFitHex then
      FTextWidthHex := 16;
  end;
end;

procedure TATBinHex.SetTextWidthFitUHex(AValue: Boolean);
begin
  if AValue <> FTextWidthFitUHex then
  begin
    FTextWidthFitUHex := AValue;
    if not FTextWidthFitUHex then
      FTextWidthUHex := 8;
  end;
end;

procedure TATBinHex.SetTextWrap(AValue: Boolean);
begin
  if AValue <> FTextWrap then
  begin
    FTextWrap := AValue;
    if FTextWrap then
      HPosAt(0, False);
    Redraw;
  end;
end;

procedure TATBinHex.SetSearchIndentVert(AValue: Integer);
begin
  FSearchIndentVert := AValue;
  ILimitMin(FSearchIndentVert, 0);
  ILimitMax(FSearchIndentVert, cMaxSearchIndent);
end;

procedure TATBinHex.SetSearchIndentHorz(AValue: Integer);
begin
  FSearchIndentHorz := AValue;
  ILimitMin(FSearchIndentHorz, 0);
  ILimitMax(FSearchIndentHorz, cMaxSearchIndent);
end;

procedure TATBinHex.SetFontFooter(AValue: TFont);
begin
  FFontFooter.Assign(AValue);
end;

procedure TATBinHex.SetFontGutter(AValue: TFont);
begin
  FFontGutter.Assign(AValue);
end;

procedure TATBinHex.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TATBinHex.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TATBinHex.WMVScroll(var Message: TLMVScroll);
var
  ACols: Integer;
  AMax, ANew: Int64;
  ADisable: Boolean;
begin
  case Message.ScrollCode of
    SB_TOP:
      PosBegin;

    SB_BOTTOM:
      PosEnd;

    SB_LINEUP:
      PosLineUp;

    SB_LINEDOWN:
      PosLineDown;

    SB_PAGEUP:
      PosPageUp;

    SB_PAGEDOWN:
      PosPageDown;

    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        //if scrolled to end, do PosEnd, to make OK on huge files
        if not IsModeVariable then
        begin
          AMax := GetVertScroll_MaxPos-GetVertScroll_PageSize;
          if (AMax>0) and (Message.Pos>=AMax) then
          begin
            PosEnd;
            Exit;
          end;
        end;

        ACols := ColsNum;
        AMax := FFileSize div ACols;
        I64LimitMin(AMax, 1);
        I64LimitMax(AMax, MAXSHORT);
        ANew := FFileSize * Message.Pos div AMax;

        //For small issue, when file position jumps at the EOF:
        //Disable scroll past the last visible line.
        ADisable := FViewAtEnd and (ANew >= FViewPos);

        if not ADisable then
          SetPosOffset(ANew);
      end;
  end;

  Message.Result := 0;
  Invalidate;
end;

procedure TATBinHex.WMHScroll(var Message: TLMHScroll);
begin
  case Message.ScrollCode of
    SB_TOP:
      HPosBegin;

    SB_BOTTOM:
      HPosEnd;

    SB_LINELEFT:
      HPosLeft;

    SB_LINERIGHT:
      HPosRight;

    SB_PAGELEFT:
      HPosPageLeft;

    SB_PAGERIGHT:
      HPosPageRight;

    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      HPosAt(Message.Pos);
  end;

  Message.Result := 0;
  Invalidate;
end;

procedure TATBinHex.KeyDown(var Key: Word; Shift: TShiftState);
begin
  MouseNiceScroll := False;

  //PgDn: page down
  if (Key = VK_NEXT) and (Shift = []) then
  begin
    PosPageDown;
    Key := 0;
    Exit
  end;

  //PgUp: page up
  if (Key = VK_PRIOR) and (Shift = []) then
  begin
    PosPageUp;
    Key := 0;
    Exit
  end;

  //Down: down one line
  if (Key = VK_DOWN) and (Shift = []) then
  begin
    PosLineDown;
    Key := 0;
    Exit
  end;

  //Up: up one line
  if (Key = VK_UP) and (Shift = []) then
  begin
    PosLineUp;
    Key := 0;
    Exit
  end;

  //Ctrl+Home: begin of file
  if (Key = VK_HOME) and (Shift = [ssCtrl]) then
  begin
    PosBegin;
    Key := 0;
    Exit
  end;

  //Ctrl+End: end of file
  if (Key = VK_END) and (Shift = [ssCtrl]) then
  begin
    PosEnd;
    Key := 0;
    Exit
  end;

  //Left: ~200 px left
  if (Key = VK_LEFT) and (Shift = []) then
  begin
    HPosLeft;
    Key := 0;
    Exit
  end;

  //Right: ~200 px right
  if (Key = VK_RIGHT) and (Shift = []) then
  begin
    HPosRight;
    Key := 0;
    Exit
  end;

  //Home
  if (Key = VK_HOME) and (Shift = []) then
  begin
    if IsModeVariable then
      HPosBegin
    else
      PosBegin;
    Key := 0;
    Exit
  end;

  //End
  if (Key = VK_END) and (Shift = []) then
  begin
    if IsModeVariable then
      HPosEnd
    else
      PosEnd;
    Key := 0;
    Exit
  end;

  //Ctrl+A: select all
  if (Key = Ord('A')) and (Shift = [ssCtrl]) then
  begin
    SelectAll;
    Key := 0;
    Exit
  end;

  //Ctrl+C, Ctrl+Ins: copy to clipboard
  if ((Key = Ord('C')) or (Key = VK_INSERT)) and (Shift = [ssCtrl]) then
  begin
    CopyToClipboard;
    Key := 0;
    Exit
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TATBinHex.CopyToClipboard(AAsHex: Boolean = False);
var
  StrA: AnsiString;
  StrW: UnicodeString;
begin
  try
    if IsModeUnicode then
    begin
      StrW := GetSelTextW;
      SReplaceZerosW(StrW);
      SCopyToClipboardW(StrW);
    end
    else
    begin
      StrA := GetSelTextRaw;
      if AAsHex then
        StrA := SToHex(StrA)
      else
        SReplaceZeros(StrA);
      SCopyToClipboard(StrA);
    end;
  except
    MsgError(MsgViewerErrCannotCopyData);
  end;
end;

function TATBinHex.GetSelTextRaw(AMaxSize: Integer = 0): AnsiString;
var
  ABuffer: AnsiString;
  ABlockSize: Integer;
  ABytesRead: DWORD;
begin
  Result := '';

  if FSelLength > 0 then
  begin
    if AMaxSize > 0 then
      ABlockSize := Min(FSelLength, AMaxSize)
    else
      ABlockSize := Min(FSelLength, FMaxClipboardDataSizeMb * 1024 * 1024);

    SetLength(ABuffer, ABlockSize);

    if not ReadSource(FSelStart, @ABuffer[1], ABlockSize, ABytesRead) then
    begin
      MsgReadError;
      Exit;
    end;

    SetLength(ABuffer, ABytesRead);
    Result := ABuffer;
  end;
end;

function TATBinHex.GetSelText: AnsiString;
begin
  Assert(not IsModeUnicode, 'SelText is called in Unicode mode');

  Result := GetSelTextRaw;
  Result := SCodepageToUTF8(Result, FTextEncoding);
end;

function TATBinHex.GetSelTextShort: AnsiString;
begin
  Assert(not IsModeUnicode, 'SelText is called in Unicode mode');

  Result := GetSelTextRaw(cMaxShortLength * CharSize);
  Result := SCodepageToUTF8(Result, FTextEncoding);
end;

function TATBinHex.GetSelTextW: UnicodeString;
var
  S: AnsiString;
begin
  Assert(IsModeUnicode, 'SelTextW is called in non-Unicode mode');

  S := GetSelTextRaw;

  if S = '' then
    Result := ''
  else
    Result := SetStringW(@S[1], Length(S), IsUnicodeBE);
end;

function TATBinHex.GetSelTextShortW: UnicodeString;
var
  S: AnsiString;
begin
  Assert(IsModeUnicode, 'SelTextW is called in non-Unicode mode');

  S := GetSelTextRaw(cMaxShortLength * CharSize);

  if S = '' then
    Result := ''
  else
    Result := SetStringW(@S[1], Length(S), IsUnicodeBE);
end;

procedure TATBinHex.SetSelection(
  const AStart, ALength: Int64;
  AScroll: Boolean;
  AFireEvent: Boolean = True;
  ARedraw: Boolean = True);
var
  ASelChanged: Boolean;
begin
  if not FEnableSel then
    begin FSelLength := 0; Exit end;

  if (AStart >= 0) and (AStart <= PosLast) and (ALength >= 0) then
  begin
    ASelChanged := (AStart <> FSelStart) or (ALength <> FSelLength);

    if ASelChanged then
    begin
      FSelStart := AStart;
      FSelLength := ALength;
      NormalizePos(FSelStart);
      NormalizePos(FSelLength);
      I64LimitMax(FSelLength, PosLast - FSelStart + CharSize);
    end;

    if AScroll then
      Scroll(AStart, FSearchIndentVert, FSearchIndentHorz, False);

    if ARedraw then
      Redraw;

    if ASelChanged and AFireEvent then
      DoSelectionChange;
  end;
end;

procedure TATBinHex.Scroll(
  const APos: Int64;
  AIndentVert, AIndentHorz: Integer;
  ARedraw: Boolean = True);
var
  ANewPos: Int64;
  ACols, APosX, APosY: Integer;
begin
  //Scroll vertically (redraw if needed)
  if IsModeVariable then
  begin
    PosNextLineFrom(APos, AIndentVert + 1, vdirUp);
  end
  else
  begin
    ACols := ColsNum;
    ANewPos := APos div ACols * ACols;
    Dec(ANewPos, ACols * AIndentVert);
    I64LimitMin(ANewPos, 0);
    PosAt(ANewPos);
  end;

  //Scroll horizontally (redraw if needed and allowed)
  if TStrPositions(FStrings).GetCoordFromPos(
    FBitmap.Canvas, APos, OutputOptions, APosX, APosY) then
  begin
    if not ((APosX >= DrawOffsetX) and (APosX < ClientWidth - cSelectionRightIndent)) then
      HPosAt(APosX - DrawOffsetX + FHViewPos - AIndentHorz * FFontWidthDigits, ARedraw);
  end;
end;

procedure TATBinHex.SelectAll;
begin
  SetSelection(0, FFileSize, False);
end;

procedure TATBinHex.SelectNone(AFireEvent: Boolean = True);
begin
  SetSelection(0, 0, False, AFireEvent);
end;

procedure TATBinHex.MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = [ssShift]) then
    HPosLeft
  else
  if (Shift = [ssCtrl]) then
    IncreaseFontSize(True)
  else
  if (Shift = []) then
    PosLineUp(Mouse.WheelScrollLines);

  Handled := True;
end;

procedure TATBinHex.MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (Shift = [ssShift]) then
    HPosRight
  else
  if (Shift = [ssCtrl]) then
    IncreaseFontSize(False)
  else
  if (Shift = []) then
    PosLineDown(Mouse.WheelScrollLines);

  Handled := True;
end;

function TATBinHex.MousePosition(AX, AY: Integer; AStrict: Boolean = False): Int64;
begin
  Result := TStrPositions(FStrings).GetPosFromCoord(
    FBitmap.Canvas, AX, AY, OutputOptions, AStrict);
end;

procedure TATBinHex.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AMouseStartNew: Int64;
begin
  SetFocus;

  if MouseNiceScroll then
  begin
    MouseNiceScroll := False;
    Exit
  end;

  case Button of
    mbMiddle:
      if cMouseNiceScroll then
      begin
        FMouseNiceScrollPos := Point(X, Y);
        MouseNiceScroll := True;
      end;

    mbLeft:
      begin
        AMouseStartNew := MousePosition(X, Y);
        if Shift = [ssShift, ssLeft] then
        begin
          //Shift+click
          if cSelectionByShiftClick then
            SetSelection(
              Max(Min(AMouseStartNew, FMouseStartShift), 0),
              Abs(AMouseStartNew - FMouseStartShift),
              False);
        end
        else
        begin
          if FMouseTriClick and (GetTickCount - FMouseTriTime <= GetDoubleClickTime) then
          begin
            //Triple click
            FMouseDblClick := False;
            FMouseTriClick := False;
            FMouseTriTime := 0;
            if cSelectionByTripleClick then
              SelectLineAtPos(FMouseStartDbl, vbLineAll);
          end
          else
          begin
            if FMouseDblClick then
            begin
              //Double click (already handled in DblClick)
              FMouseDblClick := False;
              FMouseTriClick := True;
              FMouseTriTime := GetTickCount;
            end
            else
            begin
              //Single click (not second click of double click!)
              SelectNone(False);
            end;
          end;

          FMouseDown := True;
          FMouseStart := AMouseStartNew;
          FMouseStartShift := FMouseStart;
          FMouseStartDbl := FMouseStart - CharSize;
          FMouseDblClick := False;
        end;
      end;

  end;
end;

procedure TATBinHex.MouseMoveAction(AX, AY: Integer);
var
  APosStart, APosEnd: Int64;
begin
  APosStart := FMouseStart;
  if APosStart < 0 then Exit;
  APosEnd := MousePosition(AX, AY);
  if APosEnd < 0 then Exit;
  if APosStart > APosEnd then
    SwapInt64(APosStart, APosEnd);
  SetSelection(APosStart, APosEnd - APosStart, False, False{No event});
end;

procedure TATBinHex.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  //Show URL cursor (when not NiceScroll)
  if not MouseNiceScroll then
    if IsPosURL(MousePosition(X, Y, True{AStrict})) then
      Cursor := crHandPoint
    else
      Cursor := crIBeam;

  //If cursor is out of client area,
  //start FTimerAutoScroll which will do the scrolling
  if FMouseDown then
  begin
    if Y <= DrawOffsetY then
    begin
      FMouseRelativePos := vmPosUpper;
      FTimerAutoScroll.Enabled := True;
      Exit
    end;

    if Y >= ClientHeight - 1 then
    begin
      FMouseRelativePos := vmPosLower;
      FTimerAutoScroll.Enabled := True;
      Exit
    end;

    if X <= DrawOffsetX then
    begin
      FMouseRelativePos := vmPosLefter;
      FTimerAutoScroll.Enabled := True;
      Exit;
    end;

    if X >= ClientWidth - 1 then
    begin
      FMouseRelativePos := vmPosRighter;
      FTimerAutoScroll.Enabled := True;
      Exit;
    end;

    //Else stop timer and perform needed actions
    FMouseRelativePos := vmPosInner;
    FTimerAutoScroll.Enabled := False;
    MouseMoveAction(X, Y);
  end
  else
  begin
    FTimerAutoScroll.Enabled := False;
  end;
end;

procedure TATBinHex.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    //Mouse released where pressed:
    if MousePosition(X, Y) = FMouseStart then
      DoClickURL(FMouseStart);

    FMouseDown := False;
    FMouseStart := -1;
    FTimerAutoScroll.Enabled := False;

    if FMouseDblClick then
    begin
      if cSelectionByDoubleClick then
        SelectLineAtPos(FMouseStartDbl, vbLineWord);
    end
    else
      DoSelectionChange;

    FMouseDblClick := False;
    FMouseStartDbl := -1;
  end;
end;


procedure TATBinHex.TimerAutoScrollTimer(Sender: TObject);
var
  Y: Integer;
begin
  Y := ScreenToClient(Mouse.CursorPos).Y;

  case FMouseRelativePos of
    vmPosUpper:
      begin
        PosLineUp(cMouseAutoScrollSpeedY);
        MouseMoveAction(0, -1);
      end;

    vmPosLower:
      begin
        PosLineDown(cMouseAutoScrollSpeedY);
        MouseMoveAction(0, ClientHeight + 1);
      end;

    vmPosLefter:
      begin
        HPosDec(cMouseAutoScrollSpeedX);
        MouseMoveAction(-1, Y);
      end;

    vmPosRighter:
      begin
        HPosInc(cMouseAutoScrollSpeedX);
        MouseMoveAction(ClientWidth + 1, Y);
      end;
  end;
end;

procedure TATBinHex.TimerNiceScrollTimer(Sender: TObject);
var
  Pnt: TPoint;
  SpeedX, SpeedY: Integer;
begin
  Pnt := ScreenToClient(Mouse.CursorPos);
  Dec(Pnt.X, FMouseNiceScrollPos.X);
  Dec(Pnt.Y, FMouseNiceScrollPos.Y);

  //Perform the scroll only when cursor is out of initial bitmap circle
  if Sqrt(Sqr(Pnt.Y) + Sqr(Pnt.X)) > cBitmapNiceScrollRadius then
  begin
    //Scroll speed should be proportional to the distance between cursor and bitmap center
    SpeedX := Abs(Pnt.X) div cBitmapNiceScrollRadius * cMouseNiceScrollSpeedX;
    SpeedY := Abs(Pnt.Y) div cBitmapNiceScrollRadius * cMouseNiceScrollSpeedY;

    //Top quarter
    if (Pnt.Y <= 0) and (Abs(Pnt.Y) >= Abs(Pnt.X)) then
    begin
      Cursor := crNiceScrollUp;
      PosLineUp(SpeedY);
      Exit;
    end;

    //Bottom quarter
    if (Pnt.Y >= 0) and (Abs(Pnt.Y) >= Abs(Pnt.X)) then
    begin
      Cursor := crNiceScrollDown;
      PosLineDown(SpeedY);
      Exit;
    end;

    //Right quarter
    if (Pnt.X >= 0) and (Abs(Pnt.X) >= Abs(Pnt.Y)) then
    begin
      Cursor := crNiceScrollRight;
      HPosInc(SpeedX);
      Exit;
    end;

    //Left quarter
    if (Pnt.X <= 0) and (Abs(Pnt.X) >= Abs(Pnt.Y)) then
    begin
      Cursor := crNiceScrollLeft;
      HPosDec(SpeedX);
      Exit;
    end;
  end;

  Cursor := crNiceScrollNone;
end;


procedure TATBinHex.MenuItemCopyClick(Sender: TObject);
begin
  CopyToClipboard;
end;

procedure TATBinHex.MenuItemCopyHexClick(Sender: TObject);
begin
  CopyToClipboard(True);
end;

procedure TATBinHex.MenuItemCopyLinkClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := PosURL(MousePosition(FMousePopupPos.X, FMousePopupPos.Y, True));
  if S <> '' then
    SCopyToClipboard(S);
end;

procedure TATBinHex.MenuItemSelectAllClick(Sender: TObject);
begin
  SelectAll;
end;

procedure TATBinHex.MenuItemSelectLineClick(Sender: TObject);
var
  P: Int64;
begin
  with FMousePopupPos do
    if (X >= 0) and (Y >= 0) then
    begin
      P := MousePosition(X, Y);
      if P >= 0 then
        SelectLineAtPos(P, vbLineAll);
    end;
end;

procedure TATBinHex.UpdateMenu(Sender: TObject);
var
  En: Boolean;
begin
  En := not IsFileEmpty;
  FMenuItemCopy.Enabled := En and (FSelLength > 0);
  FMenuItemCopyHex.Enabled := En and (FSelLength > 0) and (FMode = vbmodeHex);
  FMenuItemCopyLink.Enabled := En and IsPosURL(MousePosition(FMousePopupPos.X, FMousePopupPos.Y, True));
  FMenuItemSelectLine.Enabled := En and FEnableSel;
  FMenuItemSelectAll.Enabled := En and FEnableSel and not ((FSelStart = 0) and (FSelLength >= NormalizedPos(FFileSize)));
  FMenuItemEncMenu.Enabled := not IsModeUnicode;

  FMenuItemCopy.Visible := vpCmdCopy in FPopupCommands;
  FMenuItemCopyHex.Visible := vpCmdCopyHex in FPopupCommands;
  FMenuItemCopyLink.Visible := vpCmdCopyLink in FPopupCommands;
  FMenuItemSelectLine.Visible := vpCmdSelectLine in FPopupCommands;
  FMenuItemSelectAll.Visible := vpCmdSelectAll in FPopupCommands;
  FMenuItemEncMenu.Visible := vpCmdEncMenu in FPopupCommands;
end;

procedure TATBinHex.ReadUnicodeFmt;
var
  ABuffer: Word; //2-byte Unicode signature
  ABytesRead: DWORD;
begin
  if FFileUnicodeFmt = vbUnicodeFmtUnknown then
  begin
    FFileUnicodeFmt := vbUnicodeFmtLE;
    if SourceAssigned and (FFileSize >= 2) then
    begin
      if ReadSource(0, @ABuffer, SizeOf(ABuffer), ABytesRead) and
        (ABytesRead >= 2) and (ABuffer = $FFFE) then
        FFileUnicodeFmt := vbUnicodeFmtBE;
    end;
  end;
end;

procedure TATBinHex.HPosAt(APos: Integer; ARedraw: Boolean = True);
begin
  ILimitMin(APos, 0);
  ILimitMax(APos, HPosMax);

  if APos <> FHViewPos then
  begin
    FHViewPos := APos;
    if ARedraw then
      Redraw;
  end;
end;

procedure TATBinHex.HPosInc(N: Integer);
begin
  HPosAt(FHViewPos + N);
end;

procedure TATBinHex.HPosDec(N: Integer);
begin
  HPosAt(FHViewPos - N);
end;

procedure TATBinHex.HPosBegin;
begin
  HPosAt(0);
end;

procedure TATBinHex.HPosEnd;
begin
  HPosAt(MaxInt);
end;

procedure TATBinHex.HPosLeft;
begin
  HPosDec(cArrowScrollSize);
end;

procedure TATBinHex.HPosRight;
begin
  HPosInc(cArrowScrollSize);
end;

procedure TATBinHex.HPosPageLeft;
begin
  HPosDec(ClientWidth);
end;

procedure TATBinHex.HPosPageRight;
begin
  HPosInc(ClientWidth);
end;

function TATBinHex.HPosWidth: Integer;
begin
  Result := TStrPositions(FStrings).GetScreenWidth(
    FBitmap.Canvas, OutputOptions) + FHViewPos;
end;

function TATBinHex.HPosMax: Integer;
begin
  Result := HPosWidth - ClientWidth;
  ILimitMin(Result, 0);
end;


//Note: AStartPos may be equal to FFileSize (without -CharSize).
function TATBinHex.FindLinePos(
  const AStartPos: Int64;
  ADir: TATDirection;
  var ALine: UnicodeString;
  APassiveMove: Boolean = False): Int64;
  //
  function PrevPos(const APos: Int64): Int64;
  begin
    Result := APos - CharSize;
  end;
  //
  function PrevLine(var APos: Int64): Boolean;
  var
    Len2: Integer;
  begin
    Result := True;
    PosFixCRLF(APos);
    if PosBad(PrevPos(APos)) then Exit;
    Len2 := FindLineLength(PrevPos(APos), ADir, ALine);
    Result := (Len2 > 0);
    if Result then
      NextPos(APos, ADir, Len2);
  end;
  //
var
  Len: Integer;
begin
  if (AStartPos < 0) then
    begin Result := -1; Exit end;

  Result := AStartPos;
  NormalizePos(Result);
  PosFixCRLF(Result);
  Len := FindLineLength(Result, ADir, ALine);

  //-------- Move up
  if (ADir = vdirUp) then
  begin
    //handle APassiveMove
    if (Len <= 1) then
      if APassiveMove then
      begin
        if (Len = 0) then PrevLine(Result);
        Exit;
      end;

    //if at line middle, then move to start and exit
    if (Len > 1) then
    begin
      NextPos(Result, ADir, Len - 1);
      if PosBad(Result) then Result := -1;
      Exit;
    end;

    //if at the line start
    if (Len = 1) then
    begin
      NextPos(Result, ADir);
      if PosBad(Result) then begin Result := -1; Exit end;
    end;

    if PrevLine(Result) then Exit;
    if (Len = 0) then
    begin
      NextPos(Result, ADir);
      if PrevLine(Result) then Exit;
    end;
  end
  else

  //-------- Move down
  begin
    //Move to CR
    NextPos(Result, ADir, Len);

    //Skip CR
    case GetChar(Result) of
      #13:
        begin
          NextPos(Result, ADir);
          if GetChar(Result) = #10 then
            NextPos(Result, ADir);
        end;
      #10:
        begin
          NextPos(Result, ADir);
        end;
    end;
  end;

  if PosBad(Result) then Result := -1;
end;

//Note: AStartPos may be equal to FFileSize (without -1).
function TATBinHex.FindLineLength(
  const AStartPos: Int64;
  ADir: TATDirection;
  var ALine: UnicodeString): Integer;
var
  AMaxWidth, i: Integer;
  APos: Int64;
  Dx: TATStringExtent;
  wch: WideChar;
begin
  Result := 0;
  ALine := '';
  if (AStartPos < 0) then Exit;

  APos := AStartPos;
  NormalizePos(APos);

  if (ADir = vdirUp) then
  begin
    I64LimitMax(APos, PosLast);
    if PosBad(APos) then Exit;
  end;

  for i := 1 to FMaxLength do
  begin
    if PosBad(APos) then Break;
    wch := GetChar(APos);
    if SCharCR(wch) then Break;
    ALine := ALine + wch;
    Inc(Result);
    NextPos(APos, ADir);
  end;

  if FTextWrap and (Result > 0) then
  begin
    AMaxWidth := FBitmap.Width - DrawOffsetX;
    if StringWidth(FBitmap.Canvas, ALine, OutputOptions) > AMaxWidth then
      if StringExtent(FBitmap.Canvas, ALine, Dx, OutputOptions) then
      begin
        Result := 1;
        for i := Length(ALine) downto 1 do
          if Dx[i] <= AMaxWidth then
            begin Result := StringWrapPosition(ALine, i); Break end;
        SetLength(ALine, Result);
        SDelLastSpaceW(ALine);
      end;
  end;

  ALine := DecodeString(ALine);
end;

procedure TATBinHex.PosNextLineFrom(
  const AStartPos: Int64;
  ALinesNum: Integer;
  ADir: TATDirection;
  APassiveMove: Boolean = False;
  ARedraw: Boolean = True);
var
  ANewPos: Int64;
  ALine: UnicodeString;
  i: Integer;
begin
  ANewPos := AStartPos;
  NormalizePos(ANewPos);

  NextPos(ANewPos, ADir, ALinesNum * FMaxLength);
  I64LimitMin(ANewPos, 0);
  I64LimitMax(ANewPos, PosLast);

  ReadBuffer(ANewPos);

  ANewPos := AStartPos;
  NormalizePos(ANewPos);

  for i := 1 to ALinesNum do
    ANewPos := FindLinePos(ANewPos, ADir, ALine, APassiveMove);

  if ANewPos < 0 then
  begin
    if ADir = vdirDown then
    begin
      ANewPos := FindLinePos(FFileSize, vdirUp, ALine);
      I64LimitMin(ANewPos, 0);
    end
    else
      ANewPos := 0;
  end;

  FViewPos := ANewPos;
  ReadBuffer(FViewPos);

  if ARedraw then
    Redraw;
end;

procedure TATBinHex.PosNextLine(
  ALinesNum: Integer;
  ADir: TATDirection;
  AViewAtEnd: Boolean);
begin
  if AViewAtEnd and (ADir = vdirDown) then Exit;
  PosNextLineFrom(FViewPos, ALinesNum, ADir);
end;

function TATBinHex.CharSize: Integer;
const
  cSizes: array[TATBinHexMode] of Integer = (1, 1, 1, 2, 2);
begin
  Result := cSizes[FMode];
end;

function TATBinHex.IsFileEmpty: Boolean;
begin
  Result := FFileSize < CharSize;
end;

function TATBinHex.IsModeVariable: Boolean;
begin
  Result := FMode in [vbmodeText, vbmodeUnicode];
end;

function TATBinHex.IsModeUnicode: Boolean;
begin
  Result := FMode in [vbmodeUnicode, vbmodeUHex];
end;

function TATBinHex.IsUnicodeBE: Boolean;
begin
  Result := FFileUnicodeFmt = vbUnicodeFmtBE;
end;

procedure TATBinHex.NormalizePos(var APos: Int64);
begin
  if IsModeUnicode then
    APos := APos div 2 * 2;
end;

function TATBinHex.NormalizedPos(const APos: Int64): Int64;
begin
  Result := APos;
  NormalizePos(Result);
end;

procedure TATBinHex.NextPos(var APos: Int64; ADir: TATDirection; AChars: Integer = 1);
begin
  if ADir = vdirDown then
    Inc(APos, AChars * CharSize)
  else
    Dec(APos, AChars * CharSize);
end;

function TATBinHex.GetChar(const ACharPos: Int64): WideChar;
var
  APos: Int64;
  Ch: AnsiChar;
begin
  Result := #0;
  if IsFileEmpty then Exit;

  if (ACharPos >= 0) and (ACharPos <= PosLast) then
  begin
    APos := ACharPos - FBufferPos;
    Assert((APos >= 0) and (APos < FBufferAllocSize),
      'Buffer position out of range: GetChar');

    if IsModeUnicode then
    begin
      if IsUnicodeBE then
        Result := WideChar(Ord(FBuffer[APos + 1]) + (Ord(FBuffer[APos]) shl 8))
      else
        Result := WideChar(Ord(FBuffer[APos]) + (Ord(FBuffer[APos + 1]) shl 8));
    end
    else
    begin
      Ch := FBuffer[APos];
      Result := WideChar(Ord(Ch)); //DecodeString used later
    end;
  end;

  if (not IsModeVariable) and (Result = #9) then
    Result := cCharSpecial
  else
  if IsCharSpec(Result) then
    Result := cCharSpecial;
end;

function TATBinHex.IsCharSpec(ch: WideChar): boolean;
begin
  Result := False;
  if (Ord(ch) < Ord(FFontFirstChar)) and
    ( (not IsModeVariable) or ((ch <> #13) and (ch <> #10) and (ch <> #9)) ) then
    begin Result := True; Exit end;
  if (Ord(ch) = $1E) or (Ord(ch) = $1F) then //XP/7 don't output these on Courier
    begin Result := True; Exit end;
end;

//decode string from GetChar encoding
function TATBinHex.DecodeString(const S: UnicodeString): UnicodeString;
var
  S2: AnsiString;
  i: integer;
begin
  if IsModeUnicode then
    Result := S
  else
  begin
    S2 := '';
    for i := 1 to Length(S) do
      S2 := S2 + AnsiChar(Ord(S[i]));
    Result := UTF8Decode(SCodepageToUTF8(S2, FTextEncoding));
  end;
end;


function TATBinHex.GetHex(const ACharPos: Int64): UnicodeString;
var
  APos: Int64;
begin
  Result := '';
  if (ACharPos >= 0) and (ACharPos <= PosLast) then
  begin
    APos := ACharPos - FBufferPos;
    Assert(
      (APos >= 0) and (APos < FBufferAllocSize),
      'Buffer position out of range: GetHex');

    Result := IntToHex(Ord(FBuffer[APos]), 2); //Hex of current byte

    if IsModeUnicode then //Add hex of next byte
      if (APos + 1 < FBufferAllocSize) then //Range check
        Result := IntToHex(Ord(FBuffer[APos + 1]), 2) + Result;
  end;
end;


function TATBinHex.ActiveFont: TFont;
begin
  Result := Font;
end;

procedure TATBinHex.Lock;
begin
  Inc(FLockCount);
end;

procedure TATBinHex.Unlock;
begin
  Dec(FLockCount);
end;

function TATBinHex.Locked: Boolean;
begin
  Result := FLockCount > 0;
end;

{$ifdef NOTIF}
procedure TATBinHex.NotifChanged(Sender: TObject);
begin
  //Do not reload when LMB pressed
  if (not cReloadWithLMBPressed) and FMouseDown then
    Exit;

  if not Locked then
    try
      Lock;

      if not IsFileExist(FFileName) then
      begin
        //File is deleted:
        FFileName := '';
        LoadFile(True);
      end
      else
      begin
        //File is changed:
        LoadFile(False);
      end;

      Redraw;
      if FAutoReloadBeep then
        MessageBeep(MB_ICONInformation);
    finally
      Unlock;
    end;

  DoFileReload;
end;
{$endif}

{$ifdef NOTIF}
procedure TATBinHex.DoFileReload;
begin
  if Assigned(FOnFileReload) then
    FOnFileReload(Self);
end;
{$endif}

{$ifdef NOTIF}
procedure TATBinHex.SetAutoReload(AValue: Boolean);
begin
  if FAutoReload <> AValue then
  begin
    FAutoReload := AValue;
    if FFileName <> '' then
      if FAutoReloadSimple then
      begin
        FNotif2.Timer.Enabled := False;
        FNotif2.Timer.Interval := FAutoReloadSimpleTime;
        FNotif2.FileName := FFileName;
        FNotif2.Timer.Enabled := FAutoReload;
      end
      else
      begin
        FNotif.Enabled := False;
        FNotif.FileName := FFileName;
        FNotif.Enabled := FAutoReload;
      end;
  end;
end;
{$endif}

procedure TATBinHex.DoSelectionChange;
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

procedure TATBinHex.SetTabSize(AValue: Integer);
begin
  FTabSize := AValue;
  ILimitMin(FTabSize, cTabSizeMin);
  ILimitMax(FTabSize, cTabSizeMax);
end;

function TATBinHex.GetMaxLengths(AIndex: TATBinHexMode): Integer;
begin
  Result := FMaxLengths[AIndex];
end;

procedure TATBinHex.SetMaxLengths(AIndex: TATBinHexMode; AValue: Integer);
begin
  ILimitMin(AValue, cMinLength);
  ILimitMax(AValue, cMaxLength);
  FMaxLengths[AIndex] := AValue;
  SetTextWidth(FTextWidth);
  SetTextWidthHex(FTextWidthHex);
  SetTextWidthUHex(FTextWidthUHex);
end;

procedure TATBinHex.IncreaseFontSize(AIncrement: Boolean);
begin
  if AIncrement then
  begin
    Font.Size := Font.Size+1;
  end
  else
  begin
    if Font.Size >= 6 then
      Font.Size := Font.Size-1;
  end;

  Redraw;
  DoOptionsChange;
end;

{$ifdef SEARCH}
function TATBinHex.GetOnSearchProgress: TATStreamSearchProgress;
begin
  Result := FSearch.OnProgress;
end;

procedure TATBinHex.SetOnSearchProgress(AValue: TATStreamSearchProgress);
begin
  FSearch.OnProgress := AValue;
end;

function TATBinHex.GetSearchResultStart: Int64;
begin
  Result := FSearch.FoundStart;
end;

function TATBinHex.GetSearchResultLength: Int64;
begin
  Result := FSearch.FoundLength;
end;

function TATBinHex.GetSearchString: string;
begin
  Result := FSearch.SavedText;
end;
{$endif}

{$ifdef SEARCH}
function TATBinHex.FindFirst(
  const AText: string;
  AOptions: TATStreamSearchOptions;
  const AFromPos: Int64 = -1): Boolean;
var
  AStreamEncoding: TEncConvId;
  AStartPos: Int64;
begin
  Assert(SourceAssigned, 'Source not assigned: FindFirst');

  //Handle encoding:
  if IsModeUnicode then
  begin
    //if IsUnicodeBE then
    //  AStreamEncoding := vencUnicodeBE
    //else
    //  AStreamEncoding := vencUnicodeLE;
    AStreamEncoding := eidCP1252;
  end
  else
  begin
    AStreamEncoding := FTextEncoding;
  end;

  //Handle "Origin" option:
  if (asoFromPos in AOptions) and (AFromPos >= 0) then
    AStartPos := AFromPos
  else
  if not (asoFromPage in AOptions) then
    AStartPos := 0 //0 is valid for both directions
  else
  begin
    if not (asoBackward in AOptions) then
      AStartPos := FViewPos //Forward: page start position
    else
      AStartPos := FViewPos + FViewPageSize; //Backward: page end position
  end;

  try
    case FFileSourceType of
      //vfSrcFile:
      //  FSearch.FileName := FFileName;
      vfSrcStream:
        FSearch.Stream := FStream;
    end;
    FSearchStarted := True;
  except
    MsgOpenError;
    Result := False;
    Exit;
  end;

  Result := FSearch.FindFirst(AText, AStartPos, AStreamEncoding, CharSize, AOptions);
  if Result then
    SetSelection(FSearch.FoundStart, FSearch.FoundLength, true);
end;
{$endif}

{$ifdef SEARCH}
function TATBinHex.FindNext(AFindPrevious: Boolean = False): Boolean;
begin
  Assert(SourceAssigned, 'Source not assigned: FindNext');
  Assert(FSearchStarted, 'Search not started: FindNext');
  Result := FSearch.FindNext(AFindPrevious);
  if Result then
    SetSelection(FSearch.FoundStart, FSearch.FoundLength, true);
end;
{$endif}

procedure TATBinHex.SetMaxClipboardDataSizeMb(AValue: Integer);
begin
  ILimitMin(AValue, cMaxClipboardDataSizeMbMin);
  ILimitMax(AValue, cMaxClipboardDataSizeMbMax);
  FMaxClipboardDataSizeMb := AValue;
end;

procedure TATBinHex.DoOptionsChange;
begin
  if Assigned(FOnOptionsChange) then
    FOnOptionsChange(Self);
end;

procedure TATBinHex.DoOnScroll;
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self);
end;

procedure TATBinHex.SetEnabled(AValue: Boolean);
begin
  inherited;
  Redraw;
end;

procedure TATBinHex.SetEnabled2(AValue: Boolean);
begin
  if AValue then
  begin
    FEnabled2 := AValue;
    Enabled := AValue;
  end
  else
  begin
    Enabled := AValue;
    FEnabled2 := AValue;
  end;
end;

procedure TATBinHex.SetMouseNiceScroll(AValue: Boolean);
begin
  if FMouseNiceScroll <> AValue then
  begin
    FMouseNiceScroll := AValue;
    FTimerNiceScroll.Enabled := AValue;
    Cursor := crIBeam;
    Redraw;
  end;
end;

procedure TATBinHex.DrawNiceScroll;
begin
  if MouseNiceScroll then
    Canvas.Draw(
      FMouseNiceScrollPos.X - cBitmapNiceScrollRadius,
      FMouseNiceScrollPos.Y - cBitmapNiceScrollRadius,
      FBitmapNiceScroll);
end;

procedure TATBinHex.ExitProc(Sender: TObject);
begin
  MouseNiceScroll := False;
end;

//--------------------------------------------------------
{
const
  cUnicodeFormatList: array[Boolean] of TATUnicodeFormat = (vbUnicodeFmtLE, vbUnicodeFmtBE);
}

procedure TATBinHex.SetFileUnicodeFmt(AValue: TATUnicodeFormat);
begin
  FFileUnicodeFmt := AValue;
  if IsModeUnicode then
    Redraw;
end;

procedure TATBinHex.EncodingMenuItemClick(Sender: TObject);
begin
  if Sender is TMenuItem then
  begin
    TextEncoding := TEncConvId((Sender as TMenuItem).Tag);
    DoOptionsChange;
  end;
end;

procedure TATBinHex.Reload;
begin
  Assert(SourceAssigned, 'Source not assigned: Reload');
  SetMode(FMode);
end;

procedure TATBinHex.SetTextOemSpecial(AValue: Boolean);
begin
  if FTextOemSpecial <> AValue then
  begin
    FTextOemSpecial := AValue;
    Redraw;
  end;
end;

procedure TATBinHex.SetTextUrlHilight(AValue: Boolean);
begin
  if FUrlShow <> AValue then
  begin
    FUrlShow := AValue;
    Redraw;
  end;
end;

procedure TATBinHex.SetTextNonPrintable(AValue: Boolean);
begin
  if FTextNonPrintable <> AValue then
  begin
    FTextNonPrintable := AValue;
    Redraw;
  end;
end;

//Determine, is ALine, found at position APos, ended with CR/LF
function TATBinHex.LineWithCR(const APos: Int64; const ALine: UnicodeString): Boolean;
var
  P: Int64;
begin
  P := APos + Length(ALine) * CharSize;
  Result := (not PosBad(P)) and SCharCR(GetChar(P));
end;

//Determine, is position APos starts a new line
function TATBinHex.LineWithGutterDot(const APos: Int64): Boolean;
var
  P: Int64;
begin
  P := APos - CharSize;
  Result := PosBad(P) or SCharCR(GetChar(P));
end;

procedure TATBinHex.SetTextPopupCaption(AIndex: TATPopupCommand; const AValue: AnsiString);
begin
  case AIndex of
    vpCmdCopy:
      FMenuItemCopy.Caption := AValue;
    vpCmdCopyHex:
      FMenuItemCopyHex.Caption := AValue;
    vpCmdCopyLink:
      FMenuItemCopyLink.Caption := AValue;
    vpCmdSelectLine:
      FMenuItemSelectLine.Caption := AValue;
    vpCmdSelectAll:
      FMenuItemSelectAll.Caption := AValue;
    vpCmdEncMenu:
      FMenuItemEncMenu.Caption := AValue;
  end;
end;

function TATBinHex.GetTextPopupCaption(AIndex: TATPopupCommand): AnsiString;
begin
  case AIndex of
    vpCmdCopy:
      Result := FMenuItemCopy.Caption;
    vpCmdCopyHex:
      Result := FMenuItemCopyHex.Caption;
    vpCmdCopyLink:
      Result := FMenuItemCopyLink.Caption;
    vpCmdSelectLine:
      Result := FMenuItemSelectLine.Caption;
    vpCmdSelectAll:
      Result := FMenuItemSelectAll.Caption;
    vpCmdEncMenu:
      Result := FMenuItemEncMenu.Caption;
    else
      Result := '';  
  end;
end;

function TATBinHex.DrawOffsetX: Integer;
begin
  Result := cDrawOffsetMinX;
  if FTextGutter then
    Inc(Result, FTextGutterWidth);
end;

function TATBinHex.DrawOffsetY: Integer;
begin
  Result := cDrawOffsetMinY;
end;

procedure TATBinHex.SetTextGutter(AValue: Boolean);
begin
  if FTextGutter <> AValue then
  begin
    FTextGutter := AValue;
  end;
end;


//--------------------------------------
//Printing code is in separate file:
{$ifdef PRINT}
{$I ATBinHexPrint.inc}
{$endif}


//--------------------------------------
function TATBinHex.CountLines(ABufSize: Integer): Boolean;
var
  Buf: PAnsiChar;
  AOffset: Int64;
  //------------
  function AChar(const APos: Int64): AnsiChar;
  begin
    Result := #0;
    if IsModeUnicode then
    begin
      //We need to get only #13, #10 (when other byte = #0)
      //so we don't need WideChar
      if (IsUnicodeBE) then
      begin
        if Buf[APos] = #0 then
          Result := Buf[APos + 1];
      end
      else
      begin
        if Buf[APos + 1] = #0 then
          Result := Buf[APos];
      end;
    end
    else
      Result := Buf[APos];
  end;
  //------------
  procedure AInc;
  begin
    Inc(AOffset, CharSize);
  end;
  //------------
var
  Pos1, Pos2: Int64;
  Read: DWORD;
begin
  FLinesNum := 0;

  if Assigned(FLinesData) then
    FreeMem(FLinesData);
  GetMem(FLinesData, SizeOf(Integer) * FLinesCount);

  GetMem(Buf, ABufSize);

  try
    Result := ReadSource(0, Buf, ABufSize, Read);
    if Result then
    begin
      AOffset := 0;
      Pos1 := Int64(Read) - CharSize;
      Pos2 := Int64(Read) - 2 * CharSize;
      NormalizePos(Pos1);
      NormalizePos(Pos2);

      repeat
        while (AOffset < Pos1) and (not (AChar(AOffset) in [#13, #10])) do AInc; //Go to #13, #10
        if (AOffset < Pos2) and (AChar(AOffset) = #13) and (AChar(AOffset + CharSize) = #10) then AInc; //Additionally skip #13#10
        AInc;

        //Break if EOF
        if (AOffset > Pos1) then Break;

        if (FLinesNum >= Pred(FLinesCount)) then Break;
        Inc(FLinesNum);
        FLinesData^[FLinesNum] := AOffset;
      until False;
    end;
  finally
    FreeMem(Buf);
  end;
end;

//-------------------------------
//Get offset by line number.
//To find current line, call with ALine = 0, AFindLine = True.
function TATBinHex.GetLineNumberOffset(
  ALine: Integer; AFindLine: Boolean;
  var ACurrentLine: Integer; var AOffset: Int64): Boolean;
var
  i: Integer;
begin
  ACurrentLine := 0;
  AOffset := 0;

  Result := FLinesNum > 0;
  if Result then
    if AFindLine then
    begin
      for i := Low(TIntegerArray) to FLinesNum do
        if (FLinesData^[i] > FViewPos) then begin ACurrentLine := i; Break end;
    end
    else
    begin
      if (ALine <= 1) then AOffset := 0
      else
      if (ALine > FLinesNum) then AOffset := FLinesData^[FLinesNum]
      else
        AOffset := FLinesData^[ALine - 1];
    end;
end;


//-------------------------------
procedure TATBinHex.SetPosLine(ALine: Integer);
var
  ACurLine: Integer;
  AOffset: Int64;
begin
  //Count lines always
  CountLines(FLinesBufSize);
  if GetLineNumberOffset(ALine, False, ACurLine, AOffset) then
    SetPosOffset(AOffset);
end;

function TATBinHex.GetPosLine: Integer;
var
  ACurLine: Integer;
  AOffset: Int64;
begin
  //Count lines if they weren't counted
  if (FLinesNum = 0) then
    CountLines(FLinesBufSize);

  GetLineNumberOffset(0, True, ACurLine, AOffset);
  Result := ACurLine;
end;

//----------------------------
function TATBinHex.FindLineNum(const AOffset: Int64): Integer;
var
  i: Integer;
begin
  Result := 0;
  if AOffset = 0 then Result := 1 else
    for i := Low(TIntegerArray) to FLinesNum do
      if (FLinesData^[i] = AOffset) then begin Result := i + 1; Break end;
end;

//----------------------------
procedure TATBinHex.SetLinesBufSize(AValue: Integer);
begin
  FLinesBufSize := AValue;
  ILimitMin(FLinesBufSize, cLinesBufSizeMin);
  ILimitMax(FLinesBufSize, cLinesBufSizeMax);
end;

procedure TATBinHex.SetLinesCount(AValue: Integer);
begin
  FLinesCount := AValue;
  ILimitMin(FLinesCount, cLinesCountMin);
  ILimitMax(FLinesCount, cLinesCountMax);
end;

procedure TATBinHex.SetLinesStep(AValue: Integer);
begin
  FLinesStep := AValue;
  ILimitMin(FLinesStep, cLinesStepMin);
  ILimitMax(FLinesStep, cLinesStepMax);
end;

//----------------------------
function TATBinHex.ActiveLinesShow: Boolean;
begin
  Result :=
    IsModeVariable and
    FTextGutter and
    FLinesShow and
    ((not FLinesExtUse) or (SFileExtensionMatch(FFileName, FLinesExtList)));
end;


procedure TATBinHex.DoDrawLine(ACanvas: TCanvas; const AStr: UnicodeString;
  const APos: Int64; const ARect: TRect; const ATextPnt: TPoint;
  var ADone: Boolean);
begin
  if Assigned(FOnDrawLine) then
    FOnDrawLine(
      Self,
      ACanvas,
      SConvertForOut(AStr, OutputOptions(False)),
      StringAtPos(APos),
      ARect,
      ATextPnt,
      ADone);
end;

procedure TATBinHex.DoDrawLine2(ACanvas: TCanvas; const AStr: UnicodeString;
  const APnt: TPoint; const AOptions: TATBinHexOutputOptions);
begin
  if Assigned(FOnDrawLine2) then
    FOnDrawLine2(
      Self,
      ACanvas,
      SConvertForOut(AStr, OutputOptions(False)),
      APnt,
      AOptions);
end;

procedure TATBinHex.DoClickURL(const AMousePos: Int64);
var
  S: AnsiString;
begin
  if Assigned(FOnClickURL) then
  begin
    S := PosURL(AMousePos);
    if S <> '' then
    begin
      if not ((Pos('://', S) > 0) or (Pos('www.', S) = 1)) then
        S := 'mailto:' + S;
      FOnClickURL(Self, S);
    end;
  end;
end;

procedure TATBinHex.Resize;
begin
  BitmapResizeBySteps(FBitmap, Width, Height, 50, 50);

  //Notepad feature: when control increases height and
  //file was at the end, then file is scrolled again to the end.
  if FResizeFollowTail then
    if (Height > FClientHeight) and FViewAtEnd then
      PosEnd;

  //Update last height
  FClientHeight := Height;

  Redraw(False);

  inherited;
end;


procedure TATBinHex.InitURLs;
var
  i: Integer;
begin
  for i := Low(FUrlArray) to High(FUrlArray) do
    with FUrlArray[i] do
      begin FString := ''; FPos := 0; end;
end;

{$ifdef search}
procedure TATBinHex.FindAll;
var
  MS: TMemoryStream;
  n: integer;
  p, pp: Int64;
begin
  FillChar(FFindArray, SizeOf(FFindArray), 0);

  if FSearch.SavedText = '' then Exit;
  if not (asoShowAll in FSearch.SavedOptions) then Exit;
  if FBuffer = nil then Exit;
  if FBufferAllocSize = 0 then Exit;

  n := 0;
  p := FViewPos - FBufferPos - cFindGap;
  pp := LinesNum * FMaxLengths[FMode] + 2 * cFindGap;
  I64LimitMin(p, 0);
  I64LimitMax(pp, FBufferAllocSize - p);

  MS := TMemoryStream.Create;
  try
    MS.WriteBuffer((@FBuffer[p])^, pp);
    FSearch2.Stream := MS;

    if FSearch2.FindFirst(
      FSearch.SavedText, 0,
      FSearch.SavedEncoding,
      CharSize,
      FSearch.SavedOptions - [asoBackward]) then
    repeat
      Inc(n);
      if n > High(FFindArray) then Break;
      FFindArray[n].FPos := FSearch2.FoundStart + FBufferPos + p;
      FFindArray[n].FLen := FSearch2.FoundLength div CharSize;
    until not FSearch2.FindNext;
  finally
    MS.Free;
  end;
end;
{$endif}

{$ifdef REGEX}
procedure TATBinHex.FindURLs(ABufSize: DWORD);
  procedure ShowURLs;
  var
    s: string;
    i: Integer;
  begin
    s := '';
    for i := 1 to 5 do
      with FUrlArray[i] do
        s := s + Format('"%s" (%d)'#13, [Copy(FString, 1, 50), FPos]);
    MsgInfo(s);
  end;
var
  RegEx: TDIRegEx;
  Text: string;
  SS: TATStreamSearch;
  MS: TMemoryStream;
  Enc: TATEncoding;
  i: Integer;
begin
  InitURLs;
  Text := Format('%s|%s', [cReUrl, cReEmail]);

  if IsModeUnicode then
  begin
    //Unicode encodings
    SS := TATStreamSearch.Create(nil);
    MS := TMemoryStream.Create;
    MS.WriteBuffer(FBuffer^, ABufSize);

    i := Pred(Low(FUrlArray));
    if IsUnicodeBE then
      Enc := vEncUnicodeBE
    else
      Enc := vEncUnicodeLE;

    try
      SS.Stream := MS;
      if SS.FindFirst(Text{'\w+'}, 0, Enc, [asoRegEx]) then
        repeat
          Inc(i);
          if i > High(FUrlArray) then Break;
          FUrlArray[i].FString := SetStringW(@FBuffer[SS.FoundStart], SS.FoundLength, IsUnicodeBE);
          FUrlArray[i].FPos := FBufferPos + SS.FoundStart;
        until not SS.FindNext;
    finally
      SS.Stream := nil;
      MS.Free;
      SS.Free;
    end;
  end
  else
  begin
    //1-byte encodings
    RegEx := TDIPerlRegEx.Create(nil);
    try
      RegEx.CompileOptions := RegEx.CompileOptions - [coDotAll] + [coCaseLess];
      RegEx.MatchPattern := Text;
      i := Pred(Low(FUrlArray));

      if RegEx.MatchBuf(FBuffer^, ABufSize, 0) >= 0 then
        repeat
          Inc(i);
          if i > High(FUrlArray) then Break;
          FUrlArray[i].FString := RegEx.MatchedStr;
          FUrlArray[i].FPos := FBufferPos + RegEx.MatchedStrFirstCharPos;
        until RegEx.MatchNext < 0;
    finally
      RegEx.Free;
    end;
  end;
end;
{$else}
procedure TATBinHex.FindURLs(ABufSize: DWORD);
begin
end;
{$endif}


function TATBinHex.PosURL(const APos: Int64): AnsiString;
var
  i: Integer;
begin
  Result := '';
  if APos >= 0 then
    for i := Low(FUrlArray) to High(FUrlArray) do
      with FUrlArray[i] do
      begin
        if (FString = '') then Break;
        if (APos >= FPos) and (APos < FPos + Length(FString) * CharSize) then
          begin Result := FString; Break end;
      end;
end;

function TATBinHex.IsPosURL(const APos: Int64): Boolean;
begin
  Result := PosURL(APos) <> '';
end;

procedure InitCursors;
begin
  FBitmapNiceScroll := TPortableNetworkGraphic.Create;
  FBitmapNiceScroll.LoadFromResourceName(HInstance, 'AB_MOVE');

  with Screen do
  begin
    Cursors[crNiceScrollNone]  := LoadCursor(HInstance, 'AB_MOVE');
    Cursors[crNiceScrollUp]    := LoadCursor(HInstance, 'AB_MOVE_U');
    Cursors[crNiceScrollDown]  := LoadCursor(HInstance, 'AB_MOVE_D');
    Cursors[crNiceScrollLeft]  := LoadCursor(HInstance, 'AB_MOVE_L');
    Cursors[crNiceScrollRight] := LoadCursor(HInstance, 'AB_MOVE_R');
  end;
end;


procedure TATBinHex.DoAddEncodingItem(AMenu: TMenuItem; Sub, SName: string; AEncId: TEncConvId);
var
  mi, miSub: TMenuItem;
  n: integer;
begin
  miSub:= nil;
  if Sub='eu' then Sub:= 'European' else
   if Sub='as' then Sub:= 'Asian' else
    if Sub='mi' then Sub:= 'Misc';

  if Sub<>'' then
  begin
    n:= AMenu.IndexOfCaption(Sub);
    if n<0 then
    begin
      mi:= TMenuItem.Create(Self);
      mi.Caption:= Sub;
      AMenu.Add(mi);
      n:= AMenu.IndexOfCaption(Sub);
    end;
    miSub:= AMenu.Items[n]
  end;

  if miSub=nil then miSub:= AMenu;
  mi:= TMenuItem.Create(Self);
  mi.Caption:= SName;
  mi.Tag:= Ord(AEncId);
  mi.OnClick:= EncodingMenuItemClick;

  miSub.Add(mi);
end;

procedure TATBinHex.UpdateMenuEncodings(AMenu: TMenuItem);
var
  i: integer;
begin
  AMenu.Clear;
  for i:= Low(AppEncodings) to High(AppEncodings) do
  begin
    DoAddEncodingItem(AMenu,
      AppEncodings[i].Sub,
      AppEncodings[i].Name,
      AppEncodings[i].Id
      );
  end;
end;


{ Initialization }
initialization
  InitCursors;

finalization
  FreeAndNil(FBitmapNiceScroll);

end.
