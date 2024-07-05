{************************************************}
{                                                }
{  ATStreamSearch Component                      }
{  Copyright (C) 2007-2008 Alexey Torgashin      }
{                                                }
{************************************************}

{$BOOLEVAL OFF} //Short boolean evaluation required.

{$I ATStreamSearchOptions.inc} //ATStreamSearch options.

unit ATStreamSearch;

interface

uses
  Classes,
  {$IFDEF REGEX}
  DIRegEx,
  {$ENDIF}
  {$IFDEF TNT}
  TntClasses,
  {$ENDIF}
  atbinhex_encoding,
  EncConv,
  Math;

type
  TATStreamSearchOption = (
    asoCaseSens,
    asoWholeWords,
    asoBackward,
    {$IFDEF REGEX} asoRegEx, {$ENDIF}
    {$IFDEF REGEX} asoRegExMLine, {$ENDIF}
    asoShowAll, //ATBinHex only
    asoInSelection //ATBinHex only
    );

  TATStreamSearchOptions = set of TATStreamSearchOption;

  TATStreamSearchProgress = procedure(
    const ACurrentPos, AMaximalPos: Int64;
    var AContinueSearching: Boolean) of object;

type

  { TATStreamSearch }

  TATStreamSearch = class(TComponent)
  private
    FSavedEncoding: TEncConvId;
    FStream: TStream;
    FStreamOwner: Boolean;
    FFileName: string;
    FStreamStart: Int64;
    FStreamSize: Int64;
    FFoundStart: Int64;
    FFoundLength: Int64;

    FSavStart: Int64;
    FSavLen: Int64;
    FSavText: string;
    FSavOpt: TATStreamsearchoptions;
    FSavEnc: TEncConvId;

    {$IFDEF REGEX}
    FRegEx: TDIRegExSearchStream_Enc;
    {$ENDIF}
    FOnProgress: TATStreamSearchProgress;
    FCharSize: Integer;

    FSavedText: string;
    FSavedTextLen: integer;
    FSavedOptions: TATStreamSearchOptions;
    //FSearchForValidUTF16: Boolean;
    FSavedStartPos: Int64;
    FSavedEndPos: Int64;

    procedure FreeStream;
    procedure InitSavedOptions;
    function InitProgressFields(
      const AStartPos: Int64;
      AEncoding: TEncConvId): Boolean;
    procedure DoProgress(
      const ACurrentPos, AMaximalPos: Int64;
      var AContinueSearching: Boolean);
    procedure SetFileName(const AFileName: string);
    procedure SetStream(AStream: TStream);

    {$IFDEF REGEX}
    procedure FreeRegex;
    procedure InitRegex;
    procedure RegexProgress(
      const ASender: TDICustomRegExSearch;
      const AProgress: Int64;
      var AAbort: Boolean);
    function RegexFindFirst(
      const AText: string;
      const AStartPos: Int64;
      AEncoding: TATEncoding;
      AOptions: TATStreamSearchOptions): Boolean;
    function RegexFindNext: Boolean;
    {$ENDIF}

    function TextFind(
      const AText: string;
      const AStartPos, AEndPos: Int64;
      AEncoding: TEncConvId;
      AOptions: TATStreamSearchOptions): Int64;
    function TextFindFirst(
      const AText: string;
      const AStartPos, AEndPos: Int64;
      AEncoding: TEncConvId;
      AOptions: TATStreamSearchOptions): Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveOptions;
    procedure RestoreOptions;

    property FileName: string read FFileName write SetFileName;
    property Stream: TStream read FStream write SetStream;
    property SavedText: string read FSavedText;
    property SavedEncoding: TEncConvId read FSavedEncoding;
    property SavedOptions: TATStreamSearchOptions read FSavedOptions;
    property SavedStartPos: Int64 read FSavedStartPos;
    property SavedEndPos: Int64 read FSavedEndPos;

    function Find(
      const AText: string;
      const AStartPos, AEndPos: Int64;
      AEncoding: TEncConvId;
      ACharSize: integer;
      AOptions: TATStreamSearchOptions): Boolean;

    property FoundStart: Int64 read FFoundStart write FFoundStart;
    property FoundLength: Int64 read FFoundLength write FFoundLength;

  published
    //property SearchForValidUTF16: Boolean read FSearchForValidUTF16 write FSearchForValidUTF16 default False;
    property OnProgress: TATStreamSearchProgress read FOnProgress write FOnProgress;
  end;

var
  MsgATStreamSearchRegExError: AnsiString = 'Regular expression pattern error:'#13#10#13#10'%s at offset %d';
  MsgATStreamSearchReadError: AnsiString = 'Read error at offset %d';

procedure Register;

implementation

uses
  {$IFDEF REGEX}
  DIRegEx_Api, DIRegEx_SearchStream, DIUtils,
  {$ENDIF}
  SysUtils, atbinhex_strproc;

{ Constants }

const
  cBlockSize = 64 * 1024;

{ Helper functions }

function BoolToSign(AValue: Boolean): Integer;
begin
  if AValue then
    Result := 1
  else
    Result := -1;
end;

procedure NormalizePos(var APos: Int64; ACharSize: Integer);
begin
  if ACharSize <> 1 then
    APos := APos div ACharSize * ACharSize;
end;

function LastPos(const AFileSize: Int64; ACharSize: Integer): Int64;
begin
  Result := AFileSize;
  NormalizePos(Result, ACharSize);
  Dec(Result, ACharSize);
  I64LimitMin(Result, 0);
end;

{ TATStreamSearch }

constructor TATStreamSearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStream := nil;
  FStreamOwner := False;
  FFileName := '';
  FStreamStart := -1;
  FStreamSize := 0;
  FFoundStart := -1;
  FFoundLength := 0;
  //FSearchForValidUTF16 := False;

  {$IFDEF REGEX}
  FRegEx := nil;
  {$ENDIF}

  FOnProgress := nil;
  FCharSize:= 1;
  InitSavedOptions;
end;

destructor TATStreamSearch.Destroy;
begin
  FreeStream;
  {$IFDEF REGEX}
  FreeRegex;
  {$ENDIF}
  inherited;
end;

procedure TATStreamSearch.FreeStream;
begin
  if FStreamOwner then
    if Assigned(FStream) then
      FreeAndNil(FStream);
end;

procedure TATStreamSearch.InitSavedOptions;
begin
  FSavedText := '';
  FSavedTextLen := 0;
  FSavedEncoding := eidCP1252;
  FSavedOptions := [];
  FSavedStartPos := 0;
  FSavedEndPos := High(Int64);
end;

procedure TATStreamSearch.SetFileName(const AFileName: string);
begin
  FreeStream;

  if AFileName <> '' then
    begin
      InitSavedOptions;
      FFileName := AFileName;
      FStreamOwner := True;
      FStream := {$IFDEF TNT}TTntFileStream{$ELSE}TFileStream{$ENDIF}.Create(
        AFileName, fmOpenRead or fmShareDenyNone);
    end;
end;

procedure TATStreamSearch.SetStream(AStream: TStream);
begin
  FreeStream;
  InitSavedOptions;
  FFileName := '';
  FStreamOwner := False;
  FStream := AStream;
end;

function TATStreamSearch.InitProgressFields(const AStartPos: Int64;
  AEncoding: TEncConvId): Boolean;
begin
  Assert(Assigned(FStream));
  FStreamStart := AStartPos;
  FStreamSize := FStream.Size;
  Result := FStreamSize >= FCharSize;
end;

procedure TATStreamSearch.DoProgress(
  const ACurrentPos, AMaximalPos: Int64;
  var AContinueSearching: Boolean);
begin
  AContinueSearching := True;

  if Assigned(FOnProgress) then
    FOnProgress(ACurrentPos, AMaximalPos, AContinueSearching);
end;

//-----------------------------------------------------------------
// RegEx-related code

{$IFDEF REGEX}

procedure TATStreamSearch.FreeRegex;
begin
  if Assigned(FRegEx) then
    FreeAndNil(FRegEx);
end;

procedure TATStreamSearch.InitRegex;
begin
  if not Assigned(FRegEx) then
    begin
      FRegEx := TDIRegExSearchStream_Enc.Create(Self);
      FRegEx.MatchOptions := FRegEx.MatchOptions - [moNotEmpty];
      FRegEx.OnProgress := RegexProgress;
    end;
end;

procedure TATStreamSearch.RegexProgress(
  const ASender: TDICustomRegExSearch;
  const AProgress: Int64;
  var AAbort: Boolean);
var
  ContinueSearching: Boolean;
begin
  ContinueSearching := True;
  DoProgress(
    FStreamStart + AProgress,
    FStreamSize,
    ContinueSearching);
  if not ContinueSearching then
    AAbort := True;
end;

function TATStreamSearch.RegexFindFirst(
  const AText: string;
  const AStartPos: Int64;
  AEncoding: TATEncoding;
  AOptions: TATStreamSearchOptions): Boolean;
var
  RealText: AnsiString;
begin
  Result := False;
  if AText = '' then Exit;

  //1. Prepare objects and fields

  InitRegex;

  Assert(Assigned(FRegEx), 'RegEx object not initialized');
  Assert(Assigned(FStream), 'Stream object not initialized');

  if not InitProgressFields(AStartPos, AEncoding) then Exit;

  FStream.Position := AStartPos;

  //2. Prepare RegEx object

  if asoCaseSens in AOptions then
    FRegEx.CompileOptions := FRegEx.CompileOptions - [coCaseLess]
  else
    FRegEx.CompileOptions := FRegEx.CompileOptions + [coCaseLess];

  if asoRegExMLine in AOptions then
    FRegEx.CompileOptions := FRegEx.CompileOptions + [coDotAll]
  else
    FRegEx.CompileOptions := FRegEx.CompileOptions - [coDotAll];

  RealText := StrEncodeUtf8(AText);

  if asoWholeWords in AOptions then
    begin
      //If "Whole Words" option is used, we first need to check
      //validity of original regex:
      if not FRegEx.CompileMatchPatternStr(RealText) then
        begin
          raise Exception.Create(Format(MsgATStreamSearchRegExError,
            [FRegEx.ErrorMessage, FRegEx.ErrorOffset]));
          Exit;
        end;
      //If it's OK we append '\b...\b' and compile regex again:
      RealText := '\b' + RealText + '\b';
    end;

  if not FRegEx.CompileMatchPatternStr(RealText) then
    begin
      raise Exception.Create(Format(MsgATStreamSearchRegExError,
        [FRegEx.ErrorMessage, FRegEx.ErrorOffset]));
      Exit;
    end;

  case AEncoding of
    vencANSI:
      FRegEx.SearchInitEnc(FStream, ansi_mbtowc);
    vencOEM:
      FRegEx.SearchInitEnc(FStream, oem_mbtowc);
    vencUnicodeLE:
      begin
        {if FSearchForValidUTF16 then
          FRegex.SearchInitEnc(FStream, utf16le_mbtowc)
        else}
        FRegEx.SearchInitEnc(FStream, binary16le_mbtowc);
      end;
    vencUnicodeBE:
      begin
        {if FSearchForValidUTF16 then
          FRegex.SearchInitEnc(FStream, utf16be_mbtowc)
        else}
        FRegEx.SearchInitEnc(FStream, binary16be_mbtowc);
      end;
    else
      Assert(False, 'Unknown encoding specified');
  end;

  //3. Search

  Result := RegexFindNext;
end;

function TATStreamSearch.RegexFindNext: Boolean;
var
  DummyStart, DummyLength,
  MatchStart, MatchLength: Int64;
begin
  Assert(Assigned(FRegEx), 'RegEx object not initialized');
  Assert(Assigned(FStream), 'Stream object not initialized');

  Result := FRegEx.SearchNext(
    DummyStart, DummyLength,
    MatchStart, MatchLength) >= 0;

  if Result then
    begin
      FFoundStart := FStreamStart + MatchStart * FCharSize;
      FFoundLength := MatchLength * FCharSize;
    end
  else
    begin
      FFoundStart := -1;
      FFoundLength := 0;
    end;
end;

{$ENDIF}

//-----------------------------------------------------------------
// Plain search code

function TATStreamSearch.TextFind(
  const AText: string;
  const AStartPos, AEndPos: Int64;
  AEncoding: TEncConvId;
  AOptions: TATStreamSearchOptions): Int64;
var
  Buffer: array[0 .. cBlockSize - 1] of char;
  BufPosMin, BufPosMax, TotalMax, BufPos, ReadPos: Int64;
  ReadSize, BytesRead: DWORD;
  SBufferA: string;
  SBufferW: UnicodeString;
  TextInCodepage: string;
  StringPos: Integer;
  bForward, bWholeWords, bCaseSens, bContinue: Boolean;
begin
  Result := -1;

  if AText = '' then Exit;

  //1. Init objects and fields

  Assert(Assigned(FStream), 'Stream object not initialized');

  if not InitProgressFields(AStartPos, AEncoding) then Exit;

  //2. Init variables

  bForward := not (asoBackward in AOptions);
  bWholeWords := asoWholeWords in AOptions;
  bCaseSens := asoCaseSens in AOptions;

  BufPosMin := Min(AStartPos, AEndPos);
  BufPosMax := Max(AStartPos, AEndPos);

  TotalMax := LastPos(FStreamSize, FCharSize);
  NormalizePos(TotalMax, FCharSize);
  if BufPosMax > TotalMax then
    BufPosMax := TotalMax;

  BufPos := AStartPos;
  NormalizePos(BufPos, FCharSize);

  if FCharSize=1 then
    TextInCodepage:= SCodepageFromUTF8(AText, AEncoding)
  else
    TextInCodepage:= '';

  if BufPos > BufPosMax then
    begin
      if bForward then
        Exit
      else
        BufPos := BufPosMax;
    end;

  if BufPos < BufPosMin then
    begin
      if bForward then
        BufPos := BufPosMin
      else
        Exit;
    end;

  //3. Search

  bContinue := True;
  DoProgress(BufPos, FStreamSize, bContinue);
  if not bContinue then Exit;

  repeat
    ReadPos := BufPos;
    ReadSize := cBlockSize;

    if not bForward then
      begin
        Dec(ReadPos, cBlockSize - FCharSize);
        I64LimitMin(ReadPos, 0);

        ReadSize := BufPos - ReadPos + FCharSize;
        if ReadSize > cBlockSize then
          ReadSize := cBlockSize;
      end
    else
      begin
        // do not find after AEndPos
        ReadSize := Min(Int64(ReadSize), AEndPos - ReadPos);
        if ReadSize <= 0 then Exit;
      end;

    try
      FillChar(Buffer, SizeOf(Buffer), 0);
      FStream.Position := ReadPos;
      BytesRead := FStream.Read(Buffer, ReadSize);
    except
      raise Exception.Create(Format(MsgATStreamSearchReadError, [ReadPos]));
      Exit;
    end;

    if FCharSize = 2 then
      begin
        SBufferW := SetStringW(@Buffer, BytesRead, false);
        StringPos := SFindTextW(AText, SBufferW, bForward, bWholeWords, bCaseSens, BytesRead < cBlockSize);
      end
    else
      begin
        SetString(SBufferA, Buffer, BytesRead);
        StringPos := SFindText(TextInCodepage, SBufferA, bForward, bWholeWords, bCaseSens, BytesRead < cBlockSize);
      end;

    if StringPos > 0 then
      begin
        Result := ReadPos + (StringPos - 1) * FCharSize;
        Exit
      end;

    bContinue := True;
    DoProgress(BufPos, FStreamSize, bContinue);
    if not bContinue then Exit;

    Inc(BufPos, Int64(ReadSize) * BoolToSign(bForward));
    Dec(BufPos, Int64(Length(AText) + 1) * FCharSize * BoolToSign(bForward));
    NormalizePos(BufPos, FCharSize);

    if bForward then
      begin
        if BufPos > BufPosMax then Exit;
      end
    else
      begin
        if BufPos < BufPosMin then Exit;
      end;

  until BytesRead < cBlockSize;
end;

function TATStreamSearch.TextFindFirst(
  const AText: string;
  const AStartPos, AEndPos: Int64;
  AEncoding: TEncConvId;
  AOptions: TATStreamSearchOptions): Boolean;
begin
  FFoundStart := TextFind(AText, AStartPos, AEndPos, AEncoding, AOptions);
  Result := FFoundStart >= 0;

  if Result then
    FFoundLength := FSavedTextLen
  else
    FFoundLength := 0;
end;


//-----------------------------------------------------------------
// Combined search code

function TATStreamSearch.Find(
  const AText: string;
  const AStartPos, AEndPos: Int64;
  AEncoding: TEncConvId;
  ACharSize: integer;
  AOptions: TATStreamSearchOptions): Boolean;
begin
  InitSavedOptions;

  FSavedText := AText;
  if ACharSize=1 then
    FSavedTextLen := Length(SCodepageFromUTF8(AText, AEncoding))
  else
    FSavedTextLen := Length(AText) * ACharSize;

  FSavedEncoding := AEncoding;
  FSavedOptions := AOptions;
  FSavedStartPos := AStartPos;
  FSavedEndPos := AEndPos;
  FCharSize := ACharSize;

  {$IFDEF REGEX}
  if asoRegEx in AOptions then
  begin
    Assert(not (asoBackward in AOptions), 'Backward search not supported for Regex');
    Result := RegexFindFirst(AText, AStartPos, AEncoding, AOptions);
  end
  else
  {$ENDIF}
    Result := TextFindFirst(AText, AStartPos, AEndPos, AEncoding, AOptions);
end;

procedure TATStreamSearch.SaveOptions;
begin
  FSavStart := FFoundStart;
  FSavLen := FFoundLength;
  FSavEnc := FSavedEncoding;
  FSavText := FSavedText;
  FSavOpt := FSavedOptions;
end;

procedure TATStreamSearch.RestoreOptions;
begin
  if FSavText = '' then Exit;
  FFoundStart := FSavStart;
  FFoundLength := FSavLen;
  FSavedEncoding := FSavEnc;
  FSavedText := FSavText;
  FSavedOptions := FSavOpt;
  InitProgressFields(FFoundStart, FSavedEncoding);
end;


{ Registration }

procedure Register;
begin
  RegisterComponents('Samples', [TATStreamSearch]);
end;

end.
