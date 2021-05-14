{
This file is part of the SimpleAI package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit simpleai_controller;

{$mode objfpc}{$H+}

interface

uses
  common, stemmingnazief_lib, json_lib, http_lib,
  simpleai_lib, dateutils, Dos, RegExpr, fpjson,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets, fpopenssl,
  {$endif}
  IniFiles, Classes, SysUtils;

const
  _AI_NAME = 'CarikBot';

  _AI_COUNT__MINIMAL_ASKNAME = 5;
  _AI_ACTION_SEPARATOR = '|';

  _AI_CMD_OPENFILE = 'file';
  _AI_CMD_URL = 'url';
  _AI_CMD_GET = 'get';
  _AI_CMD_OPENJSONFILE = 'file-json';

  CMD_URL_WITH_CACHE = 'url-cache';
  CMD_GET_WITH_CACHE = 'get-cache';
  CMD_JSON = 'json';
  CMD_JSON_WITH_CACHE = 'json-cache';
  CMD_JSONGET = 'json-get';
  CMD_JSONGET_WITH_CACHE = 'json-get-cache';
  CMD_POST = 'post';
  CMD_POST_WITH_CACHE = 'post-cache';
  CommandList: array  [1..11] of string =
    (_AI_CMD_OPENFILE, _AI_CMD_GET, _AI_CMD_URL, CMD_JSONGET,
    CMD_URL_WITH_CACHE, CMD_GET_WITH_CACHE, CMD_JSON, CMD_JSON_WITH_CACHE,
    CMD_JSONGET_WITH_CACHE, CMD_POST, CMD_POST_WITH_CACHE);

type

  { TSimpleAI }

  TSimpleAI = class
  private
    FActionCallback: string;
    FAdditionalHeaders: TStrings;
    FAdditionalParameters: TStrings;
    FAIName: string;
    FAutoPrune: boolean;
    FCustomReply: TJSONUtil;
    FCustomReplyData: TJSONUtil;
    FCustomReplyMode: string;
    FCustomReplyType: string;
    FImageCaption: string;
    FImageURL: string;
    FIsExternal: Boolean;
    FIsSuccesfull: boolean;
    FIsURLEncoded: boolean;
    FKeyName: string;
    FMsg: string;
    FNonStandardWordFile: String;
    FOnBeforeExecCommand: TNotifyEvent;
    FOriginalMessage: string;
    FPrefixText: string;
    FReplySuffix: string;
    FReplyType: string;
    FRequestText: string;
    FResponseText: TStringList;
    FSimpleAILib: TSimpleAILib;
    FResponseData: TMemIniFile;
    FResponseDataAsList: TStringList;
    FStemmedText: string;
    FStemmedJson: string;
    FStemmedWordCount, FNonStandardWordCount, FUnknownWordCount : Integer;
    FStemmingDictionary: string;
    FSuffixText: string;
    FTrimMessage: boolean;
    FVarName: string;
    FIsStemming: boolean;
    FStandardWordCheck: Boolean;

    function getIsCustomAction: boolean;
    function getIsMarkUp: boolean;
    function getIsStemming: boolean;
    function getResponseJson: string;
    function getTimeSession(): string;

    function getAction: string;
    function getDebug: boolean;
    function getIntentName: string;
    function getParameters: TStrings;
    function getParameterValue(KeyName: string): string;
    function getPatternString: string;

    function generateGetQuery: string;
    function execPost(AURL: string; ACache: boolean = False): string;
    function execJson(AURL: string; ACache: boolean = False): string;
    function execJsonGet(AURL: string; ACache: boolean = False): string;
    function execGet(AURL: string; ACache: boolean = False): string;

    procedure setDebug(AValue: boolean);
    function isValidCommand(ACommandString: string): boolean;
    function isCommand(Msg: string): boolean;
    function execCommand(Message: string): string;
    function openFile(FileName: string): string;
    procedure setIsStemming(AValue: boolean);
    procedure SetStemmingDictionary(AValue: string);
    procedure parseReply(AText:string);
  public
    StartTime, StopTime, ElapsedTime: cardinal;
    constructor Create; virtual;
    destructor Destroy; virtual;

    procedure Clear;
    function AddIntentFromFile(FileName: string): boolean;
    function AddEntitiesFromFile(FileName: string): boolean;
    function AddResponFromFile(FileName: string): boolean;

    function Exec(Text: string; AutoResponse: boolean = True): boolean;
    function GetQuestions(IntentName: string; Key: string = '';
      MsgCount: integer = _AI_COUNT__MINIMAL_ASKNAME): string;
    function GetResponse(IntentName: string; Action: string = '';
      EntitiesKey: string = ''): string;
    function SetResponseData(List: TStrings): boolean;
    function StringReplacement(Text: string; BURLEncode: boolean = False): string;

    property IsSuccessfull: boolean read FIsSuccesfull;
    property AIName: string read FAIName write FAIName;
    property SimpleAILib: TSimpleAILib read FSimpleAILib;
    property Action: string read getAction;
    property IntentName: string read getIntentName;
    property IsMarkUp: boolean read GetIsMarkUp;
    property KeyName: string read FKeyName;
    property VarName: string read FVarName;
    property Parameters: TStrings read getParameters;
    property AdditionalParameters: TStrings
      read FAdditionalParameters write FAdditionalParameters;
    property AdditionalHeaders: TStrings
      read FAdditionalHeaders write FAdditionalHeaders;
    property Values[KeyValue: string]: string read getParameterValue; default;
    property ActionCallback: string read FActionCallback;
    property IsURLEncoded: Boolean read FIsURLEncoded;
    property IsExternal: Boolean read FIsExternal;
    property ResponseText: TStringList read FResponseText write FResponseText;
    property ResponseJson: string read getResponseJson;
    property ResponseData: TMemIniFile read FResponseData;
    property PrefixText: string read FPrefixText write FPrefixText;
    property SuffixText: string read FSuffixText write FSuffixText;
    property Debug: boolean read getDebug write setDebug;
    property Pattern: string read getPatternString;
    property Msg: string read FMsg;
    property TrimMessage: boolean read FTrimMessage write FTrimMessage;
    property ImageURL: string read FImageURL;
    property ImageCaption: string read FImageCaption;
    property AutoPrune: boolean read FAutoPrune;
    property OriginalMessage: string read FOriginalMessage write FOriginalMessage;
    property ReplyType: string read FReplyType;
    property ReplySuffix: string read FReplySuffix;

    // Stemming
    property Stemming: boolean read getIsStemming write setIsStemming;
    property StemmingDictionary: string read FStemmingDictionary
      write SetStemmingDictionary;
    property StemmedText: string read FStemmedText;

    property StandardWordCheck: Boolean read FStandardWordCheck write FStandardWordCheck;
    property NonStandardWordFile: String read FNonStandardWordFile write FNonStandardWordFile;

    // CustomAction
    property IsCustomAction: boolean read getIsCustomAction;
    property CustomReply: TJSONUtil read FCustomReply;
    property CustomReplyType: string read FCustomReplyType;
    property CustomReplyMode: string read FCustomReplyMode;
    property CustomReplyData: TJSONUtil read FCustomReplyData;

  published
    property OnBeforeExecCommand: TNotifyEvent read FOnBeforeExecCommand write FOnBeforeExecCommand;
  end;

implementation

const
  _BASEDIR = 'files/';

// command

var
  NamaHari: TWeekNameArray = ('Minggu', 'Senin', 'Selasa', 'Rabu',
    'Kamis', 'Jumat', 'Sabtu');
  NamaBulan: TMonthNameArray = ('Januari', 'Februari', 'Maret', 'April',
    'Mei', 'Juni', 'Juli', 'Augustus', 'September', 'Oktober', 'November', 'Desember');

{ TSimpleAI }

function TSimpleAI.getIntentName: string;
begin
  Result := FSimpleAILib.IntentName;
end;

function TSimpleAI.getParameters: TStrings;
begin
  Result := FSimpleAILib.Parameters;
end;

function TSimpleAI.getParameterValue(KeyName: string): string;
begin
  Result := FSimpleAILib.Parameters.Values[KeyName];
end;

function TSimpleAI.getPatternString: string;
begin
  Result := FSimpleAILib.Pattern;
end;

function TSimpleAI.generateGetQuery: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to FSimpleAILib.Parameters.Count - 1 do
  begin
    Result := Result + '&' + FSimpleAILib.Parameters.Names[i] + '=' +
      UrlEncode(FSimpleAILib.Parameters.ValueFromIndex[i]);
  end;
end;

function TSimpleAI.StringReplacement(Text: string; BURLEncode: boolean): string;
var
  i: integer;
  s, t, range: string;
  y, m, d: word;
  regex: TRegExpr;
  dateTimePosition: TDateTime;
begin
  Result := Text;
  Result := FSimpleAILib.Intent.Entities.preg_replace(
    '%(time_session)%', getTimeSession, Result, False);
  Result := FSimpleAILib.Intent.Entities.preg_replace(
    '%(AIName)%', AIName, Result, False);
  Result := FSimpleAILib.Intent.Entities.preg_replace(
    '%(BotName)%', AIName, Result, False);

  dateTimePosition := now;

  //Setup Additional Parameter
  for i := 0 to FAdditionalParameters.Count - 1 do
  begin
    FSimpleAILib.Parameters.Values[FAdditionalParameters.Names[i]] :=
      FAdditionalParameters.ValueFromIndex[i];
  end;



  //if in range
  range := FSimpleAILib.Parameters.Values['range_value'];
  {
  if range = 'sekarang' then
    range := '';
  }
  if (range <> '') then
  begin
    case range of
      'kemaren lusa',
      'kemarin lusa':
      begin
        dateTimePosition := IncDay(Now, -2);
      end;
      'kemaren',
      'kemarin':
      begin
        dateTimePosition := IncDay(Now, -1);
      end;
      'bsk',
      'besok':
      begin
        dateTimePosition := IncDay(Now, 1);
      end;
      'lusa':
      begin
        dateTimePosition := IncDay(Now, 2);
      end;
      'besok lusa':
      begin
        dateTimePosition := IncDay(Now, 3);
      end;
    end;
  end;
  // waktu
  if FSimpleAILib.Intent.Entities.preg_match('%(time)%', Result) then
  begin
    DecodeDate(dateTimePosition, y, m, d);
    s := Parameters.Values['Waktu'];
    if s = '' then
      s := 'jam';
    t := '';
    case s of
      'jam': t := FormatDateTime('hh:nn', dateTimePosition) + ' ' + getTimeSession;
      'hari': t := NamaHari[DayOfWeek(dateTimePosition)];
      'bulan': t := NamaBulan[m];
      'tahun': t := IntToStr(y);
      'tanggal': t := FormatDateTime('dd/mm/yyyy', dateTimePosition)
    end;
    t := range + ' ' + t;
    Result := FSimpleAILib.Intent.Entities.preg_replace(
      '%(time)%', t, Result, False);
  end;

  regex := TRegExpr.Create;
  //regex.Expression := '%(.*)%';
  regex.Expression := '%([a-zA-Z0-9_]+)%';
  if regex.Exec(Result) then
  begin
    s := regex.Match[1];
    if FSimpleAILib.Parameters.Values[s] <> '' then
    begin
      if BURLEncode then
        Result := SimpleAILib.Intent.Entities.preg_replace(
          '%' + s + '%', UrlEncode(FSimpleAILib.Parameters.Values[s]), Result, True)
      else
        Result := SimpleAILib.Intent.Entities.preg_replace(
          '%' + s + '%', FSimpleAILib.Parameters.Values[s], Result, True);
    end;

    while regex.ExecNext do
    begin
      s := regex.Match[1];
      if FSimpleAILib.Parameters.Values[s] <> '' then
      begin
        if BURLEncode then
          Result := SimpleAILib.Intent.Entities.preg_replace(
            '%' + s + '%', UrlEncode(FSimpleAILib.Parameters.Values[s]), Result, True)
        else
          Result := SimpleAILib.Intent.Entities.preg_replace(
            '%' + s + '%', FSimpleAILib.Parameters.Values[s], Result, True);
      end
      else
      begin
        Result := SimpleAILib.Intent.Entities.preg_replace(
          '%' + s + '%', '', Result, True);
      end;
    end;

  end;
  regex.Free;

end;

procedure TSimpleAI.setDebug(AValue: boolean);
begin
  FSimpleAILib.Intent.Debug := AValue;
end;

function TSimpleAI.isValidCommand(ACommandString: string): boolean;
var
  s: string;
begin
  Result := False;
  for s in CommandList do
  begin
    if ACommandString = s then
      Exit(True);
  end;
end;

function TSimpleAI.isCommand(Msg: string): boolean;
var
  lst: TStrings;
begin
  Result := False;
  if Msg = '' then
    Exit;
  lst := Explode(Msg, ':');
  if lst.Count = 1 then
  begin
    lst.Free;
    Exit;
  end;

  if isValidCommand(lst[0]) then
    Result := True;

  lst.Free;
end;

function TSimpleAI.execCommand(Message: string): string;
var
  convertedMessage, s, url: string;
  lst: TStrings;

  function stripText(AText: string): string;
  begin
    Result := StringReplace(AText, '<b>', '*', [rfReplaceAll]);
    Result := StringReplace(Result, '</b>', '*', [rfReplaceAll]);
    Result := StripHTML(Result);
    Result := StringReplace(Result, #9, '', [rfReplaceAll]);
    Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
    Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  end;

begin
  FReplyType := 'text';
  FReplySuffix := '';
  ResponseText.Text := '';
  Result := Message;
  convertedMessage := StringReplacement(Message);
  lst := Explode(convertedMessage, ':');
  case lst[0] of
    _AI_CMD_OPENFILE:
    begin
      s := trim(_BASEDIR + lst[1]);
      Result := openFile(s);
      if Result = '' then
        Result := convertedMessage;
      Result := Trim(Result);
      Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
      Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
      Result := StringReplacement(Result);
    end;
    _AI_CMD_OPENJSONFILE:
    begin
      s := trim(_BASEDIR + lst[1]);
      Result := openFile(s);
      if Result = '' then
        Result := convertedMessage;
      Result := StringReplacement(Result);
    end;

    CMD_POST:
    begin
      convertedMessage := StringReplacement(Message, True);
      url := Trim(Copy(convertedMessage, Pos(':', convertedMessage) + 1));
      Result := execPost(url);
      Result := stripText(Result);
    end;
    CMD_POST_WITH_CACHE:
    begin
      convertedMessage := StringReplacement(Message, True);
      url := Trim(Copy(convertedMessage, Pos(':', convertedMessage) + 1));
      Result := execPost(url, True);
      Result := stripText(Result);
    end;

    CMD_JSON:
    begin
      convertedMessage := StringReplacement(Message, True);
      url := Trim(Copy(convertedMessage, Pos(':', convertedMessage) + 1));
      Result := execJson(url);
      Result := stripText(Result);
    end;
    CMD_JSON_WITH_CACHE:
    begin
      convertedMessage := StringReplacement(Message, True);
      url := Trim(Copy(convertedMessage, Pos(':', convertedMessage) + 1));
      Result := execJson(url, True);
      Result := stripText(Result);
    end;

    _AI_CMD_GET,
    _AI_CMD_URL:
    begin
      convertedMessage := StringReplacement(Message, True);
      Result := Trim(Copy(convertedMessage, Pos(':', convertedMessage) + 1));
      Result := execGet(Result);
      Result := stripText(Result);
      if Result <> '' then
        Result := Result + GetResponse(IntentName + 'Footer');
    end;
    CMD_GET_WITH_CACHE,
    CMD_URL_WITH_CACHE:
    begin
      convertedMessage := StringReplacement(Message, True);
      Result := Trim(Copy(convertedMessage, Pos(':', convertedMessage) + 1));
      Result := execGet(Result, True);
      Result := stripText(Result);
      if Result <> '' then
        Result := Result + GetResponse(IntentName + 'Footer');
    end;
    CMD_JSONGET:
    begin
      convertedMessage := StringReplacement(Message, True);
      url := Trim(Copy(convertedMessage, Pos(':', convertedMessage) + 1));
      Result := execJsonGet(url);
      Result := stripText(Result);
    end;
    CMD_JSONGET_WITH_CACHE:
    begin
      convertedMessage := StringReplacement(Message, True);
      url := Trim(Copy(convertedMessage, Pos(':', convertedMessage) + 1));
      Result := execJsonGet(url, True);
      Result := stripText(Result);
    end;
  end;

  lst.Free;
end;

function TSimpleAI.openFile(FileName: string): string;
var
  _note: TStringList;
begin
  Result := '';
  if FileExists(trim(FileName)) then
  begin
    _note := TStringList.Create;
    _note.LoadFromFile(trim(FileName));
    Result := _note.Text;
    _note.Free;
  end;
end;

function TSimpleAI.execPost(AURL: string; ACache: boolean): string;
var
  i: integer;
  s: string;
  lst: TStrings;
  Response: IHTTPResponse;
begin
  Result := '';
  FIsSuccesfull := False;

  if ACache then
  begin
    Result := LoadCache(AURL);
    Result := Trim(Result);
    if Result <> '' then
      Exit;
  end;

  with THTTPLib.Create(AURL) do
  begin
    AllowRedirect := True;
    try
      AddHeader('_source', 'carik');
      AddHeader('User-Agent', 'carik/nlp');
      //get header from response list
      s := GetResponse(IntentName, '', 'header');
      s := StringReplace(s, ':', '=', [rfReplaceAll]);
      lst := Explode(s, '|');
      for i := 0 to lst.Count - 1 do
      begin
        if lst.Names[i] <> '' then
          AddHeader(lst.Names[i], lst.ValueFromIndex[i]);
      end;
      lst.Free;

      for i := 0 to FSimpleAILib.Parameters.Count - 1 do
      begin
        FormData[FSimpleAILib.Parameters.Names[i]] :=
          UrlEncode(FSimpleAILib.Parameters.ValueFromIndex[i]);
      end;
      for i := 0 to FAdditionalHeaders.Count - 1 do
      begin
        AddHeader(FAdditionalHeaders.Names[i], FAdditionalHeaders.ValueFromIndex[i]);
      end;
      Response := Post();
      Result := Response.ResultText;
      if Response.ResultCode = 200 then
      begin
        FIsSuccesfull := True;
        if ACache and (Result <> '') then
        begin
          SaveCache(AURL, Result);
        end;
      end
      else
      begin
        Result := 'FAILED: ' + Result;
        if not Debug then
          Result := '';
      end;
    except
      on e: Exception do
      begin
        if Debug then
        begin
          Result := e.Message;
        end;
      end;
    end;

    Free;
  end;
end;

function TSimpleAI.execJson(AURL: string; ACache: boolean): string;
var
  pathName: string;
  lst: TStrings;
  json: TJSONUtil;
begin
  Result := '';
  pathName := 'text';
  lst := Explode(AURL, '|');
  if lst.Count > 1 then
  begin
    pathName := lst[0];
    AURL := lst[1];
    Result := pathName;
  end;
  lst.Free;

  if ACache then
  begin
    Result := LoadCache(AURL);
    Result := Trim(Result);
    if Result <> '' then
      Exit;
  end;

  Result := execPost( AURL);
  if not FIsSuccesfull then
  begin
    if Debug then
    begin
      //TODO: log debug;
    end;
    Result := '';
  end;

  if Result = '' then
    Exit;

  parseReply(Result);
  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(Result, False);

    //Result := json[pathName];
    Result := json.Data.FindPath(pathName).AsString;

    FImageURL := json['image'];
    FImageCaption := json['image_caption'];
    FAutoPrune := s2b(json['prune']);
    if ACache and (Result <> '') then
    begin
      SaveCache(AURL, Result);
    end;
  except
    on e: Exception do
    begin
      Result := '';
    end;
  end;
  json.Free;

end;

function TSimpleAI.execJsonGet(AURL: string; ACache: boolean): string;
var
  pathName: string;
  lst: TStrings;
  json: TJSONUtil;
begin
  Result := '';
  pathName := 'text';
  lst := Explode(AURL, '|');
  if lst.Count > 1 then
  begin
    pathName := lst[0];
    AURL := lst[1];
    Result := pathName;
  end;
  lst.Free;

  if ACache then
  begin
    Result := LoadCache(AURL);
    Result := Trim(Result);
    if Result <> '' then
      Exit;
  end;

  Result := file_get_contents(AURL);
  if Result = '' then
    Exit;

  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(Result, False);
    Result := json[pathName];
    FImageURL := json['image'];
    FImageCaption := json['image_caption'];
    if ACache and (Result <> '') then
    begin
      SaveCache(AURL, Result);
    end;
  except
    Result := '';
  end;
  json.Free;
end;

function TSimpleAI.execGet(AURL: string; ACache: boolean): string;
var
  i: integer;
  s, tempURL: string;
  lst: TStrings;
  Response: IHTTPResponse;
begin
  Result := '';
  FIsSuccesfull := False;
  if ACache then
  begin
    Result := LoadCache(AURL);
    Result := Trim(Result);
    if Result <> '' then
      Exit;
  end;

  tempURL := AURL;
  if GetResponse(IntentName, '', 'full-query') = 'yes' then
    tempURL := AURL + generateGetQuery;

  with THTTPLib.Create(tempURL) do
  begin
    AllowRedirect := True;
    try
      AddHeader('_source', 'carik');
      //get header from response list
      s := GetResponse(IntentName, '', 'header');
      s := StringReplace(s, ':', '=', [rfReplaceAll]);
      lst := Explode(s, '|');
      for i := 0 to lst.Count - 1 do
      begin
        if lst.Names[i] <> '' then
          AddHeader(lst.Names[i], lst.ValueFromIndex[i]);
      end;
      lst.Free;

      for i := 0 to FSimpleAILib.Parameters.Count - 1 do
      begin
        FormData[FSimpleAILib.Parameters.Names[i]] :=
          UrlEncode(FSimpleAILib.Parameters.ValueFromIndex[i]);
      end;
      Response := Get();
      Result := Response.ResultText;
      if Response.ResultCode <> 200 then
      begin
        FIsSuccesfull := True;
        Result := 'FAILED: ' + Result;
        if not Debug then
          Result := '';
      end;
    except
      on e: Exception do
      begin
        if Debug then
        begin
          Result := e.Message;
        end;
      end;
    end;
    Free;
  end;

  if ACache and (Result <> '') then
  begin
    SaveCache(AURL, Result);
  end;
end;

function TSimpleAI.getTimeSession(): string;
var
  Hour, Min, Sec, HSec: word;
begin
  GetTime(Hour, Min, Sec, HSec);
  Result := 'pagi';
  if Hour > 11 then
    Result := 'siang';
  if Hour >= 15 then
    Result := 'sore';
  if Hour > 17 then
    Result := 'sore';
  if Hour >= 18 then
    Result := 'malam';
end;

function TSimpleAI.getAction: string;
begin
  Result := FSimpleAILib.Action;
end;

function TSimpleAI.getDebug: boolean;
begin
  Result := FSimpleAILib.Intent.Debug;
end;

constructor TSimpleAI.Create;
begin
  FAIName := _AI_NAME;
  FResponseData := TMemIniFile.Create('');
  FResponseDataAsList := TStringList.Create;
  FSimpleAILib := TSimpleAILib.Create;
  FResponseText := TStringList.Create;
  FAdditionalParameters := TStringList.Create;
  FAdditionalHeaders := TStringList.Create;
  FMsg := '';
  FOriginalMessage := '';
  FActionCallback := '';
  FImageURL := '';
  FImageCaption := '';
  FAutoPrune := false;
  FIsURLEncoded := false;
  FTrimMessage := False;
  FIsExternal := False;
  FIsSuccesfull := False;

  // Stemming
  FIsStemming := False;
  FStemmingDictionary := 'files' + DirectorySeparator + STEMMINGNAZIEF_DICTIONARY_FILE;
  FStemmedText := '';
  FStandardWordCheck := False;
  FNonStandardWordFile := 'files' + DirectorySeparator + WORD_NONSTANDARD_FILE;

  FReplyType := '';
  FReplySuffix := '';
  FCustomReplyMode := '';
  FCustomReply := TJSONUtil.Create;
  FCustomReplyData := TJSONUtil.Create;
end;

destructor TSimpleAI.Destroy;
begin
  FCustomReplyData.Free;
  FCustomReply.Free;
  FAdditionalHeaders.Free;
  FAdditionalParameters.Free;
  FResponseText.Free;
  FResponseDataAsList.Free;
  FResponseData.Free;
  FSimpleAILib.Free;
end;

procedure TSimpleAI.Clear;
begin
  FSimpleAILib.Clear;
end;

function TSimpleAI.AddIntentFromFile(FileName: string): boolean;
begin
  Result := FSimpleAILib.AddDataIntentFromFile(FileName);
end;

function TSimpleAI.AddEntitiesFromFile(FileName: string): boolean;
begin
  Result := FSimpleAILib.AddDataEntitiesFromFile(FileName);
end;

function TSimpleAI.AddResponFromFile(FileName: string): boolean;
var
  lst: TStrings;
begin
  Result := False;
  if not FileExists(FileName) then
    exit;

  if Assigned(FResponseData) then
    FResponseData.Free;
  FResponseData := TMemIniFile.Create('');
  FResponseData.Clear;
  lst := TStringList.Create;
  with TStringList.Create do
  begin
    LoadFromFile(FileName);
    FResponseDataAsList.Add(Text);

    lst.Text := FResponseDataAsList.Text;
    FResponseData.SetStrings(lst);

    Result := True;
    Free;
  end;
  lst.Free;

end;

function TSimpleAI.Exec(Text: string; AutoResponse: boolean): boolean;
var
  stemmer: TStemmingNazief;
begin
  FMsg := '';
  Result := False;
  FIsExternal := False;
  if Text = '' then
    Exit;

  StartTime := _GetTickCount;
  if FTrimMessage then
  begin
    Text := ReplaceAll(Text, ['''', '"'], '');
  end;

  if FIsStemming then
  begin
    stemmer := TStemmingNazief.Create;
    stemmer.LoadDictionaryFromFile(FStemmingDictionary);
    stemmer.StandardWordCheck := FStandardWordCheck;
    if FStandardWordCheck then
      stemmer.LoadNonStandardWordFromFile( FNonStandardWordFile);
    FStemmedJson := stemmer.ParseSentence(Text);
    FStemmedWordCount := stemmer.WordCount;
    FNonStandardWordCount := stemmer.NonStandardWordCount;
    FUnknownWordCount := stemmer.UnknownWordCount;
    FStemmedText := stemmer.Text;
    FIsStemming := stemmer.IsDictionaryLoaded;
    Stemmer.Free;
  end;

  FResponseText.Clear;
  Result := FSimpleAILib.Exec(Text);
  if not AutoResponse then
    Exit;

  FRequestText := Text;

  if Result then
  begin
    FPrefixText := FSimpleAILib.Prefix;
    FSuffixText := FSimpleAILib.Suffix;
    FResponseText.Add(GetResponse(IntentName, Action, ''));
    FActionCallback := GetResponse(IntentName, '', 'action');
    FIsURLEncoded := s2b( GetResponse(IntentName, '', 'urlencoded'));
    if FActionCallback = '._' then
      FActionCallback := '';
  end
  else
  begin
    FResponseText.Add(GetResponse('none', '', ''));
    FActionCallback := '';
  end;

  FResponseText.Text := FPrefixText + trim(FResponseText.Text) + FSuffixText;
  if FSimpleAILib.Intent.Entities.preg_match('%(.*)%', FResponseText.Text) then
  begin
    FResponseText.Text := StringReplacement(FResponseText.Text, FIsURLEncoded);
  end;

  // is Command
  if isCommand(FResponseText.Text) then
  begin
    //FResponseText.Text := execCommand(FResponseText.Text);
  end;
  StopTime := _GetTickCount;
  ElapsedTime := StopTime - StartTime;
end;

function TSimpleAI.GetQuestions(IntentName: string; Key: string;
  MsgCount: integer): string;
begin
  Result := GetResponse(IntentName, '', Key);
end;

function TSimpleAI.GetResponse(IntentName: string; Action: string;
  EntitiesKey: string): string;
var
  i: integer;
  link: string;
  item_list: TStringList;
begin
  Result := '';

  link := FResponseData.ReadString(IntentName, 'link', '');
  if link <> '' then
    IntentName := link;

  if EntitiesKey <> '' then
  begin
    Result := FResponseData.ReadString(IntentName, EntitiesKey, '._');
    Exit;
  end;

  item_list := TStringList.Create;
  FResponseData.ReadSectionRaw(IntentName, item_list);

  // clean up
  for i := item_list.Count - 1 downto 0 do
  begin
    if pos('say=', item_list[i]) <> 1 then
      item_list.Delete(i);
  end;

  if item_list.Count > 0 then
  begin
    Randomize;

    RandSeed := GetTickCount64;
    i := Random(item_list.Count);

    Result := item_list[i];
    FKeyName := item_list.Names[i];
    i := pos(':', Result);
    if i > 0 then
    begin
      FVarName := copy(FKeyName, i + 1);
      FKeyName := copy(FKeyName, 0, i - 1);
    end;

    if Debug then
    begin
      //FMsg := '';
    end;
    Result := copy(Result, pos('=', Result) + 1);
  end;

  item_list.Free;
  if isCommand(Result) then
  begin
    if Assigned(FOnBeforeExecCommand) then
      FOnBeforeExecCommand(self);
    FIsExternal := True;
    Result := execCommand(Result);
  end;
end;

function TSimpleAI.SetResponseData(List: TStrings): boolean;
begin
  Result := False;
  if not Assigned(List) then
    Exit;

  if Assigned(FResponseData) then
    FResponseData.Free;
  FResponseData := TMemIniFile.Create('');
  FResponseData.Clear;

  FResponseDataAsList.Add(List.Text);
  FResponseData.SetStrings(List);

  Result := True;
end;


function TSimpleAI.getResponseJson: string;
var
  i: integer;
  s, json, actionName, txt, v: string;
  lst: TStrings;
  o: TJSONUtil;
  customReplyDataAsArray: TJSONArray;
  cmdAction, parameterAction, fieldAction : TStrings;
begin
  Result := '';
  actionName := Action;
  lst := FSimpleAILib.Intent.Explode(Action, _AI_ACTION_SEPARATOR);
  if lst.Count > 0 then
    actionName := lst[0];

  // response text
  txt := '';
  for i := 0 to FResponseText.Count - 1 do
  begin
    txt := txt + '"' + StringToJSONString(FResponseText[i], False) + '"';
    if i < FResponseText.Count - 1 then
      txt := txt + ',';
  end;

  json := json + '';
  json := json + '{';
  json := json + '"code" : 0,';
  json := json + '"request" : {';
  json := json + '"text" : "' + StringToJSONString(FRequestText, False) + '"';
  if FOriginalMessage <> '' then
    json := json + ',"original_text" : "' + StringToJSONString(FOriginalMessage, False) + '"';
  json := json + '},';
  if FIsStemming then
  begin
    json := json + '"stemming" : {';
    json := json + '"text" : "' + FStemmedText + '",';
    json := json + '"wordcount" : "' + i2s(FStemmedWordCount) + '",';
    json := json + '"nonstandardword_count" : "' + i2s(FNonStandardWordCount) + '",';
    json := json + '"unknownword_count" : "' + i2s(FUnknownWordCount) + '",';
    json := json + '"response" : ' + FStemmedJson;
    json := json + '},';
  end;
  json := json + '"response" : {';
  json := json + '"intents" : {';
  json := json + '"action" : "' + actionName + '",';
  json := json + '"name" : "' + IntentName + '",';
  if not SimpleAILib.Intent.Context.IsEmpty then
    json := json + '"context" : "' + SimpleAILib.Intent.Context + '",';;
  if Debug then
  begin
    json := json + '"key" : "' + FSimpleAILib.Intent.IntentKey + '",';
    json := json + '"pattern" : "' + StringToJSONString(FSimpleAILib.Pattern, False) + '",';
    //json := json + '"time_usage" : "' + IntToStr(ElapsedTime) + '",';
  end;
  json := json + '"parameters" : {';

  for i := 0 to FSimpleAILib.Parameters.Count - 1 do
  begin
    v := FSimpleAILib.Parameters.ValueFromIndex[i];
    v := StringToJSONString(v, False);
    json := json + '"' + FSimpleAILib.Parameters.Names[i] + '" : "' + v + '"';

    if i < FSimpleAILib.Parameters.Count - 1 then
      json := json + ',';
  end;
  json := json + '}';
  json := json + '},';
  json := json + '"text" : [' + txt + ']';
  json := json + ',"elapsed_time" : ' + ElapsedTime.ToString;
  if FIsExternal then
    json := json + ',"external" : true';
  if FMsg <> '' then
    json := json + ',"msg" : "' + FMsg + '"';
  json := json + '}';
  json := json + '}';

  lst.Free;

  o := TJSONUtil.Create;
  o.LoadFromJsonString( json, False);

  //image view
  if not FImageURL.IsEmpty then
  begin
    FActionCallback := 'image.view|url='+FImageURL;
  end;

  if FActionCallback <> '' then
  begin
    FActionCallback := StringReplacement(FActionCallback);
    parameterAction := Explode(FActionCallback, '|');
    cmdAction := Explode(parameterAction[0], '.');

    o['response/action/callback_string'] := FActionCallback;
    o['response/action/callback_name'] := cmdAction[0];
    if cmdAction.Count > 1 then
      o['response/action/callback_method'] := cmdAction[1];
    if not FImageCaption.IsEmpty then
      o['response/action/caption'] := FImageCaption;
    for i := 1 to parameterAction.count - 1 do
    begin
      fieldAction := Explode(parameterAction[i], '=');
      s := '';
      try
        s := fieldAction[1];
      except
      end;
      o['response/action/parameter_' + i2s(i)] := parameterAction[i];
      o['response/action/' + fieldAction[0]] := s;
      fieldAction.Free;
    end;

    cmdAction.Free;
    parameterAction.Free;
  end;// FActionCallback

  if IsCustomAction then
  begin
    o['action/action/type'] := FCustomReplyType;
    o['action/action/mode'] := FCustomReplyMode;
    try
      customReplyDataAsArray := TJSONArray(GetJSON(CustomReplyData.Data.AsJSON, False));
      o.ValueArray['action/action/data'] := customReplyDataAsArray;
    except
    end;
  end;

  json := o.AsJSON;
  o.Free;
  Result := json;
end;

procedure TSimpleAI.setIsStemming(AValue: boolean);
begin
  FIsStemming := AValue;
end;

procedure TSimpleAI.SetStemmingDictionary(AValue: string);
begin
  if FStemmingDictionary = AValue then
    Exit;
  FStemmingDictionary := AValue;
end;

procedure TSimpleAI.parseReply(AText: string);
begin
  FCustomReplyType := '';
  FCustomReplyMode := '';
  FCustomReply.LoadFromJsonString(AText, False);
  FReplyType := FCustomReply['type'];
  FReplySuffix := FCustomReply['suffix'];
  if (FReplyType = '') or (FReplyType = 'text') then
  begin
    FReplyType := 'text';
    Exit;
  end;

  FCustomReplyType := FCustomReply['action/type'];
  FCustomReplyMode := FCustomReply['action/mode'];
  FCustomReplyData.LoadFromJsonString(FCustomReply.ValueArray['action/data'].AsJSON, False);
end;

function TSimpleAI.getIsStemming: boolean;
begin
  Result := FIsStemming;
end;

function TSimpleAI.getIsMarkUp: boolean;
var
  s: string;
begin
  Result := True;
  try
    s := FSimpleAILib.Intent.Data.ReadString(FSimpleAILib.IntentName, 'markdown', 'true');
  except
  end;
  if s = 'false' then
    Result := False;
end;

function TSimpleAI.getIsCustomAction: boolean;
begin
  Result := False;
  if FReplyType = 'action' then
    Result := True;
end;


end.
