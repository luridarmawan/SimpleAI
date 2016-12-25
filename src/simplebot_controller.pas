unit simplebot_controller;

{$mode objfpc}{$H+}

{

add this to routes
    Route.Add( 'main', TSimpleBotModule); // for main address


}

{$ifdef AI_REDIS}
{$else}
{$endif}

interface

uses
  {$ifdef AI_REDIS}
  simpleairedis_controller,
  {$else}
  simpleai_controller,
  {$endif}
  sysctl,
  fastplaz_handler, html_lib, logutil_lib, http_lib,
  RegExpr, fgl, fpjson, Classes, SysUtils, fpcgi, HTTPDefs;

const
  _AI_CONFIG_NAME = 'ai/default/name';
  _AI_CONFIG_BASEDIR = 'ai/default/basedir';
  _AI_CONFIG_ENTITIES = 'ai/default/entities';
  _AI_CONFIG_INTENTS = 'ai/default/intents';
  _AI_CONFIG_RESPONSE = 'ai/default/response';
  _AI_CONFIG_DEBUG = 'ai/default/debug';
  _AI_CONFIG_DATASOURCE = 'ai/default/datasource';
  _AI_DATASOURCE_REDIS = 'redis';
  _AI_DATASOURCE_FILE = 'file';
  _AI_RESPONSE_INTRODUCTION = 'introduction';
  _AI_RESPONSE_FIRSTSESSION = 'firstsession';
  _AI_RESPONSE_ABOUTME = 'aboutme';
  _AI_RESPONSE_SECONDSESSION = 'secondsession';

  _AI_ACTION_SEPARATOR = '|';

  _AL_LOG_LEARN = 'learn:';
  _AI_SESSION_VISITED = 'AI_VISITED';
  _AI_SESSION_LASTVISIT = 'AI_VISITLAST';
  _AI_SESSION_LASTACTION = 'AI_ACTIONLAST';
  _AI_SESSION_MESSAGECOUNT = 'AI_MESSAGECOUNT';

  _AI_SESSION_ASK_INTENT = 'AI_ASK_INTENT';
  _AI_SESSION_ASK_KEY = 'AI_ASK_KEY';
  _AI_SESSION_ASK_VAR = 'AI_ASK_VAR';
  _AI_ASK_NAME = 'TanyaNama';

  _AI_DEFINE = 'define';
  _AI_SESSION_USER = 'AI_USER_';

  _TELEGRAM_API_URL = 'https://api.telegram.org/bot';
  _TELEGRAM_CONFIG_TOKEN = 'telegram/token';

type

  generic TStringHashMap<T> = class(specialize TFPGMap<String,T>) end;
  THandlerCallback = function(const IntentName: string;
    Params: TStrings): string of object;
  THandlerCallbackMap = specialize TStringHashMap<THandlerCallback>;

  TOnErrorCallback = function(const Text: string): string of object;

  { TSimpleBotModule }

  TSimpleBotModule = class
  private
    FChatID: string;
    MessageID: string;
    FOnError: TOnErrorCallback;
    FDataLoaded: boolean;
    FTelegramToken: string;
    Text: string;
    HTTP: THTTPLib;
    HTTP_Response: IHTTPResponse;
    function getDebug: boolean;
    function getHandler(const TagName: string): THandlerCallback;

    procedure LoadConfig(DataName: string);
    procedure LoadAIDataFromFile;
    procedure setDebug(AValue: boolean);
    procedure setHandler(const TagName: string; AValue: THandlerCallback);
    function handlerProcessing(ActionName, Message: string): string;
    function URL_Handler(const IntentName: string; Params: TStrings): string;
    function OnErrorHandler(const Message: string): string;

    // example handler
    function Example_Handler(const IntentName: string; Params: TStrings): string;

  public
    {$ifdef AI_REDIS}
    SimpleAI: TSimpleAIRedis;
    {$else}
    SimpleAI: TSimpleAI;
    {$endif}
    constructor Create; virtual;
    destructor Destroy; virtual;

    function Exec(Message: string): string;
    function PrepareQuestion: boolean;
    function SetQuestions(IntentName: string; Key: string = '';
      MsgCount: integer = _AI_COUNT__MINIMAL_ASKNAME): string;
    function isAnswer(): boolean;
    function GetResponse(IntentName:string; Action: string = ''; EntitiesKey: string = ''): string;
    function StringReplacement(Message: string): string;

    procedure SetSession(Key, Value: string);
    function GetSession(Key: string): string;

    property ChatID: string read FChatID write FChatID;

    property Debug: boolean read getDebug write setDebug;
    property isDataLoaded: boolean read FDataLoaded;
    property TelegramToken: string read FTelegramToken write FTelegramToken;
    function TelegramSend(ChatIDRef, ReplyToMessageID, Message: string): boolean;

    property Handler[const TagName: string]: THandlerCallback
      read getHandler write setHandler;
    property OnError: TOnErrorCallback read FOnError write FOnError;

  end;

var
  ___HandlerCallbackMap: THandlerCallbackMap;

implementation

uses json_lib, common;

constructor TSimpleBotModule.Create;
begin
  FOnError := nil;
  ___HandlerCallbackMap := THandlerCallbackMap.Create;

  FDataLoaded := False;
  HTTP := THTTPLib.Create;
  {$ifdef AI_REDIS}
  SimpleAI := TSimpleAIRedis.Create;
  {$else}
  SimpleAI := TSimpleAI.Create;
  {$endif}
  LoadConfig('');

  FChatID := '';
  Handler['example'] := @Example_Handler;
  Handler['url'] := @URL_Handler;
end;

destructor TSimpleBotModule.Destroy;
begin
  ___HandlerCallbackMap.Free;
  if Assigned(SimpleAI) then
    SimpleAI.Free;
  HTTP.Free;
end;

procedure TSimpleBotModule.LoadConfig(DataName: string);
var
  i: integer;
  s: string;
  lst: TStrings;
begin
  FTelegramToken := Config[_TELEGRAM_CONFIG_TOKEN];

  try
    SimpleAI.Debug := Config[_AI_CONFIG_DEBUG];
    SimpleAI.AIName := Config[_AI_CONFIG_NAME];
  except
  end;

  // redis
  {$ifdef AI_REDIS}
  SimpleAI.UseRedis := False;
  s := Config[_AI_CONFIG_DATASOURCE];
  if s = _AI_DATASOURCE_REDIS then
  begin
    SimpleAI.UseRedis := True;
    if SimpleAI.LoadDataFromRedis then
      Exit;
  end;
  {$endif}

  //-- sementar buat test
  LoadAIDataFromFile;

end;

procedure TSimpleBotModule.LoadAIDataFromFile;
var
  i: integer;
  s, basedir: string;
  lst: TStrings;
begin
  basedir := Config[_AI_CONFIG_BASEDIR];

  // load Entities
  s := Config[_AI_CONFIG_ENTITIES];
  lst := Explode(s, ',');
  for i := 0 to lst.Count - 1 do
  begin
    SimpleAI.AddEntitiesFromFile(basedir + lst[i]);
  end;
  lst.Free;

  // load Intents
  s := Config[_AI_CONFIG_INTENTS];
  lst := Explode(s, ',');
  for i := 0 to lst.Count - 1 do
  begin
    SimpleAI.AddIntentFromFile(basedir + lst[i]);
  end;
  lst.Free;

  // load Response
  s := Config[_AI_CONFIG_RESPONSE];
  lst := Explode(s, ',');
  for i := 0 to lst.Count - 1 do
  begin
    SimpleAI.AddResponFromFile(basedir + lst[i]);
  end;
  lst.Free;

  FDataLoaded := True;
end;

procedure TSimpleBotModule.setDebug(AValue: boolean);
begin
  SimpleAI.Debug := AValue;
end;

function TSimpleBotModule.getDebug: boolean;
begin
  Result := SimpleAI.Debug;
end;

procedure TSimpleBotModule.setHandler(const TagName: string; AValue: THandlerCallback);
begin
  ___HandlerCallbackMap[TagName] := AValue;
end;

function TSimpleBotModule.getHandler(const TagName: string): THandlerCallback;
begin
  Result := ___HandlerCallbackMap[TagName];
end;

function TSimpleBotModule.handlerProcessing(ActionName, Message: string): string;
var
  i: integer;
  h: THandlerCallback;
begin
  Result := SimpleAI.ResponseText.Text;
  i := ___HandlerCallbackMap.IndexOf(ActionName);
  if i = -1 then
    Exit;
  h := ___HandlerCallbackMap.Data[i];
  Result := h(SimpleAI.IntentName, SimpleAI.Parameters);
end;

function TSimpleBotModule.URL_Handler(const IntentName: string;
  Params: TStrings): string;
var
  i: integer;
  lst: TStrings;
  s, url, method, parameters: string;
  httpClient: THTTPLib;
  httpResponse: IHTTPResponse;
begin
  lst := Explode(SimpleAI.Action, _AI_ACTION_SEPARATOR);
  if lst.Count = 1 then
    Exit;

  method := 'get';
  if lst.Count = 2 then
    url := lst[1]
  else
  begin
    method := lst[1];
    url := lst[2];
  end;

  if pos('?', url) = 0 then
    url := url + '?';

  httpClient := THTTPLib.Create;
  parameters := '';
  for i := 0 to SimpleAI.Parameters.Count - 1 do
  begin
    if method = 'get' then
    begin
      if i <= SimpleAI.Parameters.Count - 1 then
        parameters := parameters + '&';
      parameters := parameters + SimpleAI.Parameters.Names[i] + '=' +
        SimpleAI.Parameters.ValueFromIndex[i];
    end;
    if method = 'post' then
    begin
      s := SimpleAI.Parameters.Names[i];
      httpClient.FormData[s] := SimpleAI.Parameters.ValueFromIndex[i];
    end;
  end;

  httpClient.FormData['wow'] := 'keren';
  httpClient.URL := url + parameters;

  if method = 'get' then
    httpResponse := httpClient.Get();
  if method = 'post' then
    httpResponse := httpClient.Post();

  if httpResponse.ResultCode = 200 then
    Result := httpResponse.ResultText;

  httpClient.Free;
end;

function TSimpleBotModule.OnErrorHandler(const Message: string): string;
begin

end;

procedure TSimpleBotModule.SetSession(Key, Value: string);
var
  sessionKey: string;
begin
  sessionKey := ChatID + '_' + Key;
  _SESSION[sessionKey] := Value;
end;

function TSimpleBotModule.GetSession(Key: string): string;
var
  sessionKey: string;
begin
  sessionKey := ChatID + '_' + Key;
  Result := _SESSION[sessionKey];
end;

function TSimpleBotModule.Example_Handler(const IntentName: string;
  Params: TStrings): string;
begin
  if not SimpleAI.Debug then
    Exit;
  Result := 'This is Example Hook Handler';

end;

function TSimpleBotModule.Exec(Message: string): string;
var
  json: TJSONUtil;
  result_json: TJSONData;
  _text: string;
  messageCount: integer;
  s, text_response, search_title: string;
  lst: TStrings;

  lastvisit_time, lastvisit_length: cardinal;
begin
  if _GET['_DEBUG'] <> '' then
    SimpleAI.Debug := True;

  Text := LowerCase(Message);
  text_response := '';
  SimpleAI.PrefixText := '';
  SimpleAI.SuffixText := '';


  //s := SimpleAI.GetResponse( 'Greeting', '');
  //die( s);

  s := getSession(_AI_SESSION_VISITED);
  if s = '' then
  begin
    s := SimpleAI.GetResponse(_AI_RESPONSE_INTRODUCTION, '', _AI_RESPONSE_FIRSTSESSION);
    s := s + SimpleAI.GetResponse(_AI_RESPONSE_INTRODUCTION, '',
      _AI_RESPONSE_ABOUTME);
    SimpleAI.SuffixText := s;
    //SimpleAI.ResponseText.Add(s);
    setSession(_AI_SESSION_VISITED, '1');
    setSession(_AI_SESSION_LASTVISIT, i2s(_GetTickCount));
    setSession(_AI_SESSION_ASK_INTENT, _AI_ASK_NAME);
  end;

  s := getSession(_AI_SESSION_LASTVISIT);
  lastvisit_time := StrToInt64(s);
  lastvisit_length := (_GetTickCount - lastvisit_time) div 3600000; // jam
  if lastvisit_length > 1 then
  begin
    s := SimpleAI.GetResponse(_AI_RESPONSE_INTRODUCTION, '',
      _AI_RESPONSE_SECONDSESSION);
    s := StringReplacement(s);
    if s <> '' then
      SimpleAI.ResponseText.Add(s);
    setSession(_AI_SESSION_MESSAGECOUNT, '0');
  end;

  // message count
  try
    messageCount := 0;
    messageCount := s2i(getSession(_AI_SESSION_MESSAGECOUNT));
  except
  end;
  messageCount := messageCount + 1;

  if SimpleAI.Exec(Text) then
  begin
    SimpleAI.ResponseText.Text := trim(SimpleAI.ResponseText.Text);
    text_response := SimpleAI.ResponseJson;

    if SimpleAI.Action <> '' then
    begin
      // do action
      setSession(_AI_SESSION_LASTACTION, SimpleAI.Action);
      lst := Explode(SimpleAI.Action, _AI_ACTION_SEPARATOR);
      text_response := handlerProcessing(lst[0], Message);
      lst.Free;

      if text_response <> '' then
        SimpleAI.ResponseText.add(text_response);
    end; //SimpleAI.Action;

    if isAnswer() then
    begin
      // something to do
    end;

  end // if exec
  else
  begin
    if FOnError <> nil then
    begin
      SimpleAI.ResponseText.Text := FOnError(Text);
    end
    else
    begin
      LogUtil.Add(Text, _AL_LOG_LEARN);
    end;
  end;

  if messageCount > _AI_COUNT__MINIMAL_ASKNAME then
  begin
    if PrepareQuestion then
    begin
      messageCount := 0;
      // do something
    end;
  end;

  text_response := SimpleAI.ResponseJson;
  json := TJSONUtil.Create;
  json.LoadFromJsonString(text_response);
  if SimpleAI.Debug then
    text_response := json.AsJSONFormated
  else
    text_response := json.AsJSON;
  json.Free;

  //---
  Result := text_response;
  setSession(_AI_SESSION_MESSAGECOUNT, i2s(messageCount));
  setSession(_AI_SESSION_LASTVISIT, i2s(_GetTickCount));
end;

function TSimpleBotModule.PrepareQuestion: boolean;
var
  s, askIntent: string;
  askKey, askVar, askValue: string;
begin
  Result := False;
  askIntent := getSession(_AI_SESSION_ASK_INTENT);
  if askIntent = '' then
    Exit;

  s := SetQuestions(askIntent);
end;

function TSimpleBotModule.SetQuestions(IntentName: string; Key: string;
  MsgCount: integer): string;
begin
  Result := SimpleAI.SetQuestions(IntentName);
  SimpleAI.ResponseText.Add(Result);
  setSession(_AI_SESSION_ASK_INTENT, IntentName);
  setSession(_AI_SESSION_ASK_KEY, SimpleAI.KeyName);
  setSession(_AI_SESSION_ASK_VAR, SimpleAI.VarName);
end;

// todo: next question dgn parameter count
// count=0 => langsung tanyain

function TSimpleBotModule.isAnswer: boolean;
var
  askIntent: string;
  askKey, askVar, askValue, s: string;
begin
  Result := False;
  askIntent := getSession(_AI_SESSION_ASK_INTENT);
  if askIntent = '' then
    Exit;

  askVar := getSession(_AI_SESSION_ASK_VAR);
  askValue := SimpleAI.Parameters.Values[askVar];
  if askValue = '' then
    Exit;

  setSession(_AI_SESSION_ASK_INTENT, '');
  setSession(_AI_SESSION_ASK_KEY, '');
  setSession(_AI_SESSION_ASK_VAR, '');
  setSession(_AI_SESSION_USER + askVar, askValue);

  if SimpleAI.Action = _AI_DEFINE then
    Exit;
  ;

  // set response
  s := SimpleAI.GetResponse(askIntent + 'Response', '', '');
  if s <> '' then
  begin
    if SimpleAI.SimpleAILib.Intent.Entities.preg_match('%(.*)%', s) then
    begin
      s := StringReplacement(s);
    end;

    SimpleAI.ResponseText.Add(s);
  end;

  Result := True;
end;

function TSimpleBotModule.GetResponse(IntentName: string; Action: string;
  EntitiesKey: string): string;
begin
  Result := SimpleAI.GetResponse(IntentName, Action, EntitiesKey);
end;

function TSimpleBotModule.StringReplacement(Message: string): string;
var
  regex: TRegExpr;
  s: string;
begin
  Result := Text;

  Message := SimpleAI.StringReplacement(Message);

  regex := TRegExpr.Create;
  regex.Expression := '%(.*)%';
  if regex.Exec(Message) then
  begin
    s := getSession(_AI_SESSION_USER + regex.Match[1]);
    Result := SimpleAI.SimpleAILib.Intent.Entities.preg_replace(
      '%(.*)%', s, Message, True);
  end;
  regex.Free;
end;


function TSimpleBotModule.TelegramSend(ChatIDRef, ReplyToMessageID,
  Message: string): boolean;
var
  httpClient: THTTPLib;
  httpResponse: IHTTPResponse;
begin
  Result := False;
  if FTelegramToken = '' then
    Exit;
  //if ((ChatIDRef = '') or (ChatIDRef = '0')) then
  //  Exit;

  Message := StringReplace(Message, '\n', ' ', [rfReplaceAll]);
  Message := StringReplace(Message, '\r', ' ', [rfReplaceAll]);

  httpClient := THTTPLib.Create;
  httpClient.URL := _TELEGRAM_API_URL + FTelegramToken + '/sendMessage' +
    '?chat_id=' + ChatIDRef + '&reply_to_message_id=' + MessageID +
    '&text=' + Message;

  httpResponse := httpClient.Get();

  if httpResponse.ResultCode = 200 then
    Result := True;
  httpClient.Free;
end;



end.
