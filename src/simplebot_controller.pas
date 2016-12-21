unit simplebot_controller;

{$mode objfpc}{$H+}

{

add this to routes
    Route.Add( 'main', TSimpleBotModule); // for main address


}

interface

uses
  simpleai_controller,
  fastplaz_handler, html_lib, logutil_lib, http_lib,
  fgl, fpjson, Classes, SysUtils, fpcgi, HTTPDefs;

const
  _AI_CONFIG_NAME = 'ai/default/name';
  _AI_CONFIG_BASEDIR = 'ai/default/basedir';
  _AI_CONFIG_ENTITIES = 'ai/default/entities';
  _AI_CONFIG_INTENTS = 'ai/default/intents';
  _AI_CONFIG_RESPONSE = 'ai/default/response';
  _AI_CONFIG_DEBUG = 'ai/default/debug';
  _AI_RESPONSE_INTRODUCTION = 'introduction';
  _AI_RESPONSE_FIRSTSESSION = 'firstsession';
  _AI_RESPONSE_ABOUTME = 'aboutme';
  _AI_RESPONSE_SECONDSESSION = 'secondsession';

  _AI_SESSION_VISITED = 'AI_VISITED';
  _AI_SESSION_LASTVISIT = 'AI_LASTVISIT';

  _TELEGRAM_API_URL = 'https://api.telegram.org/bot';
  _TELEGRAM_CONFIG_TOKEN = 'telegram/token';

type

  generic TStringHashMap<T> = class(specialize TFPGMap<String,T>) end;
  THandlerCallback = function(const ActionName: string;
    Params: TStrings): string of object;
  THandlerCallbackMap = specialize TStringHashMap<THandlerCallback>;


  { TSimpleBotModule }

  TSimpleBotModule = class
  private
    ChatID, MessageID: string;
    FTelegramToken: string;
    Text: string;
    HTTP: THTTPLib;
    HTTP_Response: IHTTPResponse;
    function getDebug: boolean;
    function getHandler(const TagName: string): THandlerCallback;

    procedure LoadConfig(DataName: string);
    procedure setDebug(AValue: boolean);
    procedure setHandler(const TagName: string; AValue: THandlerCallback);
    function execHandler(ActionName, Message: string): string;
    function URL_Handler(const ActionName: string; Params: TStrings): string;

    // example handler
    function Example_Handler(const ActionName: string; Params: TStrings): string;

  public
    SimpleAI: TSimpleAI;
    constructor Create; virtual;
    destructor Destroy; virtual;

    function Exec(Message: string): string;

    property Debug: boolean read getDebug write setDebug;
    property TelegramToken: string read FTelegramToken write FTelegramToken;
    function TelegramSend(ChatIDRef, ReplyToMessageID, Message: string): boolean;

    property Handler[const TagName: string]: THandlerCallback
      read getHandler write setHandler;

  end;

var
  ___HandlerCallbackMap: THandlerCallbackMap;

implementation

uses json_lib, common;

constructor TSimpleBotModule.Create;
begin
  ___HandlerCallbackMap := THandlerCallbackMap.Create;

  HTTP := THTTPLib.Create;
  SimpleAI := TSimpleAI.Create;
  LoadConfig('');

  Handler['example'] := @Example_Handler;
  Handler['url'] := @URL_Handler;
end;

destructor TSimpleBotModule.Destroy;
begin
  ___HandlerCallbackMap.Free;
  SimpleAI.Free;
  HTTP.Free;
end;

procedure TSimpleBotModule.LoadConfig(DataName: string);
var
  i: integer;
  s, basedir: string;
  lst: TStrings;
begin
  FTelegramToken := Config[_TELEGRAM_CONFIG_TOKEN];

  try
    basedir := Config[_AI_CONFIG_BASEDIR];
    SimpleAI.Debug := Config[_AI_CONFIG_DEBUG];
    SimpleAI.AIName := Config[_AI_CONFIG_NAME];
  except
  end;

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

function TSimpleBotModule.execHandler(ActionName, Message: string): string;
var
  i: integer;
  h: THandlerCallback;
begin
  Result := SimpleAI.ResponseText;
  i := ___HandlerCallbackMap.IndexOf(ActionName);
  if i = -1 then
    Exit;
  h := ___HandlerCallbackMap.Data[i];
  Result := h(ActionName, SimpleAI.Parameters);
end;

function TSimpleBotModule.URL_Handler(const ActionName: string;
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

function TSimpleBotModule.Example_Handler(const ActionName: string;
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

  s := _SESSION[_AI_SESSION_VISITED];
  if s = '' then
  begin
    s := SimpleAI.GetResponse(_AI_RESPONSE_INTRODUCTION, '', _AI_RESPONSE_FIRSTSESSION);
    SimpleAI.SuffixText := s + SimpleAI.GetResponse(_AI_RESPONSE_INTRODUCTION,
      '', _AI_RESPONSE_ABOUTME);
    _SESSION[_AI_SESSION_VISITED] := '1';
    _SESSION[_AI_SESSION_LASTVISIT] := _GetTickCount;
  end;

  s := _SESSION[_AI_SESSION_LASTVISIT];
  lastvisit_time := StrToInt64(s);
  lastvisit_length := (_GetTickCount - lastvisit_time) div 3600000; // jam
  if lastvisit_length > 1 then
  begin
    SimpleAI.SuffixText := SimpleAI.GetResponse(_AI_RESPONSE_INTRODUCTION,
      '', _AI_RESPONSE_SECONDSESSION);
  end;

  if SimpleAI.Exec(Text) then
  begin
    text_response := SimpleAI.ResponseJson;

    if SimpleAI.Action <> '' then
    begin
      lst := Explode(SimpleAI.Action, _AI_ACTION_SEPARATOR);
      text_response := execHandler(lst[0], Message);
      lst.Free;

      json := TJSONUtil.Create;
      json.LoadFromJsonString(SimpleAI.ResponseJson);
      json['response/text'] := text_response;
      text_response := json.AsJSON;
      json.Free;
    end; //SimpleAI.Action;

  end
  else
  begin
    text_response := SimpleAI.ResponseJson;
    LogUtil.Add(Text, 'AI_LEARN');
  end;


  //---
  Result := text_response;
  _SESSION[_AI_SESSION_LASTVISIT] := _GetTickCount;
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
