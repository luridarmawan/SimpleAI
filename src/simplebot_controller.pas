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
  THandlerCallback = function(const ATagName: string;
    AParams: TStringList): string of object;
  THandlerCallbackMap = specialize TStringHashMap<THandlerCallback>;

  { TSimpleBotModule }

  TSimpleBotModule = class(TMyCustomWebModule)
  private
    ChatID, MessageID: string;
    FTelegramToken: string;
    Text: string;
    HTTP: THTTPLib;
    HTTP_Response: IHTTPResponse;
    procedure BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
    function getHandler(const TagName: string): THandlerCallback;

    procedure LoadConfig(DataName: string);
    procedure setHandler(const TagName: string; AValue: THandlerCallback);
  public
    SimpleBOT: TSimpleAI;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;

    property TelegramToken: string read FTelegramToken write FTelegramToken;
    function TelegramSend(ChatIDRef, ReplyToMessageID, Message: string): boolean;

    property Handler[const TagName: string]: THandlerCallback
      read getHandler write setHandler;

  end;

var
  ___HandlerCallbackMap: THandlerCallbackMap;

implementation

uses json_lib, common;

constructor TSimpleBotModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  ___HandlerCallbackMap := THandlerCallbackMap.Create;

  SimpleBOT := TSimpleAI.Create;
  LoadConfig('');

  HTTP := THTTPLib.Create;
end;

destructor TSimpleBotModule.Destroy;
begin
  ___HandlerCallbackMap.Free;
  SimpleBOT.Free;
  HTTP.Free;

  inherited Destroy;
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
    SimpleBOT.Debug := Config[_AI_CONFIG_DEBUG];
    SimpleBOT.AIName := Config[_AI_CONFIG_NAME];
  except
  end;

  // load Entities
  s := Config[_AI_CONFIG_ENTITIES];
  lst := Explode(s, ',');
  for i := 0 to lst.Count - 1 do
  begin
    SimpleBOT.AddEntitiesFromFile(basedir + lst[i]);
  end;
  lst.Free;

  // load Intents
  s := Config[_AI_CONFIG_INTENTS];
  lst := Explode(s, ',');
  for i := 0 to lst.Count - 1 do
  begin
    SimpleBOT.AddIntentFromFile(basedir + lst[i]);
  end;
  lst.Free;

  // load Response
  s := Config[_AI_CONFIG_RESPONSE];
  lst := Explode(s, ',');
  for i := 0 to lst.Count - 1 do
  begin
    SimpleBOT.AddResponFromFile(basedir + lst[i]);
  end;
  lst.Free;

end;

procedure TSimpleBotModule.setHandler(const TagName: string; AValue: THandlerCallback);
begin
  ___HandlerCallbackMap[TagName] := AValue;
end;

function TSimpleBotModule.getHandler(const TagName: string): THandlerCallback;
begin
  Result := ___HandlerCallbackMap[TagName];
end;

// Init First
procedure TSimpleBotModule.BeforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
end;

// GET Method Handler
procedure TSimpleBotModule.Get;
begin
  Response.Content := '{}';
end;

// POST Method Handler
procedure TSimpleBotModule.Post;
var
  json: TJSONUtil;
  result_json: TJSONData;
  s, text_response, search_title: string;

  lastvisit_time, lastvisit_length: cardinal;
begin
  if _GET['_DEBUG'] <> '' then
    SimpleBOT.Debug := True;

  // telegram style
  //   {"message":{"message_id":0,"text":"Hi","chat":{"id":0}}}
  json := TJSONUtil.Create;
  json.LoadFromJsonString(Request.Content);
  Text := json['message/text'];
  if Text = 'False' then
    Text := '';
  MessageID := json['message/message_id'];
  ChatID := json['message/chat/id'];
  json.Free;

  // jika tidak ada di body, ambil dari parameter POST
  if Text = '' then
    Text := _POST['text'];

  text_response := '';
  SimpleBOT.PrefixText := '';
  SimpleBOT.SuffixText := '';

  s := _SESSION[_AI_SESSION_VISITED];
  if s = '' then
  begin
    s := SimpleBOT.GetResponse(_AI_RESPONSE_INTRODUCTION, '', _AI_RESPONSE_FIRSTSESSION);
    SimpleBOT.SuffixText := s + SimpleBOT.GetResponse(_AI_RESPONSE_INTRODUCTION,
      '', _AI_RESPONSE_ABOUTME);
    _SESSION[_AI_SESSION_VISITED] := '1';
    _SESSION[_AI_SESSION_LASTVISIT] := _GetTickCount;
  end;

  lastvisit_time := _SESSION[_AI_SESSION_LASTVISIT];
  lastvisit_length := (_GetTickCount - lastvisit_time) div 3600000; // jam
  if lastvisit_length > 1 then
  begin
    SimpleBOT.SuffixText := SimpleBOT.GetResponse(_AI_RESPONSE_INTRODUCTION,
      '', _AI_RESPONSE_SECONDSESSION);
  end;

  if SimpleBOT.Exec(Text) then
  begin
    text_response := SimpleBOT.ResponseJson;

    if SimpleBOT.Action <> '' then
    begin
      text_response := 'action: ' + SimpleBOT.Action;

      case SimpleBOT.Action of
        'property_search':
        begin
          // TODO: buat procedural di sini

        end;
        else
        begin
        end;
      end;

      if ((SimpleBOT.Action = 'property_search') or
        (SimpleBOT.Action = 'property_request')) then // R123

      begin
        text_response := 'searching property ...';

        HTTP.URL := 'https://new-api.rumah123.com/consumer/v1/listings';
        HTTP.AddHeader('X-Client-Type',
          'KlX3eY1TWGJN5j3+qT19sCGwXW3cqYTha4OBuJPgm7/ehFvDxCONeQlcDpje2Yo7VDWCauHhdXXLNPER3Ho5X2iyCaFnkkqwlnzc+rTSRYEmTZnrQkwcLL51YzPd/6/TrWn5eMQQTMdacFHlB0vl8tJhEiR0fWCLOChnNOVVYEe4G3NCfxiT0aNApMhSNPnYcMyNKlId1qmBu5M70XXHrw==|HSYFSsKh14UNT5gWl4F6l2encUTyHnRf8asi7fszaQo=');

        HTTP.FormData['type'] := SimpleBOT.Parameters.Values['PropertyType'];
        HTTP.FormData['category'] := SimpleBOT.Parameters.Values['PropertyCategory'];
        if SimpleBOT.Parameters.Values['PropertyCategory'] = '' then
          HTTP.FormData['category'] := 's';

        HTTP.FormData['keyword'] := SimpleBOT.Parameters.Values['whatever'];
        HTTP_Response := HTTP.Post();

        //if HTTP_Response.ResultCode = 200 then
        begin
          result_json := GetJSON(HTTP_Response.ResultText);
          search_title := result_json.FindPath('data.title_keyword').AsString;

          text_response := '<b>' + UrlDecode(search_title) + '</b>' + '<br>URL:';
          with HTMLUtil.Create do
          begin
            try
              text_response :=
                text_response + '<br>- ' + Link(
                result_json.FindPath('data.rows[0].ads.tagline').AsString,
                result_json.FindPath('data.rows[0].ads.url').AsString,
                ['target="_blank"']);
              text_response :=
                text_response + '<br>- ' + Link(
                result_json.FindPath('data.rows[1].ads.tagline').AsString,
                result_json.FindPath('data.rows[1].ads.url').AsString,
                ['target="_blank"']);
              text_response :=
                text_response + '<br>- ' + Link(
                result_json.FindPath('data.rows[2].ads.tagline').AsString,
                result_json.FindPath('data.rows[2].ads.url').AsString,
                ['target="_blank"']);
              SimpleBOT.ResponseText :=
                result_json.FindPath('data.rows[0].ads.url').AsString;
            except
            end;
          end;

          result_json.Free;
        end;

        json := TJSONUtil.Create;
        json.LoadFromJsonString(SimpleBOT.ResponseJson);
        json['response/text'] := text_response;
        text_response := json.AsJSON;
        json.Free;

      end; // R123

    end; //SimpleBOT.Action;

  end
  else
  begin
    text_response := SimpleBOT.ResponseJson;
    LogUtil.Add(Text, 'AI_LEARN');
  end;


  // Send To Telegram
  if s2i(ChatID) <> 0 then
    TelegramSend(ChatID, MessageID, SimpleBOT.ResponseText);

  //---
  _SESSION[_AI_SESSION_LASTVISIT] := _GetTickCount;
  Response.Content := text_response;
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
