unit simpleairedis_controller;

{$mode objfpc}{$H+}

interface

uses
  {$IFNDEF Windows}
  cthreads,
  {$ENDIF}
  common,
  fpcgi, simpleai_controller, redis_controller,
  Classes, SysUtils;

const
  _AI_COUNT__MINIMAL_ASKNAME = 5;
  _AI_ACTION_SEPARATOR = '|';

  _AI_REDIS_ENTITIES = 'AI_ENTITIES';
  _AI_REDIS_INTENTS = 'AI_INTENTS';
  _AI_REDIS_RESPONSES = 'AI_RESPONSES';

type

  { TSimpleAIRedis }

  TSimpleAIRedis = class(TSimpleAI)
  private
    FUseRedis: boolean;
    FRedis: TRedisConstroller;
  public
    constructor Create; override;
    destructor Destroy; override;

    function RedisInit: boolean;
    function LoadDataFromRedis: boolean;
  published
    property UseRedis: boolean read FUseRedis write FUseRedis;
  end;

implementation

{ TSimpleAIRedis }


constructor TSimpleAIRedis.Create;
begin
  inherited Create;
end;

destructor TSimpleAIRedis.Destroy;
begin
  inherited Destroy;
end;

{
constructor TSimpleAIRedis.Create;
begin
  inherited Create;
  FUseRedis := False;
  //FRedis := TRedisConstroller.Create;
end;

destructor TSimpleAIRedis.Destroy;
begin
  if Assigned( FRedis) then
    FRedis.Free;
  inherited Destroy;
end;
}
function TSimpleAIRedis.RedisInit: boolean;
var
  redis_text: WideString;
  lst: TStrings;
begin
  Result := False;
  if FRedis.Ping then
  begin
    lst := TStringList.Create;

    SimpleAILib.Intent.Entities.Data.GetStrings(lst);
    redis_text := UrlEncode(lst.Text);
    FRedis[_AI_REDIS_ENTITIES] := redis_text;

    SimpleAILib.Intent.Data.GetStrings(lst);
    redis_text := UrlEncode(lst.Text);
    FRedis[_AI_REDIS_INTENTS] := redis_text;

    ResponseData.GetStrings(lst);
    redis_text := UrlEncode(lst.Text);
    FRedis[_AI_REDIS_RESPONSES] := redis_text;

    lst.Free;
    Result := True;
  end;

end;

function TSimpleAIRedis.LoadDataFromRedis: boolean;
var
  erri: integer;
  redis_text: WideString;
  lst: TStrings;
begin
  Result := False;
  if not FRedis.Ping then
    Exit;

  erri := 0;
  lst := TStringList.Create;

  redis_text := FRedis[_AI_REDIS_ENTITIES];
  lst.Text := UrlDecode(redis_text);
  if not SimpleAILib.Intent.Entities.SetData(lst) then
    erri := erri + 1;

  redis_text := FRedis[_AI_REDIS_INTENTS];
  lst.Text := UrlDecode(redis_text);
  if not SimpleAILib.Intent.SetData(lst) then
    erri := erri + 2;

  redis_text := FRedis[_AI_REDIS_RESPONSES];
  lst.Text := UrlDecode(redis_text);
  if not SetResponseData(lst) then
    erri := erri + 8;

  lst.Free;
end;

end.

