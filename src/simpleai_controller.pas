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
  simpleai_lib, Dos, RegExpr,
  IniFiles, Classes, SysUtils;

const
  _AI_NAME = 'RockBot';
  _AI_ACTION_SEPARATOR = '|';
  _AI_COUNT__MINIMAL_ASKNAME = 5;


type

  { TSimpleAI }

  TSimpleAI = class
  private
    FAIName: string;
    FKeyName: string;
    FMsg: string;
    FPrefixText: string;
    FRequestText: string;
    FResponseText: TStringList;
    FSimpleAILib: TSimpleAILib;
    FResponseData: TMemIniFile;
    FResponseDataAsList: TStringList;
    FSuffixText: string;
    FVarName: string;

    function getResponseJson: string;
    function getTimeSession(): string;

    function getAction: string;
    function getDebug: boolean;
    function getIntentName: string;
    function getParameters: TStrings;
    function getParameterValue(KeyName: string): string;
    function getPatternString: string;

    procedure setDebug(AValue: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    function AddIntentFromFile(FileName: string): boolean;
    function AddEntitiesFromFile(FileName: string): boolean;
    function AddResponFromFile(FileName: string): boolean;

    function Exec(Text: string; AutoResponse: boolean = True): boolean;
    function GetQuestions(IntentName: string; Key: string = '';
      MsgCount: integer = _AI_COUNT__MINIMAL_ASKNAME): string;
    function GetResponse(IntentName: string; Action: string = '';
      EntitiesKey: string = ''): string;
    function SetResponseData(List: TStrings): boolean;
    function StringReplacement(Text: string): string;

    property AIName: string read FAIName write FAIName;
    property SimpleAILib: TSimpleAILib read FSimpleAILib;
    property Action: string read getAction;
    property IntentName: string read getIntentName;
    property KeyName: string read FKeyName;
    property VarName: string read FVarName;
    property Parameters: TStrings read getParameters;
    property Values[KeyValue: string]: string read getParameterValue; default;
    property ResponseText: TStringList read FResponseText write FResponseText;
    property ResponseJson: string read getResponseJson;
    property ResponData: TMemIniFile read FResponseData;
    property PrefixText: string read FPrefixText write FPrefixText;
    property SuffixText: string read FSuffixText write FSuffixText;
    property Debug: boolean read getDebug write setDebug;
    property Pattern: string read getPatternString;
    property Msg: string read FMsg;
  end;

implementation

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

function TSimpleAI.StringReplacement(Text: string): string;
var
  s, t: string;
  y, m, d: word;
  regex: TRegExpr;
begin
  Result := Text;
  Result := FSimpleAILib.Intent.Entities.preg_replace(
    '%(time_session)%', getTimeSession, Result, False);
  Result := FSimpleAILib.Intent.Entities.preg_replace(
    '%(AIName)%', AIName, Result, False);

  // waktu
  if FSimpleAILib.Intent.Entities.preg_match('%(time)%', Result) then
  begin
    DecodeDate(Date, y, m, d);
    s := Parameters.Values['Waktu'];
    t := '';
    case s of
      'jam': t := FormatDateTime('hh:nn', Now) + ' ' + getTimeSession;
      'hari': t := NamaHari[DayOfWeek(Now)];
      'bulan': t := NamaBulan[m];
      'tahun': t := IntToStr(y);
      'tanggal': t := FormatDateTime('dd/mm/yyyy', Now)
    end;
    t := s + ' ' + t;
    Result := FSimpleAILib.Intent.Entities.preg_replace(
      '%(time)%', t, Result, False);
  end;

  regex := TRegExpr.Create;
  regex.Expression := '%(.*)%';
  if regex.Exec(Result) then
  begin
    s := regex.Match[1];
    Result := SimpleAILib.Intent.Entities.preg_replace(
      '%'+s+'%', FSimpleAILib.Parameters.Values[ s], Result, True);
  end;
  regex.Free;

end;

procedure TSimpleAI.setDebug(AValue: boolean);
begin
  FSimpleAILib.Intent.Debug := AValue;
end;

function TSimpleAI.getTimeSession: string;
var
  Hour, Min, Sec, HSec: word;
begin
  GetTime(Hour, Min, Sec, HSec);
  Result := 'pagi';
  if Hour > 11 then
    Result := 'siang';
  if Hour > 15 then
    Result := 'sore';
  if Hour > 17 then
    Result := 'petang';
  if Hour > 18 then
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
  FMsg := '';
end;

destructor TSimpleAI.Destroy;
begin
  FResponseText.Free;
  ;
  FResponseDataAsList.Free;
  FResponseData.Free;
  FSimpleAILib.Free;
end;

function TSimpleAI.AddIntentFromFile(FileName: string): boolean;
begin
  FSimpleAILib.AddDataIntentFromFile(FileName);
end;

function TSimpleAI.AddEntitiesFromFile(FileName: string): boolean;
begin
  FSimpleAILib.AddDataEntitiesFromFile(FileName);
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
begin
  FMsg := '';
  Result := False;
  if Text = '' then
    Exit;
  Result := FSimpleAILib.Exec(Text);
  if not AutoResponse then
    Exit;

  FRequestText := Text;
  if Result then
  begin
    FResponseText.Add(GetResponse(IntentName, Action, ''));
  end
  else
  begin
    FResponseText.Add(GetResponse('none', '', ''));
  end;

  FResponseText.Text := FPrefixText + FResponseText.Text + FSuffixText;
  if FSimpleAILib.Intent.Entities.preg_match('%(.*)%', FResponseText.Text) then
  begin
    FResponseText.Text := StringReplacement(FResponseText.Text);
  end;
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
  item_list: TStringList;
begin
  Result := '';

  if EntitiesKey <> '' then
  begin
    Result := FResponseData.ReadString(IntentName, EntitiesKey, '._');
    Exit;
  end;

  item_list := TStringList.Create;
  FResponseData.ReadSectionRaw(IntentName, item_list);

  if item_list.Count > 0 then
  begin
    Randomize;
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
  json, actionName, txt, v: string;
  lst: TStrings;
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
    txt := txt + '"' + FResponseText[i] + '"';
    if i < FResponseText.Count - 1 then
      txt := txt + ',';
  end;

  json := json + '';
  json := json + '{';
  json := json + '"code" : 0,';
  json := json + '"request" : {';
  json := json + '"text" : "' + FRequestText + '"';
  json := json + '},';
  json := json + '"response" : {';
  json := json + '"intents" : {';
  json := json + '"action" : "' + actionName + '",';
  json := json + '"name" : "' + IntentName + '",';
  if Debug then
  begin
    json := json + '"key" : "' + SimpleAILib.Intent.IntentKey + '",';
    json := json + '"pattern" : "' + FSimpleAILib.Pattern + '",';
  end;
  json := json + '"parameters" : {';

  //json := json + '"Greeting" : "' + '-' + '"';
  for i := 0 to FSimpleAILib.Parameters.Count - 1 do
  begin
    v := FSimpleAILib.Parameters.ValueFromIndex[i];
    v := StringReplace(v, '\', '\\', [rfReplaceAll]);
    json := json + '"' + FSimpleAILib.Parameters.Names[i] + '" : "' + v + '"';

    if i < FSimpleAILib.Parameters.Count - 1 then
      json := json + ',';
  end;
  json := json + '}';
  json := json + '},';
  json := json + '"text" : [' + txt + ']';
  if FMsg <> '' then
    json := json + ',"msg" : "' + FMsg + '"';
  json := json + '}';
  json := json + '}';

  lst.Free;
  Result := json;
end;


end.
