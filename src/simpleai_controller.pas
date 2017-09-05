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
  common,
  simpleai_lib, dateutils, Dos, RegExpr, fpjson,
  IniFiles, Classes, SysUtils;

const
  _AI_NAME = 'CarikBot';
  _AI_COUNT__MINIMAL_ASKNAME = 5;
  _AI_ACTION_SEPARATOR = '|';
  _AI_CMD_OPENFILE = 'file';
  _AI_CMD_OPENJSONFILE = 'json';

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
    FTrimMessage: boolean;
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
    function isCommand(Msg: string): boolean;
    function execCommand(Msg: string): string;
    function openFile(FileName: string): string;
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
    property TrimMessage: boolean read FTrimMessage write FTrimMessage;
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

function TSimpleAI.StringReplacement(Text: string): string;
var
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

  dateTimePosition := now;

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
  regex.Expression := '%(.*)%';
  if regex.Exec(Result) then
  begin
    s := regex.Match[1];
    if FSimpleAILib.Parameters.Values[s] <> '' then
      Result := SimpleAILib.Intent.Entities.preg_replace(
        '%' + s + '%', FSimpleAILib.Parameters.Values[s], Result, True);
  end;
  regex.Free;

end;

procedure TSimpleAI.setDebug(AValue: boolean);
begin
  FSimpleAILib.Intent.Debug := AValue;
end;

function TSimpleAI.isCommand(Msg: string): boolean;
var
  lst: TStrings;
begin
  Result := False;
  lst := Explode(Msg, ':');

  //todo: is valid command

  if lst.Count > 1 then
    Result := True;
  lst.Free;
end;

function TSimpleAI.execCommand(Msg: string): string;
var
  s: string;
  lst: TStrings;
begin
  Result := Msg;
  lst := Explode(Msg, ':');
  case lst[0] of
    _AI_CMD_OPENFILE:
    begin
      s := trim(_BASEDIR + lst[1]);
      Result := openFile(s);
      if Result = '' then
        Result := Msg;
      Result := Trim( Result);
      Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
      Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
      Result := StringReplacement( Result);
    end;
    _AI_CMD_OPENJSONFILE:
    begin
      s := trim(_BASEDIR + lst[1]);
      Result := openFile(s);
      if Result = '' then
        Result := Msg;
      Result := StringReplacement( Result);
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

function TSimpleAI.getTimeSession: string;
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
  FMsg := '';
  FTrimMessage := False;
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

  if FTrimMessage then
  begin
    Text := ReplaceAll(Text, ['''', '"'], '');
  end;

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

  // is Command
  if isCommand(FResponseText.Text) then
  begin
    FResponseText.Text := execCommand(FResponseText.Text);
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

    // todo: use GetTickCount64
    RandSeed := GetTickCount; // more true random
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
    txt := txt + '"' + StringToJSONString(FResponseText[i]) + '"';
    if i < FResponseText.Count - 1 then
      txt := txt + ',';
  end;

  json := json + '';
  json := json + '{';
  json := json + '"code" : 0,';
  json := json + '"request" : {';
  json := json + '"text" : "' + StringToJSONString(FRequestText) + '"';
  json := json + '},';
  json := json + '"response" : {';
  json := json + '"intents" : {';
  json := json + '"action" : "' + actionName + '",';
  json := json + '"name" : "' + IntentName + '",';
  if Debug then
  begin
    json := json + '"key" : "' + FSimpleAILib.Intent.IntentKey + '",';
    json := json + '"pattern" : "' + StringToJSONString(FSimpleAILib.Pattern) + '",';
  end;
  json := json + '"parameters" : {';

  //json := json + '"Greeting" : "' + '-' + '"';
  for i := 0 to FSimpleAILib.Parameters.Count - 1 do
  begin
    v := FSimpleAILib.Parameters.ValueFromIndex[i];
    v := StringToJSONString(v);
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
