unit simpleai_controller;

{$mode objfpc}{$H+}

interface

uses
  simpleai_lib, Dos,
  IniFiles, Classes, SysUtils;

const
  _AI_NAME = 'RockBot';

type

  { TSimpleAI }

  TSimpleAI = class
  private
    FAIName: string;
    FMsg: string;
    FPrefixText: string;
    FRequestText: string;
    FResponseText: string;
    FSimpleAILib: TSimpleAILib;
    FResponseData: TMemIniFile;
    FResponseDataAsList: TStringList;
    FSuffixText: string;

    function getResponseJson: string;
    function getTimeSession(): string;

    function getAction: string;
    function getDebug: boolean;
    function getIntentName: string;
    function getParameters: TStrings;
    function getParameterValue(KeyName: string): string;
    function getPatternString: string;

    function StringReplacement(Text: string): string;
    procedure setDebug(AValue: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    function AddIntentFromFile(FileName: string): boolean;
    function AddEntitiesFromFile(FileName: string): boolean;
    function AddResponFromFile(FileName: string): boolean;

    function Exec(Text: string; AutoResponse: boolean = True): boolean;
    function GetResponse(IntentName, Action: string; EntitiesKey: string = ''): string;

    property AIName: string read FAIName write FAIName;
    property SimpleAI: TSimpleAILib read FSimpleAILib;
    property Action: string read getAction;
    property IntentName: string read getIntentName;
    property Parameters: TStrings read getParameters;
    property Values[KeyName: string]: string read getParameterValue; default;
    property ResponseText: string read FResponseText write FResponseText;
    property ResponseJson: string read getResponseJson;
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
  NamaBulan: TMonthNameArray = ('Januari', 'Februari', 'Maret', 'April', 'Mei', 'Juni',
    'Juli', 'Augustus', 'September', 'Oktober', 'November', 'Desember');

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
  FMsg := '';
end;

destructor TSimpleAI.Destroy;
begin
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
  Result := False;
  FResponseText := '';
  if Text = '' then
    Exit;
  Result := FSimpleAILib.Exec(Text);
  if not AutoResponse then
    Exit;

  FRequestText := Text;
  if Result then
  begin
    FResponseText := GetResponse(IntentName, Action, '');
  end
  else
  begin
    FResponseText := GetResponse('none', '', '');
  end;

  FResponseText := FPrefixText + FResponseText + FSuffixText;
  if FSimpleAILib.Intent.Entities.preg_match('%(.*)%', FResponseText) then
  begin
    FResponseText := StringReplacement(FResponseText);
  end;
end;

function TSimpleAI.GetResponse(IntentName, Action: string; EntitiesKey: string): string;
var
  i: integer;
  item_list: TStringList;
begin
  Result := '';
  FMsg := '';
  FResponseText := '';

  if EntitiesKey <> '' then
  begin
    FResponseText := FResponseData.ReadString(IntentName, EntitiesKey, '._');
    Result := FResponseText;
    Exit;
  end;

  item_list := TStringList.Create;
  FResponseData.ReadSectionRaw(IntentName, item_list);

  if item_list.Count > 0 then
  begin
    Randomize;
    i := Random(item_list.Count);

    FResponseText := item_list[i];
    if Debug then
      FMsg := ' (' + IntToStr(i + 1) + '/' + IntToStr(item_list.Count) + ')';
    FResponseText := copy(FResponseText, pos('=', FResponseText) + 1);
  end;

  Result := FResponseText;
  item_list.Free;
end;


function TSimpleAI.getResponseJson: string;
var
  i: integer;
  json: string;
begin
  Result := '';

  json := json + '';
  json := json + '{';
  json := json + '"code" : 0,';
  json := json + '"request" : {';
  json := json + '"text" : "' + FRequestText + '"';
  json := json + '},';
  json := json + '"response" : {';
  json := json + '"intents" : {';
  json := json + '"action" : "' + Action + '",';
  json := json + '"name" : "' + IntentName + '",';
  if Debug then
    json := json + '"pattern" : "' + SimpleAI.Pattern + '",';
  json := json + '"parameters" : {';

  //json := json + '"Greeting" : "' + '-' + '"';
  for i := 0 to FSimpleAILib.Parameters.Count - 1 do
  begin
    json := json + '"' + FSimpleAILib.Parameters.Names[i] + '" : "' +
      FSimpleAILib.Parameters.ValueFromIndex[i] + '"';
    if i < FSimpleAILib.Parameters.Count - 1 then
      json := json + ',';
  end;
  json := json + '}';
  json := json + '},';
  json := json + '"text" : "' + FResponseText + '"';
  if FMsg <> '' then
    json := json + ',"msg" : "' + FMsg + '"';
  json := json + '}';
  json := json + '}';
  Result := json;
end;


end.
