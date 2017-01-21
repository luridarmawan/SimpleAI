unit jadwalsholat_controller;

{$mode objfpc}{$H+}

interface

uses
  common, http_lib, fpjson, json_lib,
  dateutils, Classes, SysUtils;

type

  { TJadwalSholatController }

  TJadwalSholatController = class
  private
    FCache: boolean;
    function getJadwalSholatData(CityName: string; Day: integer): string;
  public
    constructor Create;
    destructor Destroy;

    property Cache: boolean read FCache write FCache;
    function Find(CityName: string; Day: integer): string;
  end;


implementation

const
  _JADWALSHOLAT_DEFAULT_CITY = 'jakarta';
  _JADWALSHOLAT_MSG_ERROR = 'maaf... \ngagal mendapatkan informasi jadwal sholat';
  _JADWALSHOLAT_LAMPUMIMPI_URL =
    'http://lampumimpi.com/jadwal_sholat/kota/%city/hari/%day';
  _JADWALSHOLAT_CACHE_DIR = 'ztemp/cache/jadwalsholat/';
  _JADWALSHOLAT_CACHE__EXTENSION = '.txt';

// example:
//   http://lampumimpi.com/jadwal_sholat/kota/bandung/hari/19
// result:
//   {"code":200,"success":true,"message":"Jadwal sholat kota bandung.","data":[{"hari":"19","shubuh":"04:26","dzuhur":"12:04","ashar":"15:28","magrib":"18:18","isya":"19:33"}]}

var
  Response: IHTTPResponse;

{ TJadwalSholatController }

constructor TJadwalSholatController.Create;
begin
  FCache := True;
end;

destructor TJadwalSholatController.Destroy;
begin
end;

function TJadwalSholatController.getJadwalSholatData(CityName: string;
  Day: integer): string;
var
  s, return: string;
  json: TJSONUtil;
  jsonData: TJSONData;
  i: integer;
  httpClient: THTTPLib;
begin
  Result := '';

  httpClient := THTTPLib.Create;
  s := UrlEncode(LowerCase(CityName), False);
  if s = '' then
    s := _JADWALSHOLAT_DEFAULT_CITY;
  httpClient.URL := StringReplace(_JADWALSHOLAT_LAMPUMIMPI_URL, '%city', s,
    [rfReplaceAll]);
  httpClient.URL := StringReplace(httpClient.URL, '%day', i2s(Day), [rfReplaceAll]);

  Response := httpClient.Get;
  if Response.ResultCode <> 200 then
    Exit;

  try
    jsonData := GetJSON(Response.ResultText);
    if jsonData.GetPath('code').AsString = '200' then
    begin
      Result := Response.ResultText;
    end;
    jsonData.Free;
  except
    on E: Exception do
    begin
    end;
  end;

  httpClient.Free;
end;

function TJadwalSholatController.Find(CityName: string; Day: integer): string;
var
  cacheFile, s: string;
  json: TJSONUtil;
  jsonData: TJSONData;
  forceGetJadwal: boolean;
  i: integer;
  cacheData: TStringList;
begin
  Result := '';
  cacheData := TStringList.Create;

  if CityName = '' then
    CityName := _JADWALSHOLAT_DEFAULT_CITY;
  cacheFile := _JADWALSHOLAT_CACHE_DIR + CityName + '-' + i2s(Day) +
    _JADWALSHOLAT_CACHE__EXTENSION;
  forceGetJadwal := False;
  if FileExists(cacheFile) then
  begin
    i := HoursBetween(FileDateToDateTime(FileAge(cacheFile)), now);
    if i = 0 then
      cacheData.LoadFromFile(cacheFile)
    else
      forceGetJadwal := True;
  end
  else
    forceGetJadwal := True;

  if forceGetJadwal then
  begin
    cacheData.Text := getJadwalSholatData(CityName, Day);
    if cacheData.Text = '' then
    begin
      Result := _JADWALSHOLAT_MSG_ERROR;
      Exit;
    end;
    if FCache then
      cacheData.SaveToFile(cacheFile);
  end;

  try
    jsonData := GetJSON(cacheData.Text);
    s := jsonData.GetPath('data').AsJSON;
    s := Copy(s, 2, length(s) - 2); // force jsonarray to jsonstring

    json := TJSONUtil.Create;
    json.LoadFromJsonString(s);
    s := jsonData.GetPath('message').AsString;
    s := s + '\nshubuh: ' + json['shubuh'];
    s := s + '\ndzuhur: ' + json['dzuhur'];
    s := s + '\nashar: ' + json['ashar'];
    s := s + '\nmagrib: ' + json['magrib'];
    s := s + '\nisya: ' + json['isya'];
    json.Free;

    Result := s;
  except
    on E: Exception do
    begin
      Result := _JADWALSHOLAT_MSG_ERROR; //e.Message;
    end;
  end;

end;

end.
