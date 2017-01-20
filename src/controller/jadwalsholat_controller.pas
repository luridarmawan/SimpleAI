unit jadwalsholat_controller;

{$mode objfpc}{$H+}

interface

uses
  common, http_lib, fpjson, json_lib,
  Classes, SysUtils;

type

  { TJadwalSholatController }

  TJadwalSholatController = class
  private
  public
    constructor Create;
    destructor Destroy;

    function Find(CityName: string; Day: integer): string;
  end;


implementation

const
  _JADWALSHOLAT_DEFAULT_CITY = 'jakarta';
  _JADWALSHOLAT_MSG_ERROR = 'maaf... \ngagal mendapatkan informasi jadwal sholat';
  _JADWALSHOLAT_LAMPUMIMPI_URL =
    'http://lampumimpi.com/jadwal_sholat/kota/%city/hari/%day';

// example:
//   http://lampumimpi.com/jadwal_sholat/kota/bandung/hari/19
// result:
//   {"code":200,"success":true,"message":"Jadwal sholat kota bandung.","data":[{"hari":"19","shubuh":"04:26","dzuhur":"12:04","ashar":"15:28","magrib":"18:18","isya":"19:33"}]}

var
  Response: IHTTPResponse;

{ TJadwalSholatController }

constructor TJadwalSholatController.Create;
begin
end;

destructor TJadwalSholatController.Destroy;
begin
end;

function TJadwalSholatController.Find(CityName: string; Day: integer): string;
var
  s, return: string;
  json: TJSONUtil;
  jsonData: TJSONData;
begin
  return := '';

  with THTTPLib.Create do
  begin
    s := UrlEncode(LowerCase(CityName), False);
    if s = '' then
      s := _JADWALSHOLAT_DEFAULT_CITY;
    URL := StringReplace(_JADWALSHOLAT_LAMPUMIMPI_URL, '%city', s, [rfReplaceAll]);
    URL := StringReplace(url, '%day', i2s(Day), [rfReplaceAll]);

    AddHeader('Cache-Control', 'no-cache');
    Response := Get;

    if Response.ResultCode = 200 then
    begin
      try
        jsonData := GetJSON(Response.ResultText);
        if jsonData.GetPath('code').AsString = '200' then
        begin
          s := jsonData.GetPath('data').AsJSON;
          s := Copy(s, 2, length(s) - 2); // force jsonarray to jsonstring
        end;

        json := TJSONUtil.Create;
        json.LoadFromJsonString(s);

        s := jsonData.GetPath('message').AsString;
        s := s + '\nshubuh: ' + json['shubuh'];
        s := s + '\ndzuhur: ' + json['dzuhur'];
        s := s + '\nashar: ' + json['ashar'];
        s := s + '\nmagrib: ' + json['magrib'];
        s := s + '\nisya: ' + json['isya'];

        json.Free;

        return := s;
      except
        on E: Exception do
        begin
          return := e.Message;
        end;
      end;
      jsonData.Free;
    end
    else
    begin
      return := _JADWALSHOLAT_MSG_ERROR;
    end;
    Free;
  end;

  Result := return;
end;

end.
