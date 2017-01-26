{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit kamus_controller;

{$mode objfpc}{$H+}

interface

uses
  kamusibacor_integration, kamuskemdikbud_integration,
  common, http_lib, fpjson, json_lib,
  dateutils, Classes, SysUtils;

type

  { TKamusController }

  TKamusController = class
  private
    FCache: boolean;
    FToken: string;
    function getDataKamusIbacor(Text: string): string;
    function getDataKamusKemdikbud(Text: string): string;
    function findKemdikbud(Text: string): string;
    function findIbacor(Text: string): string;
  public
    constructor Create;
    destructor Destroy;

    property Cache: boolean read FCache write FCache;
    property Token: string read FToken write FToken;
    function Find(Text: string): string;
  end;


implementation

const
  _KAMUS_CACHE_DIR = 'ztemp/cache/kamus/';
  _KAMUS_CACHE_EXTENSION = '.txt';
  _KAMUS_MSG_ERROR = 'Maaf, saat ini data belum bisa saya temukan';
  _KAMUS_MSG_ERROR_TOKEN = 'Maaf, belum bisa akses ke database kamus';
  _KAMUS_MAX_RESULT = 2;

var
  Response: IHTTPResponse;

{ TKamusController }

constructor TKamusController.Create;
begin
  FCache := True;

end;

destructor TKamusController.Destroy;
begin

end;

function TKamusController.getDataKamusIbacor(Text: string): string;
begin
  Result := '';
  with TKamusIbacorIntegration.Create do
  begin
    Result := Find(Text);
    Free;
  end;
end;

function TKamusController.getDataKamusKemdikbud(Text: string): string;
begin
  Result := '';
  with TKamusIntegration.Create do
  begin
    Result := Find(Text);
    Free;
  end;
end;

function TKamusController.Find(Text: string): string;
var
  lst : TStrings;
begin
  lst := Explode( Text, ' ');
  Result := findKemdikbud( lst.ValueFromIndex[0]);
  Result := '*tentang ' + Text + ':*\n' + Result;
  lst.Free;
end;

function TKamusController.findKemdikbud(Text: string): string;
var
  forceGetKamus: boolean;
  i: integer;
  cacheFile: string;
  cacheData: TStringList;
begin
  Result := '';
  cacheData := TStringList.Create;
  forceGetKamus := False;
  cacheFile := _KAMUS_CACHE_DIR + Text + _KAMUS_CACHE_EXTENSION;

  forceGetKamus := False;
  if FileExists(cacheFile) then
  begin
    i := HoursBetween(FileDateToDateTime(FileAge(cacheFile)), now);
    if i = 0 then
      cacheData.LoadFromFile(cacheFile)
    else
      forceGetKamus := True;
  end
  else
    forceGetKamus := True;

  if forceGetKamus then
  begin
    cacheData.Text := getDataKamusKemdikbud(Text);
    if cacheData.Text = '' then
    begin
      Result := _KAMUS_MSG_ERROR;
      cacheData.Free ;
      Exit;
    end;
    if FCache then
      cacheData.SaveToFile(cacheFile);
  end;

  Result := cacheData.Text;
  cacheData.Free;
end;

function TKamusController.findIbacor(Text: string): string;
var
  s, cacheFile: string;
  cacheData: TStringList;
  forceGetKamus: boolean;
  i: integer;
  jsonData: TJSONData;
begin
  Result := '';
  Text := trim(Text);
  if Text = '' then
    Exit;
  if FToken = '' then
  begin
    Result := _KAMUS_MSG_ERROR_TOKEN;
    Exit;
  end;
  cacheData := TStringList.Create;

  forceGetKamus := False;
  cacheFile := _KAMUS_CACHE_DIR + Text + _KAMUS_CACHE_EXTENSION;
  if FileExists(cacheFile) then
  begin
    i := HoursBetween(FileDateToDateTime(FileAge(cacheFile)), now);
    if i = 0 then
      cacheData.LoadFromFile(cacheFile)
    else
      forceGetKamus := True;
  end
  else
    forceGetKamus := True;

  if forceGetKamus then
  begin
    cacheData.Text := getDataKamusIbacor(Text);
    if cacheData.Text = '' then
    begin
      Result := _KAMUS_MSG_ERROR;
      cacheData.Free;
      Exit;
    end;
    if FCache then
      cacheData.SaveToFile(cacheFile);
  end;

  jsonData := GetJSON(cacheData.Text);
  Result := UpperCase(Text) + ':';
  try
    for i := 0 to _KAMUS_MAX_RESULT - 1 do
    begin
      s := jsonData.GetPath('kamus[0][' + i2s(i) + '].arti[0]').AsString;
      Result := Result + #13'-' + s;
    end;
  except
  end;

  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
  cacheData.Free;
end;


end.
