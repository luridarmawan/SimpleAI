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
  kamuskemdikbud_integration,
  common, http_lib, fpjson, json_lib,
  dateutils, Classes, SysUtils;

type

  { TKamusController }

  TKamusController = class
  private
    FCache: boolean;
    function getDataKamusKemdikbud(Text: string): string;
    function findKemdikbud(Text: string): string;
  public
    constructor Create;
    destructor Destroy;

    property Cache: boolean read FCache write FCache;
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
  //Result := findKemdikbud( lst.ValueFromIndex[0]);
  Result := findKemdikbud( Text);
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

end.
