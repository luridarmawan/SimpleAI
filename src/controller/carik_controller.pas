{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit carik_controller;

{$mode objfpc}{$H+}

interface

uses
  fastplaz_handler, common,
  IniFiles, fpjson,
  Classes, SysUtils;

type

  { TCarikController }

  TCarikController = class
  private
    DataFile: TextFile;
    FGroupName: string;
    FPath: string;
    FReady: boolean;
    FData: TIniFile;
    function getIsRecording: boolean;
    function SaveToFile(Text: string): boolean;
    procedure setGroupName(AValue: string);
    procedure setPath(AValue: string);
  public
    constructor Create;
    destructor Destroy;
    function CarikHandler(const IntentName: string; Params: TStrings): string;
    function Start: boolean;
    function Stop: boolean;
    procedure RecordTelegramMessage(Message: string);

    property Ready: boolean read FReady;
    property Path: string read FPath write setPath;
    property GroupName: string read FGroupName write setGroupName;
    property Recording: boolean read getIsRecording;
  end;


implementation

const
  _CARIK_PATH_DEFAULT = 'files/carik/';
  _CARIK_DATA_FILE = '0-carik.dat';
  _CARI_FILE_EXTENSION = '.html';
  _CARIK_INTENT_START = 'CarikStart';
  _CARIK_INTENT_STOP = 'CarikStop';
  _CARIK_CONFIG_PATH = 'carik/path';
  _CARIK_RECORDING = 'recording';
  _CARIK_COUNT = 'count';

  _CARIK_MSG_START = 'Ok, saya mulai mencatat ...';
  _CARIK_MSG_CANNOT_START = 'Maaf, sepertinya saya tidak bisa mencatat diskusi ini';

  _CARIK_HTML_STYLE = '<style>span.message {padding:0 0 0 5px;}</style>';
  _CARIK_HTML_USERNAME = '<span class="username">%s, %s</span>';
  _CARIK_HTML_MESSAGE = '<br><span class="message">%s</span>';
  _CARIK_HTML_PHOTO = '<img src="%s">';

{ TCarikController }

function TCarikController.getIsRecording: boolean;
begin
  Result := False;
  if FData.ReadString(FGroupName, _CARIK_RECORDING, '0') = '1' then
    Result := True;
end;

function TCarikController.SaveToFile(Text: string): boolean;
var
  i: integer;
  fileName: string;
begin
  i := FData.ReadInteger(FGroupName, _CARIK_COUNT, 0);
  fileName := FPath + FGroupName + '_' + i2s(i) + _CARI_FILE_EXTENSION;

  AssignFile(DataFile, fileName);
  { $I+}
  try
    if not FileExists(fileName) then
      Rewrite(DataFile)
    else
      Append(DataFile);
    WriteLn(DataFile, Text);
    CloseFile(DataFile);
  except
  end;
end;

procedure TCarikController.setGroupName(AValue: string);
begin
  if FGroupName = AValue then
    Exit;
  FGroupName := AValue;
  FGroupName := StringReplace(FGroupName, ' ', '', [rfReplaceAll]);
end;

procedure TCarikController.setPath(AValue: string);
begin
  if FPath = AValue then
    Exit;
  FPath := AValue;
  FReady := DirectoryIsWritable(AValue);
end;

constructor TCarikController.Create;
var
  fileData: string;
begin
  FReady := False;
  Path := Config[_CARIK_CONFIG_PATH];
  if Path = '' then
    Path := _CARIK_PATH_DEFAULT;

  fileData := FPath + _CARIK_DATA_FILE;
  FData := TIniFile.Create(fileData);
end;

destructor TCarikController.Destroy;
begin
  FData.Free;
end;

function TCarikController.CarikHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if IntentName = _CARIK_INTENT_STOP then
    Stop;
  if IntentName = _CARIK_INTENT_START then
  begin
    if Start then
    begin
      Result := _CARIK_MSG_START;
    end
    else
    begin
      Result := _CARIK_MSG_CANNOT_START;
    end;
  end;
end;

function TCarikController.Start: boolean;
var
  i: integer;
  fileName: string;
begin
  Result := False;
  if not FReady then
    Exit;

  i := FData.ReadInteger(FGroupName, _CARIK_COUNT, 0) + 1;
  FData.WriteString(FGroupName, _CARIK_RECORDING, '1');
  FData.WriteInteger(FGroupName, _CARIK_COUNT, i);

  fileName := FPath + FGroupName + '_' + i2s(i) + _CARI_FILE_EXTENSION;
  if not FileExists(fileName) then
  begin
    SaveToFile(_CARIK_HTML_STYLE + '<h1>' + FGroupName + '</h1><table>');
  end;

  Result := True;
end;

function TCarikController.Stop: boolean;
begin
  Result := False;
  FData.WriteString(FGroupName, _CARIK_RECORDING, '0');
end;

procedure TCarikController.RecordTelegramMessage(Message: string);
var
  s: string;
  i: integer;
  html: TStringList;
  jsonData: TJSONData;
  userName, msg, Caption, photo: string;
begin
  html := TStringList.Create;
  try
    jsonData := GetJSON(Message);
  except
  end;
  userName := jsonData.GetPath('message.from.username').AsString;

  try
    msg := jsonData.GetPath('message.text').AsString;
  except
  end;

  try
    photo := jsonData.GetPath('message.photo[0].file_id').AsString;
    Caption := jsonData.GetPath('message.caption').AsString;
  except
  end;

  msg := msg + Caption;
  if photo <> '' then
  begin
    photo := format(_CARIK_HTML_PHOTO, [photo]);
    msg := photo + #13#10'<br>' + msg;
  end;

  html.Add('<tr>');
  html.Add('<td>&nbsp;</td>');
  html.Add('<td>');
  s := FormatDateTime('d-mm-y H:n:s', now);
  html.Add(Format(_CARIK_HTML_USERNAME, [userName, s]));

  html.Add(Format(_CARIK_HTML_MESSAGE, [msg]));

  html.Add('<td>');
  html.Add('</tr>');

  SaveToFile(html.Text);
  html.Free;
end;

end.
