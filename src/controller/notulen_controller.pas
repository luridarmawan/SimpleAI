{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit notulen_controller;

{
  Chat Recorder


  Carik := TNotulenController.Create;
  Carik.UserName := 'usernamethatsendmessage';
  Carik.GroupName := 'thisgroupname';
  .
  .
  Carik.Start;
  .
  .
  .
  if Carik.Recording then
    Carik.RecordTelegramMessage(Request.Content);

  .
  .
  Carik.Free;
}

{$mode objfpc}{$H+}

interface

uses
  common, fastplaz_handler, telegram_integration, logutil_lib,
  IniFiles, fpjson,
  Classes, SysUtils;

type

  { TNotulenController }

  TNotulenController = class
  private
    DataFile: TextFile;
    Telegram: TTelegramIntegration;
    FGroupName: string;
    FPath: string;
    FReady: boolean;
    FData: TIniFile;
    FRecordNumber: integer;
    FUserName: string;
    function getIsRecording: boolean;
    function SaveToFile(Text: string): boolean;
    function SaveToFileCSV(Text: string): boolean;
    function getDirPath(IndexRecording: integer): string;
    function downloadFile(FileID: string): string;
    procedure setGroupName(AValue: string);
    procedure setPath(AValue: string);
  public
    constructor Create;
    destructor Destroy;
    function StartHandler(const IntentName: string; Params: TStrings): string;
    function StopHandler(const IntentName: string; Params: TStrings): string;
    function Start: boolean;
    function Stop: boolean;
    procedure RecordTelegramMessage(Message: string);

    property Ready: boolean read FReady;
    property Path: string read FPath write setPath;
    property UserName: string read FUserName write FUserName;
    property GroupName: string read FGroupName write setGroupName;
    property Recording: boolean read getIsRecording;
    property RecordNumber: integer read FRecordNumber;
  end;


implementation

const
  _NOTULEN_CONFIG_GROUPS = 'carik/groups/';
  _NOTULEN_CONFIG_PATH = 'carik/path';
  _NOTULEN_PATH_DEFAULT = 'files/carik/';
  _NOTULEN_DATA_FILE = 'carik.dat';
  _NOTULEN_FILE_EXTENSION = '.html';
  _NOTULEN_RECORDING = 'recording';
  _NOTULEN_COUNT = 'count';
  _NOTULEN_DIR_PREFIX = 'group-';

  _NOTULEN_MSG_START = 'Ok, saya mulai mencatat ...';
  _NOTULEN_MSG_RECORDNUMBER = 'ini notulen ke %d';
  _NOTULEN_MSG_CANNOT_START = 'Maaf, sepertinya saya tidak bisa mencatat diskusi ini';

  _NOTULEN_HTML_STYLE =
    '<style>body{font-family:Tahoma,"Lucida Grande","Trebuchet MS"}span.username{border-bottom:1px solid #c2d1f0;font-size:small;display:block;background:#e6f5ff;padding:2px 2px 2px 5px}span.message{padding:0 0 0 10px}table{min-width:300px}table,td,th{border:1px solid #00134d}td{border:0;border-bottom:1px solid #668cff}img{min-width:90%}</style>';
  _NOTULEN_HTML_USERNAME = '<span class="username">%s, %s</span>';
  _NOTULEN_HTML_MESSAGE = '<span class="message">%s</span>';
  _NOTULEN_HTML_PHOTO = '<img src="%s">';

{ TNotulenController }

function TNotulenController.getIsRecording: boolean;
begin
  Result := False;
  if FData.ReadString(FGroupName, _NOTULEN_RECORDING, '0') = '1' then
    Result := True;
end;

function TNotulenController.SaveToFile(Text: string): boolean;
var
  i: integer;
  fileName, dir: string;
begin
  i := FData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0);
  dir := getDirPath(i);
  fileName := dir + 'index' + _NOTULEN_FILE_EXTENSION;

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

function TNotulenController.SaveToFileCSV(Text: string): boolean;
var
  i: integer;
  fileName, dir: string;
begin
  i := FData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0);
  dir := getDirPath(i);
  fileName := dir + LowerCase(FGroupName) + '.csv';

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

function TNotulenController.getDirPath(IndexRecording: integer): string;
begin
  Result := FPath + _NOTULEN_DIR_PREFIX + FGroupName + '-' +
    i2s(IndexRecording) + DirectorySeparator;
end;

function TNotulenController.downloadFile(FileID: string): string;
var
  filePath, targetFile: string;
begin
  Result := '';
  Telegram := TTelegramIntegration.Create;
  Telegram.Token := Config['telegram/token'];
  if Telegram.Token = '' then
    Exit;

  FRecordNumber := FData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0);

  filePath := Telegram.GetFile(FileID);
  targetFile := getDirPath(FRecordNumber) + filePath;
  if Telegram.DownloadFile(filePath, targetFile) then
  begin
    Result := filePath;
  end;
  Telegram.Free;
end;

procedure TNotulenController.setGroupName(AValue: string);
begin
  if FGroupName = AValue then
    Exit;
  //FGroupName:= SafeText( AValue);
  FGroupName := StringReplace(FGroupName, ' ', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, '(', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, ')', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, '@', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, '-', '', [rfReplaceAll]);
end;

procedure TNotulenController.setPath(AValue: string);
begin
  if FPath = AValue then
    Exit;
  FPath := AValue;
  FReady := DirectoryIsWritable(AValue);
end;

constructor TNotulenController.Create;
var
  fileData: string;
begin
  FReady := False;
  Path := Config[_NOTULEN_CONFIG_PATH];
  if Path = '' then
    Path := _NOTULEN_PATH_DEFAULT;

  fileData := FPath + _NOTULEN_DATA_FILE;
  FData := TIniFile.Create(fileData);
end;

destructor TNotulenController.Destroy;
begin
  FData.Free;
end;

function TNotulenController.StartHandler(const IntentName: string;
  Params: TStrings): string;
var
  s, admin: string;
begin
  Result := _NOTULEN_MSG_CANNOT_START;
  if FGroupName = '' then
    Exit;
  s := 'carik/groups/' + FGroupName + '/admin';
  admin := Config[s];
  if admin = '' then
    Exit;

  if pos(FUserName, admin) = 0 then
    Exit;

  if Start then
  begin
    Result := _NOTULEN_MSG_START + format(_NOTULEN_MSG_RECORDNUMBER, [FRecordNumber]);
  end
  else
  begin
    Result := _NOTULEN_MSG_CANNOT_START;
  end;
end;

function TNotulenController.StopHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  Stop;
end;

function TNotulenController.Start: boolean;
var
  i: integer;
  s, fileName, dir: string;
begin
  Result := False;
  if not FReady then
    Exit;

  i := FData.ReadInteger(FGroupName, _NOTULEN_RECORDING, 0);
  FRecordNumber := FData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0);
  if i = 1 then
  begin
    Result := True;
    Exit;
  end;

  s := _NOTULEN_CONFIG_GROUPS + FGroupName + '/admin';
  s := Config[s];
  if s <> '' then
  begin
    LogUtil.Add(FUserName + ' is in ' + s, 'notulen');
    if Pos(FUserName, s) = 0 then
      Exit;
    LogUtil.Add(FUserName + ' is permitted', 'notulen');
  end;

  i := FData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0) + 1;
  FRecordNumber := i;
  FData.WriteString(FGroupName, _NOTULEN_RECORDING, '1');
  FData.WriteInteger(FGroupName, _NOTULEN_COUNT, i);

  dir := getDirPath(i);
  try
    if not DirectoryExists(dir) then
    begin
      mkdir(dir);
      mkdir(dir + 'files');
      mkdir(dir + 'photo');
    end;
  except
  end;

  // save index.html
  fileName := dir + 'index' + _NOTULEN_FILE_EXTENSION;
  if not FileExists(fileName) then
  begin
    SaveToFile(_NOTULEN_HTML_STYLE + '<h1>' + FGroupName + ' #' +
      i2s(FRecordNumber) + '</h1><table>');
  end;

  Result := True;
end;

function TNotulenController.Stop: boolean;
begin
  Result := False;
  FData.WriteString(FGroupName, _NOTULEN_RECORDING, '0');
end;

procedure TNotulenController.RecordTelegramMessage(Message: string);
var
  s: string;
  html: TStringList;
  jsonData: TJSONData;
  csv, msg, Caption, photo: string;
begin
  html := TStringList.Create;
  try
    jsonData := GetJSON(Message);
  except
  end;

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
    //todo: getfile
    s := downloadFile(photo);
    photo := format(_NOTULEN_HTML_PHOTO, [s]);
    msg := photo + #13#10'<br>' + msg;
  end;
  csv := StringReplace(msg, #13#10, #13, [rfReplaceAll]);
  csv := StringReplace(csv, #13, '\n', [rfReplaceAll]);
  csv := StringReplace(csv, '<br>', '\n', [rfReplaceAll]);

  html.Add('<tr>');
  html.Add('<td>&nbsp;</td>');
  html.Add('<td>');
  s := FormatDateTime('d-mm-y H:n:s', now);
  csv := s + '|' + FUserName + '|' + csv;
  html.Add(Format(_NOTULEN_HTML_USERNAME, [FUserName, s]));

  html.Add(Format(_NOTULEN_HTML_MESSAGE, [msg]));

  html.Add('<td>');
  html.Add('</tr>');

  SaveToFile(html.Text);
  SaveToFileCSV(csv);
  html.Free;
end;

end.
