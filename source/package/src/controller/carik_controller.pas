{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit carik_controller;

{
  Chat Recorder


  Carik := TCarikController.Create;
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
  common, fastplaz_handler, telegram_integration, logutil_lib, mailer_lib,
  simpleai_controller, http_lib, json_lib,
  {$if FPC_FULlVERSION >= 30200}
  opensslsockets, fpopenssl,
  {$endif}
  IniFiles, fpjson, Classes, SysUtils, string_helpers;

const
  _GROUP_MEMBERBARU_ABAIKAN = 'group_memberbaru_abaikan';

  _NOTULEN_MSG_START = 'Ok, saya mulai mencatat ... ✍ ...';
  _NOTULEN_MSG_STOP = '... catetan sudah dihentikan.';
  _NOTULEN_MSG_RECORDNUMBER = '\nini notulen ke %d';
  _NOTULEN_MSG_CANNOT_START = 'Maaf, sepertinya saya tidak bisa mencatat diskusi ini';
  _NOTULEN_MSG_NOTPERMITTED = 'Sepertinya anda belum masuk sebagai admin';
  _NOTULEN_MSG_SECRET = 'rahasia ...';
  _NOTULEN_MSG_NORECORDING = 'saat ini tidak ada yang dicatat di sini.';
  _NOTULEN_MSG_RECORDING = 'Sekarang sedang mencatat diskusi:';
  _NOTULEN_MSG_TERKIRIM = '🎁 terkirim ... ';
  _NOTULEN_MSG_NOTFOUND = 'Catatan tidak tersedia';

type

  { TCarikController }

  TCarikController = class
  private
    DataFile: TextFile;
    FFullName: string;
    FGroupChatID: string;
    FInvitedFullName: string;
    FInvitedUserId: string;
    FInvitedUserName: string;
    FIsGroup: boolean;
    FIsInvitation: boolean;
    FUserID: string;
    FUserPrefix: string;
    Telegram: TTelegramIntegration;
    FGroupName: string;
    FGroupNameOri: string;
    FPath: string;
    FReady: boolean;
    FData: TIniFile;
    FGroupData: TIniFile;
    FRecordNumber: integer;
    FUserName: string;
    function getCustomMessage(const KeyName: string): string;
    function getisCollectiveWelcomeGreeting: boolean;
    function getIsPermiited: boolean;
    function getIsRecording: boolean;
    function getisSpamChecking: boolean;
    function SaveToFile(Text: string): boolean;
    function SaveToFileCSV(Text: string): boolean;
    function getDirPath(IndexRecording: integer): string;
    function getSafeGroupName(AGroupChatID: string): string;
    function downloadFile(FileID: string): string;
    procedure setCustomMessage(const KeyName: string; AValue: string);
    procedure setGroupName(AValue: string);
    procedure setPath(AValue: string);
    function openFile(FileName: string): string;
    function isValidCommand(CommandString: string): boolean;
    function getGroupInfo(GroupNameID: string; ADetail: boolean = False): string;
    function getGroupAdminList(GroupNameID: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    function StartHandler(const IntentName: string; Params: TStrings): string;
    function StopHandler(const IntentName: string; Params: TStrings): string;
    function CheckHandler(const IntentName: string; Params: TStrings): string;
    function TopicHandler(const IntentName: string; Params: TStrings): string;
    function SendHandler(const IntentName: string; Params: TStrings): string;
    function GroupInfoHandler(const IntentName: string; Params: TStrings): string;
    function isSapaMemberBaru: boolean;
    function MemberBaruAbaikanHandler(const IntentName: string;
      Params: TStrings): string;
    function MemberBaruSapaHandler(const IntentName: string; Params: TStrings): string;
    function Start: boolean;
    function Stop: boolean;
    function Send: boolean;
    procedure RecordTelegramMessage(Message: string);
    function EnableBot: boolean;
    function DisableBot: boolean;
    function IsDisabled: boolean;
    function IsImageRecognitionDisabled: boolean;
    procedure ImageRecognitionCounting;

    property Data: TIniFile read FData write FData;
    property Ready: boolean read FReady;
    property Path: string read FPath write setPath;
    property UserID: string read FUserID write FUserID;
    property UserPrefix: string read FUserPrefix write FUserPrefix;
    property UserName: string read FUserName write FUserName;
    property FullName: string read FFullName write FFullName;
    property GroupName: string read FGroupName write setGroupName;
    property GroupChatID: string read FGroupChatID write FGroupChatID;
    property InvitedUserId: string read FInvitedUserId write FInvitedUserId;
    property InvitedUserName: string read FInvitedUserName write FInvitedUserName;
    property InvitedFullName: string read FInvitedFullName write FInvitedFullName;
    property IsRecording: boolean read getIsRecording;
    property RecordNumber: integer read FRecordNumber;
    property IsGroup: boolean read FIsGroup write FIsGroup;
    property IsInvitation: boolean read FIsInvitation write FIsInvitation;
    property IsPermitted: boolean read getIsPermiited;
    property isCollectiveWelcomeGreeting: boolean read getisCollectiveWelcomeGreeting;
    property isSpamChecking: boolean read getisSpamChecking;
    property CustomMessage[const KeyName: string]: string
      read getCustomMessage write setCustomMessage;

    function IsCommand(Text: string): boolean;
    function ExecCommand(Text: string): string;

    function AdminAdd(ValidUserName: string): boolean;
    function AdminDel(ValidUserName: string): boolean;
    function GroupInfo: string;
    procedure Invited;
  published
    property GroupData: TIniFile read FGroupData;
  end;


implementation

const
  _NOTULEN_CONFIG_GROUPS = 'carik/groups/';
  _NOTULEN_CONFIG_PATH = 'carik/path';
  _NOTULEN_PATH_DEFAULT = 'files/carik/';
  _NOTULEN_PATH_DOCS = 'docs/%s/';
  _NOTULEN_DATA_FILE = 'carik.dat';
  _NOTULEN_GROUP_DATA_FILE = 'carik-group.dat';
  _NOTULEN_FILE_EXTENSION = '.html';
  _NOTULEN_NAME = 'name';
  _NOTULEN_DISABLE = 'disable';
  _NOTULEN_RECORDING = 'recording';
  _NOTULEN_COUNT = 'count';
  _NOTULEN_TOPIC = 'topic';
  _NOTULEN_INVITEDBY_ID = 'invitedby_id';
  _NOTULEN_INVITEDBY_NAME = 'invitedby_name';
  _NOTULEN_INVITEDBY_USERNAME = 'invitedby_username';
  _NOTULEN_INVITEDBY_DATE = 'invitedby_date';
  _NOTULEN_GROUP_ID = 'id';
  //_NOTULEN_DIR_PREFIX = 'group-';
  _NOTULEN_ADMIN_PREFIX = 'admin-';
  _NOTULEN_IMAGERECOGNITION_COUNTING = 'image_recognition';
  _NOTULEN_IMAGERECOGNITION_DISABLED = 'image_recognition_disabled';

  _NOTULEN_SECTION_GROUP_LIST = 'GroupList';

  _NOTULEN_HTML_STYLE =
    '<style>body{font-family:Tahoma,"Lucida Grande","Trebuchet MS"}span.username{border-bottom:1px solid #c2d1f0;font-size:small;display:block;background:#e6f5ff;padding:2px 2px 2px 5px}span.message{padding:0 0 0 10px}table{min-width:300px}table,td,th{border:1px solid #00134d}td{border:0;border-bottom:1px solid #668cff}</style>' + '<style>span {white-space: pre;font-family: monospace;display: block;unicode-bidi: embed}</style>';
  _NOTULEN_HTML_USERNAME = '<span class="username">%s, %s</span>';
  _NOTULEN_HTML_MESSAGE = '<span class="message">%s</span>';
  //_NOTULEN_HTML_DOCUMENT = '<a href="%s">document</a>';
  _NOTULEN_HTML_PHOTO = '<img src="%s">';
  _NOTULEN_HTML_VIDEO = '<video controls><source src="%s" type="video/mp4"></video>';

  _NOTULEN_SUPERADMIN = 'luridarmawan';
  GROUPINFO_URL = 'services/groupinfo_url';

//_NOTULEN_MIME_VIDEO = 'video/mp4';

{ TCarikController }

function TCarikController.getIsRecording: boolean;
begin
  Result := False;
  if FGroupData.ReadString(FGroupName, _NOTULEN_RECORDING, '0') = '1' then
    Result := True;
end;

//TODO: fix this function !!!!
function TCarikController.getisSpamChecking: boolean;
var
  i: Integer;
  separator, serviceUrl: string;
  http_response: IHTTPResponse;
  json: TJSONUtil;
begin
  Result := False;
  serviceUrl := Config[GROUPINFO_URL];
  separator := '?';
  if serviceUrl.IsExists('?') then separator:= '&';
  if serviceUrl.IsEmpty then
    Exit;
  serviceUrl := serviceUrl + separator + 'channel=telegram&id=' + FGroupChatID;
  with THTTPLib.Create do
  begin
    AllowRedirect := True;
    URL := serviceUrl;
    http_response := Get;
    if http_response.ResultCode <> 200 then
    begin
      Free;
      Exit;
    end;
    if http_response.ResultText.IsEmpty then
    begin
      Free;
      Exit;
    end;
    Free;
  end;
  json := TJSONUtil.Create;
  try
    json.LoadFromJsonString(http_response.ResultText, False);
  except
  end;
  i := s2i(json['code']);
  if i <> 0 then
  begin
    json.Free;
    Exit;
  end;
  Result := s2b(json['data/options/spam_checking']);
  json.Free;
end;

function TCarikController.getIsPermiited: boolean;
var
  s, _admin: string;
begin
  Result := False;
  s := 'carik/groups/' + FGroupName + '/admin';
  _admin := Config[s];
  if _admin = '' then
    _admin := _NOTULEN_SUPERADMIN;

  if pos(FUserName, _admin) > 0 then
    Result := True;

  if FGroupData.ReadInteger(FGroupName, _NOTULEN_ADMIN_PREFIX + FUserName, 0) > 0 then
    Result := True;
end;

function TCarikController.SaveToFile(Text: string): boolean;
var
  i: integer;
  fileName, dir: string;
begin
  Result := False;
  i := FGroupData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0);
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
    Result := True;
  except
  end;
end;

function TCarikController.SaveToFileCSV(Text: string): boolean;
var
  i: integer;
  fileName, dir: string;
begin
  Result := False;
  i := FGroupData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0);
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
    Result := True;
  except
  end;
end;

function TCarikController.getDirPath(IndexRecording: integer): string;
var
  _safeNAme: string;
begin
  _safeNAme := getSafeGroupName(FGroupChatID) + '-' + FGroupChatID;
  Result := FPath + 'groups' + DirectorySeparator + _safeNAme + '-' +
    i2s(IndexRecording) + DirectorySeparator;
end;

function TCarikController.getSafeGroupName(AGroupChatID: string): string;
begin
  Result := FData.ReadString(_NOTULEN_SECTION_GROUP_LIST, AGroupChatID, '');
end;

function TCarikController.downloadFile(FileID: string): string;
var
  filePath, targetFile: string;
begin
  Result := '';
  Telegram := TTelegramIntegration.Create;
  Telegram.Token := Config['telegram/default/token'];
  if Telegram.Token = '' then
    Exit;

  FRecordNumber := FGroupData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0);

  filePath := Telegram.GetFilePath(FileID);
  targetFile := getDirPath(FRecordNumber) + filePath;
  if Telegram.DownloadFile(filePath, targetFile) then
  begin
    Result := filePath;
  end;
  Telegram.Free;
end;

function TCarikController.getCustomMessage(const KeyName: string): string;
begin
  Result := FGroupData.ReadString(FGroupName, 'MSG_' + KeyName, '');
end;

function TCarikController.getisCollectiveWelcomeGreeting: boolean;
var
  i: Integer;
  serviceUrl: string;
  http_response: IHTTPResponse;
  json: TJSONUtil;
begin
  Result := False;
  serviceUrl := Config[GROUPINFO_URL];
  if serviceUrl.IsEmpty then
    Exit;
  serviceUrl := serviceUrl + '&channel=telegram&id=' + FGroupChatID;
  with THTTPLib.Create do
  begin
    AllowRedirect := True;
    URL := serviceUrl;
    http_response := Get;
    if http_response.ResultCode <> 200 then
    begin
      Free;
      Exit;
    end;
    if http_response.ResultText.IsEmpty then
    begin
      Free;
      Exit;
    end;
    Free;
  end;
  json := TJSONUtil.Create;
  json.LoadFromJsonString(http_response.ResultText, False);
  i := s2i(json['code']);
  if i <> 0 then
  begin
    json.Free;
    Exit;
  end;
  Result := s2b(json['data/options/welcome_member_collective']);
  json.Free;
end;

procedure TCarikController.setCustomMessage(const KeyName: string; AValue: string);
begin
  FGroupData.WriteString(FGroupName, 'MSG_' + KeyName, AValue);
end;

procedure TCarikController.setGroupName(AValue: string);
begin
  FGroupName := getSafeGroupName(FGroupChatID);
  if FGroupName <> '' then
    Exit;
  FGroupName := AValue;
  FGroupNameOri := AValue;
  //FGroupName:= SafeText( AValue);
  FGroupName := StringReplace(FGroupName, ' ', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, '(', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, ')', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, '@', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, '-', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, '_', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, '"', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, '''', '', [rfReplaceAll]);
  FGroupName := StringReplace(FGroupName, '&', '', [rfReplaceAll]);
end;

procedure TCarikController.setPath(AValue: string);
begin
  if FPath = AValue then
    Exit;
  FPath := AValue;
  FReady := DirectoryIsWritable(AValue);
end;

function TCarikController.openFile(FileName: string): string;
var
  _note: TStringList;
begin
  Result := '';
  FileName := Trim(FileName);
  if FileExists(FileName) then
  begin
    _note := TStringList.Create;
    try
      _note.LoadFromFile(FileName);
      Result := _note.Text;
      Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
      Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
      Result := StringReplace(Result, '===', #13, [rfReplaceAll]); // page break
    except
      on E: Exception do
      begin
      end;
    end;
    _note.Free;
  end;
end;

constructor TCarikController.Create;
begin
  FReady := False;
  FIsGroup := False;
  FIsInvitation := False;
  Path := Config[_NOTULEN_CONFIG_PATH];
  if Path = '' then
    Path := _NOTULEN_PATH_DEFAULT;

  FGroupData := TIniFile.Create(FPath + _NOTULEN_GROUP_DATA_FILE);
  FData := TIniFile.Create(FPath + _NOTULEN_DATA_FILE);
  FUserPrefix := '';
  FGroupChatID := '';
  FGroupName := '';
end;

destructor TCarikController.Destroy;
begin
  FData.Free;
  FGroupData.Free;
end;

function TCarikController.StartHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := _NOTULEN_MSG_CANNOT_START;
  if FGroupName = '' then
    Exit;

  if not IsPermitted then
    Exit;

  LogUtil.Add('starting ... ', 'carik');
  if Start then
  begin
    LogUtil.Add('--- ' + FGroupName + ' recorded', 'carik');
    Result := _NOTULEN_MSG_START + format(_NOTULEN_MSG_RECORDNUMBER, [FRecordNumber]);
  end
  else
  begin
    Result := _NOTULEN_MSG_CANNOT_START;
  end;
end;

function TCarikController.StopHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := '';
  if not IsPermitted then
    Exit;
  Stop;
  Result := '... catetan sudah dihentikan.';
end;

function TCarikController.CheckHandler(const IntentName: string;
  Params: TStrings): string;
var
  i, _recordStatus, _recordNumber: integer;
  s, _recordName, _groupName, _groupTopic: string;
  lst: TStringList;
begin
  Result := _NOTULEN_MSG_SECRET;

  if not IsPermitted then
    Exit;

  lst := TStringList.Create;
  FGroupData.ReadSections(lst);

  s := '';
  for i := 0 to lst.Count - 1 do
  begin
    _groupName := lst[i];
    if FIsGroup then
    begin
      if _groupName <> FGroupName then
        Continue;
    end;
    _recordStatus := FGroupData.ReadInteger(_groupName, _NOTULEN_RECORDING, 0);
    if _recordStatus = 1 then
    begin
      _recordName := FGroupData.ReadString(_groupName, _NOTULEN_NAME, _groupName);
      _groupTopic := FGroupData.ReadString(_groupName, _NOTULEN_TOPIC, '');
      _recordNumber := FGroupData.ReadInteger(_groupName, _NOTULEN_COUNT, 0);

      s := s + '\n- ' + _recordName + ' (#' + i2s(_recordNumber) + ')';
      if _groupTopic <> '' then
        s := s + ', topic: ' + _groupTopic;
    end;
  end;
  if s = '' then
    s := _NOTULEN_MSG_NORECORDING
  else
    s := _NOTULEN_MSG_RECORDING + s;

  Result := s;
  lst.Free;
end;

function TCarikController.TopicHandler(const IntentName: string;
  Params: TStrings): string;
var
  _topic: string;
begin
  Result := '';

  if not IsPermitted then
    Exit;

  _topic := Params.Values['topic_value'];
  _topic := StringReplace(_topic, '"', '', [rfReplaceAll]);
  FGroupData.WriteString(FGroupName, _NOTULEN_TOPIC, _topic);
  FGroupData.WriteString(FGroupName, _NOTULEN_GROUP_ID, FGroupChatID);

  if IsRecording then
  begin
    // TODO: direct save to file
  end;

  Result := 'Baik, topik saat ini *"' + ucwords(_topic) + '"*';
end;

function TCarikController.SendHandler(const IntentName: string;
  Params: TStrings): string;
var
  i: integer;
  _dir, _fileTarget: string;
begin
  Result := _NOTULEN_MSG_NOTPERMITTED;
  if FGroupName = '' then
    Exit;
  if not IsPermitted then
    Exit;

  i := FGroupData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0);
  if i = 0 then
  begin
    Result := _NOTULEN_MSG_NOTFOUND;
    Exit;
  end;

  _dir := getDirPath(i);
  if not DirectoryExists(_dir) then
  begin
    Result := _NOTULEN_MSG_NOTFOUND + ' ...';
    Exit;
  end;

  _fileTarget := Copy(_dir, 0, Length(_dir) - 1) + '.zip';
  Result := 'sepertinya gagal kirim nihh... ';
  if ZipFolder(_dir, _fileTarget) then
  begin
    Telegram := TTelegramIntegration.Create;
    Telegram.Token := Config['telegram/default/token'];
    Result := '. . .';
    if Telegram.Token <> '' then
    begin
      Result := 'Catatan ' + FGroupName;
      if FGroupData.ReadString(FGroupName, _NOTULEN_TOPIC, '') <> '' then
        Result := Result + ', Topik: ' + FGroupData.ReadString(FGroupName,
          _NOTULEN_TOPIC, '');
      Telegram.SendDocument(UserID, _fileTarget, Result);
      Result := _NOTULEN_MSG_TERKIRIM;
    end;
    Telegram.Free;
  end;

  {
  if Send then
  begin
    Result := 'terkirim ...';
  end;
  }
end;

function TCarikController.GroupInfoHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := GroupInfo;
end;

function TCarikController.isSapaMemberBaru: boolean;
begin
  Result := False;
  if FGroupData.ReadString(FGroupName, _GROUP_MEMBERBARU_ABAIKAN, '0') <> '1' then
    Result := True;
end;

function TCarikController.MemberBaruAbaikanHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := _NOTULEN_MSG_NOTPERMITTED;
  if FGroupName = '' then
    Exit;
  if not IsPermitted then
    Exit;

  FGroupData.WriteString(FGroupName, _GROUP_MEMBERBARU_ABAIKAN, '1');

  Result := 'OK';
end;

function TCarikController.MemberBaruSapaHandler(const IntentName: string;
  Params: TStrings): string;
begin
  Result := _NOTULEN_MSG_NOTPERMITTED;
  if FGroupName = '' then
    Exit;
  if not IsPermitted then
    Exit;

  FGroupData.WriteString(FGroupName, _GROUP_MEMBERBARU_ABAIKAN, '0');

  Result := 'OK';
end;


function TCarikController.Start: boolean;
var
  i: integer;
  s, fileName, dir, _topic: string;
begin
  Result := False;
  if not FReady then
    Exit;

  i := FGroupData.ReadInteger(FGroupName, _NOTULEN_RECORDING, 0);
  FRecordNumber := FGroupData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0);
  if i = 1 then
  begin
    Result := True;
    Exit;
  end;

  s := _NOTULEN_CONFIG_GROUPS + FGroupName + '/admin';
  s := Config[s];
  if s <> '' then
  begin
    if Pos(FUserName, s) = 0 then
      Exit;
  end;

  i := FGroupData.ReadInteger(FGroupName, _NOTULEN_COUNT, 0) + 1;
  FRecordNumber := i;
  FGroupData.WriteString(FGroupName, _NOTULEN_NAME, FGroupNameOri);
  FGroupData.WriteString(FGroupName, _NOTULEN_GROUP_ID, FGroupChatID);
  FGroupData.WriteString(FGroupName, _NOTULEN_RECORDING, '1');
  FGroupData.WriteInteger(FGroupName, _NOTULEN_COUNT, i);

  dir := getDirPath(i);
  try
    if not DirectoryExists(dir) then
    begin
      ForceDirectories(dir);
      ForceDirectories(dir + 'files');
      ForceDirectories(dir + 'photos');
      ForceDirectories(dir + 'documents');
    end;
  except
    on E: Exception do
    begin
    end;
  end;

  // save index.html
  fileName := dir + 'index' + _NOTULEN_FILE_EXTENSION;
  if not FileExists(fileName) then
  begin
    _topic := FGroupData.ReadString(FGroupName, _NOTULEN_TOPIC, '');
    if _topic = '' then
      _topic := '%topic%';
    s := _NOTULEN_HTML_STYLE + #13'<h1>' + FGroupName + ' #' +
      i2s(FRecordNumber) + '</h1>';
    s := s + #13'<h3>Topic: ' + _topic + '</h3>';
    s := s + #13'<b>Level: -</b>';
    s := s + #13'<br>Tanggal: ' + FormatDateTime('dd-mm-yyyy hh:nn', now) +
      ' - Generated by CarikBot';
    s := s + #13'<table>';
    SaveToFile(s);
  end;

  Result := True;
end;

function TCarikController.Stop: boolean;
begin
  Result := False;
  FGroupData.WriteString(FGroupName, _NOTULEN_RECORDING, '0');
  //FGroupData.WriteString(FGroupName, _NOTULEN_TOPIC, '');
  Result := True;
end;

function TCarikController.Send: boolean;
var
  _mail: TMailer;
begin
  Result := False;
  _mail := TMailer.Create(); // TODO: sent email

  _mail.AddTo('me', 'luri@kioss.com');
  _mail.Message.Add('test');

  // ... ...

  Result := _mail.Send;
  _mail.Free;
end;

procedure TCarikController.RecordTelegramMessage(Message: string);
var
  s: string;
  html: TStringList;
  jsonData: TJSONData;
  csv, msg, Caption, photo, _doc, _mime: string;
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

  // photo
  try
    try
      photo := jsonData.GetPath('message.photo[2].file_id').AsString;
    except
      try
        photo := jsonData.GetPath('message.photo[1].file_id').AsString;
      except
        try
          photo := jsonData.GetPath('message.photo[0].file_id').AsString;
        except
          on e: Exception do
          begin
          end;
        end;
      end;
    end;
    Caption := jsonData.GetPath('message.caption').AsString;
  except
  end;

  // document
  _mime := '';
  try
    _doc := jsonData.GetPath('message.document.file_id').AsString;
    _mime := jsonData.GetPath('message.document.mime_type').AsString;
    Caption := jsonData.GetPath('message.caption').AsString;
  except
  end;

  msg := msg + Caption;
  if photo <> '' then
  begin
    s := downloadFile(photo);
    photo := format(_NOTULEN_HTML_PHOTO, [s]);
    msg := photo + #13#10'<br>' + msg;
  end;
  if _doc <> '' then
  begin
    s := downloadFile(_doc);
    photo := format(_NOTULEN_HTML_VIDEO, [s]);
    msg := photo + #13#10'<br>' + msg;
  end;
  csv := StringReplace(msg, #13#10, #13, [rfReplaceAll]);
  csv := StringReplace(csv, #13, '\n', [rfReplaceAll]);
  csv := StringReplace(csv, '<br>', '\n', [rfReplaceAll]);

  html.Add('<tr>');
  html.Add('<td>&nbsp;</td>');
  html.Add('<td>');
  s := FormatDateTime('dd-mm-yyyy hh:nn:ss', now);
  csv := s + '|' + FFullName + '|' + csv;
  html.Add(Format(_NOTULEN_HTML_USERNAME, [FFullName, s]));

  html.Add(Format(_NOTULEN_HTML_MESSAGE, [msg]));

  html.Add('<td>');
  html.Add('</tr>');

  SaveToFile(html.Text);
  SaveToFileCSV(csv);
  html.Free;
end;

function TCarikController.EnableBot: boolean;
begin
  Result := False;
  if FGroupName = '' then
    Exit;
  if not IsPermitted then
    Exit;

  FGroupData.WriteString(FGroupName, _NOTULEN_DISABLE, '0');
  Result := True;
end;

function TCarikController.DisableBot: boolean;
begin
  Result := False;
  if FGroupName = '' then
    Exit;
  if not IsPermitted then
    Exit;

  FGroupData.WriteString(FGroupName, _NOTULEN_DISABLE, '1');
  Result := True;
end;

function TCarikController.IsDisabled: boolean;
begin
  Result := False;
  if FGroupData.ReadString(FGroupName, _NOTULEN_DISABLE, '0') = '1' then
    Result := True;
end;

function TCarikController.IsImageRecognitionDisabled: boolean;
begin
  Result := FGroupData.ReadBool(FGroupName, _NOTULEN_IMAGERECOGNITION_DISABLED, False);
end;

procedure TCarikController.ImageRecognitionCounting;
var
  i: integer;
begin
  i := FGroupData.ReadInteger(FGroupName, _NOTULEN_IMAGERECOGNITION_COUNTING, 0) + 1;
  FGroupData.WriteInteger(FGroupName, _NOTULEN_IMAGERECOGNITION_COUNTING, i);
end;

function TCarikController.isValidCommand(CommandString: string): boolean;
var
  s: string;
begin
  Result := False;
  for s in CommandList do
  begin
    if CommandString = s then
      Exit(True);
  end;
end;

function TCarikController.IsCommand(Text: string): boolean;
var
  lst: TStrings;
begin
  Result := False;
  if Text = '' then
    Exit;
  lst := Explode(Text, ':');
  if lst.Count = 1 then
    Exit;
  if isValidCommand(lst[0]) then
    Result := True;
  lst.Free;
end;

function TCarikController.ExecCommand(Text: string): string;
var
  s, _dir: string;
  lst: TStrings;

begin
  Result := '';

  _dir := _NOTULEN_PATH_DEFAULT + _NOTULEN_PATH_DOCS;
  _dir := Format(_dir, [FGroupName]);
  _dir := StringReplace(_dir, '//', '/', [rfReplaceAll]);

  lst := Explode(Text, ':');
  case lst[0] of
    _AI_CMD_OPENFILE:
    begin
      s := trim(_dir + lst[1]);
      s := StringReplace(s, '\n', '', [rfReplaceAll]);
      Result := openFile(s);
    end;
  end;

  lst.Free;
end;

function TCarikController.AdminAdd(ValidUserName: string): boolean;
begin
  Result := False;
  if FGroupName = '' then
    Exit;
  if not IsPermitted then
    Exit;
  if ValidUserName = '' then
    Exit;
  ValidUserName := StringReplace(ValidUserName, '@', '', [rfReplaceAll]);

  FGroupData.WriteInteger(FGroupName, _NOTULEN_ADMIN_PREFIX + ValidUserName, 1);
  Result := True;
end;

function TCarikController.AdminDel(ValidUserName: string): boolean;
begin
  Result := False;
  if FGroupName = '' then
    Exit;
  if not IsPermitted then
    Exit;
  if ValidUserName = '' then
    Exit;
  ValidUserName := StringReplace(ValidUserName, '@', '', [rfReplaceAll]);

  //FGroupData.DeleteKey(FGroupName, _NOTULEN_ADMIN_PREFIX + ValidUserName);
  FGroupData.WriteInteger(FGroupName, _NOTULEN_ADMIN_PREFIX + ValidUserName, 0);
  Result := True;
end;

function TCarikController.getGroupInfo(GroupNameID: string; ADetail: boolean): string;
var
  i: integer;
  s, _groupname, gid: string;
begin
  _groupname := FGroupData.ReadString(GroupNameID, 'name', '');
  gid := FGroupData.ReadString(GroupNameID, 'id', '');
  if _groupname = '' then
    _groupname := GroupNameID;

  s := '';
  if FGroupData.ReadString(GroupNameID, _NOTULEN_DISABLE, '0') = '0' then
    Result := '*' + _groupname + '*'
  else
    Result := '' + _groupname + '';

  s := '';
  if FGroupData.ReadString(GroupNameID, _NOTULEN_RECORDING, '0') = '1' then
  begin
    s := ' recording(' + FGroupData.ReadString(GroupNameID, _NOTULEN_COUNT, '0') + ')';
    if FGroupData.ReadString(GroupNameID, _NOTULEN_TOPIC, '') <> '' then
      s := s + '\n- topic: ' + FGroupData.ReadString(GroupNameID, _NOTULEN_TOPIC, '');
  end;
  Result := Result + s;

  {
  if FGroupData.ReadString(GroupNameID, _NOTULEN_IMAGERECOGNITION_COUNTING, '0') <> '0' then
    s := s + ' img(' + FGroupData.ReadString(GroupNameID,
      _NOTULEN_IMAGERECOGNITION_COUNTING, '0') + ')';
  }

  s := getGroupAdminList(GroupNameID);
  if s <> '' then
  begin
    if ADetail then
    begin
      s := ' ├ @' + StringReplace(s, ', ', #10' ├ @', [rfReplaceAll]);
      Result := Result + #10'👤 Lurah:'#10 + s;
    end
    else
      Result := Result + '\n   - lurah: ' + s;
  end;

  // detail group - admin list etc
  if ADetail then
  begin
    Telegram := TTelegramIntegration.Create;
    Telegram.Token := Config['telegram/default/token'];
    if Telegram.Token = '' then
      Exit;
    s := Telegram.GroupAdminList(gid);
    if s <> '' then
    begin
      s := ' ├ ' + StringReplace(s, ',', #10' ├ ', [rfReplaceAll]);
      Result := Result + #10'👮 Admin:'#10 + s;
    end;
    i := Telegram.GroupMemberCount(gid);
    Result := Result + #10'👥 ada ' + i2s(i) + ' anggota';
    Telegram.Free;
  end; // detail group

  //Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
end;

function TCarikController.getGroupAdminList(GroupNameID: string): string;
var
  i: integer;
  s: string;
  lst: TStrings;
begin
  Result := '';
  lst := TStringList.Create;

  FGroupData.ReadSectionValues(GroupNameID, lst);
  s := '';
  for i := 0 to lst.Count - 1 do
  begin
    if Pos('admin-', lst[i]) > 0 then
    begin
      s := copy(lst[i], 7);
      s := copy(s, 1, pos('=', s) - 1);
      Result := Result + ' ' + s;
    end;
  end;

  Result := trim(Result);
  Result := StringReplace(Result, ' ', ', ', [rfReplaceAll]);
  lst.Free;
end;


function TCarikController.GroupInfo: string;
var
  i: integer;
  lastGroup: string;
  lst, return: TStrings;
begin
  if not IsPermitted then
    Exit;
  lst := TStringList.Create;
  return := TStringList.Create;

  FGroupData.ReadSections(lst);

  lastGroup := '';
  if FGroupName = '' then
  begin
    lastGroup := '*20 Group Terakhir:* ';
    for i := lst.Count - 20 to lst.Count - 1 do
    begin
      if i < lst.Count - 1 then
        lastGroup := lastGroup + #10'├ ' + i2s(i) + '. ' + (lst[i])
      else
        lastGroup := lastGroup + #10'└ ' + i2s(i) + '. ' + (lst[i]);
    end;
  end;
  (lst as TStringList).Sort;

  for i := 0 to lst.Count - 1 do
  begin
    if FGroupName <> '' then
    begin
      if lst[i] = FGroupName then
      begin
        return.Add(getGroupInfo(lst[i], True));
      end;
    end
    else
    begin
      if i < lst.Count - 1 then
        return.Add('├ ' + i2s(i + 1) + '. ' + getGroupInfo(lst[i]))
      else
        return.Add('└ ' + i2s(i + 1) + '. ' + getGroupInfo(lst[i]));
    end;
  end;

  Result := lastGroup + ' ';
  if FGroupName <> '' then
    Result := lastGroup + #10#10'*Group Info:*'#10 + return.Text;
  Result := StringReplace(Result, #13, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '\n', [rfReplaceAll]);
  Result := StringReplace(Result, '_', '\_', [rfReplaceAll]);

  return.Free;
  lst.Free;
end;

procedure TCarikController.Invited;
begin
  FData.WriteString(_NOTULEN_SECTION_GROUP_LIST, FGroupChatID, FGroupName);

  FGroupData.WriteString(FGroupName, _NOTULEN_GROUP_ID, FGroupChatID);
  FGroupData.WriteString(FGroupName, _NOTULEN_INVITEDBY_ID, FUserID);
  FGroupData.WriteString(FGroupName, _NOTULEN_INVITEDBY_NAME, FFullName);
  FGroupData.WriteString(FGroupName, _NOTULEN_INVITEDBY_USERNAME, FUserName);
  FGroupData.WriteString(FGroupName, _NOTULEN_INVITEDBY_DATE,
    FormatDateTime('yyyy/mm/dd hh:nn', now));
  LogUtil.Add('invited to ' + FGroupChatID + ' ' + FGroupName, 'INVITATION');
end;

end.
