unit entities_lib;

{$mode objfpc}{$H+}

interface

uses
  IniFiles, RegExpr,
  Classes, SysUtils;

const
  _SIMPLEAI_ENTITIES_DATA_FILENAME = 'files/entities.txt';
  _SIMPLEAI_ENTITIES_SEPARATOR = '|';

type

  { TEntitiesFAI }

  TEntitiesFAI = class
  private
    FAction: string;
    FData: TMemIniFile;
    FDataAsList: TStringList;
    FEntityName: string;
    FisLoaded: boolean;
    FKey: string;
    FValue: string;

  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    procedure Clear;
    function AddDataEntitiesFromFile(FileName: string): boolean;
    function LoadDataFromFile(FileName: string =
      _SIMPLEAI_ENTITIES_DATA_FILENAME): boolean;

    function GetPregString(EntityName: string; SpecificKey: string = ''): string;

    function GetKey(const EntityName: string; Text: string): string;
    function Exists(Text: string): boolean;

    // regex
    function preg_match(const RegexExpression: string; SourceString: string): boolean;
    function preg_replace(const RegexExpression, ReplaceString, SourceString: string;
      UseSubstitution: boolean): string;

    function SetData( List: TStrings): boolean;

  published
    property Data: TMemIniFile read FData write FData;
    property Action: string read FAction;
    property EntityName: string read FEntityName;
    property Key: string read FKey;
    property Value: string read FValue;
    property isLoaded: boolean read FisLoaded;
  end;

implementation

{ TEntitiesFAI }

function TEntitiesFAI.preg_match(const RegexExpression: string;
  SourceString: string): boolean;
begin
  Result := False;
  try
    with TRegExpr.Create do
    begin
      Expression := RegexExpression;
      Result := Exec(SourceString);
      Free;
    end;
  except
  end;
end;

function TEntitiesFAI.preg_replace(
  const RegexExpression, ReplaceString, SourceString: string;
  UseSubstitution: boolean): string;
begin
  try
    with TRegExpr.Create do
    begin
      Expression := RegexExpression;
      Result := Replace(SourceString, ReplaceString, UseSubstitution);
      Free;
    end;
  except
    Result := SourceString;
  end;
end;

constructor TEntitiesFAI.Create;
begin
  FEntityName := '';
  FKey := '';
  FValue := '';
  FAction := '';
  FisLoaded := False;
  FDataAsList := TStringList.Create;

end;

destructor TEntitiesFAI.Destroy;
begin
  FDataAsList.Free;

  if Assigned(FData) then
    FData.Free;
end;

procedure TEntitiesFAI.Clear;
begin
  FData.Clear;
end;

function TEntitiesFAI.AddDataEntitiesFromFile(FileName: string): boolean;
var
  lst: TStrings;
begin
  Result := False;
  if not FileExists(FileName) then
    exit;
  ;

  if Assigned(FData) then
    FData.Free;
  FData := TMemIniFile.Create('');
  FData.Clear;

  lst := TStringList.Create;
  with TStringList.Create do
  begin
    LoadFromFile(FileName);
    FDataAsList.Add(Text);

    lst.Text := FDataAsList.Text;
    FData.SetStrings(lst);

    FisLoaded := True;
    Result := True;
    Free;
  end;
  lst.Free;
end;

function TEntitiesFAI.SetData(List: TStrings): boolean;
begin
  Result := False;
  if not Assigned( List) then
    Exit;

  if Assigned(FData) then
    FData.Free;
  FData := TMemIniFile.Create('');
  FData.Clear;

  FDataAsList.Add( List.Text);
  FData.SetStrings(List);

  FisLoaded := True;
  Result := True;
end;


function TEntitiesFAI.LoadDataFromFile(FileName: string): boolean;
begin
  Result := False;
  if not FileExists(FileName) then
    Exit;

  if Assigned(FData) then
    FData.Free;
  FData := TMemIniFile.Create(FileName);

  FisLoaded := True;
  Result := True;
end;

function TEntitiesFAI.GetPregString(EntityName: string; SpecificKey: string): string;
var
  i: integer;
  str: TStrings;
begin
  Result := '';
  if not FisLoaded then
    Exit;

  if SpecificKey <> '' then
  begin
    if Pos('-',SpecificKey) = -1 then
    begin
      Result := '(' + FData.ReadString(EntityName, SpecificKey, '') + ')';
      Exit;
    end;
  end;

  str := TStringList.Create;
  FData.ReadSection(EntityName, str);

  Result := '';
  for i := 0 to str.Count - 1 do
  begin
    if ( ('-'+str[i]) <> SpecificKey) then
    begin
      Result := Result + FData.ReadString(EntityName, str[i], '');
      if i < str.Count - 1 then
        Result := Result + _SIMPLEAI_ENTITIES_SEPARATOR;
    end;
  end;
  if Result = '' then
    Result := '.*';
  Result := '(' + Result + ')';

  str.Free;
end;

function TEntitiesFAI.GetKey(const EntityName: string; Text: string): string;
var
  i: integer;
  pattern: string;
  str: TStrings;
begin
  Result := '';
  FAction := '';
  if (EntityName = '') or (Text = '') then
    Exit;

  Text := LowerCase(Text);
  str := TStringList.Create;

  FData.ReadSection(EntityName, str);
  for i := 0 to str.Count - 1 do
  begin
    pattern := '^(' + FData.ReadString(EntityName, str[i], '') + ')\Z';
    if preg_match(pattern, Text) then
    begin
      Result := str[i];
      str.Free;
      Exit;
    end;
  end;

  str.Free;
end;

function TEntitiesFAI.Exists(Text: string): boolean;
var
  i: integer;
  s, entity_name, pattern: string;
  str: TStrings;
begin
  Result := False;
  if Text = '' then
    Exit;
  FEntityName := '';
  FKey := '';
  Text := LowerCase(Text);
  str := TStringList.Create;

  FData.ReadSections(str);
  for i := 0 to str.Count - 1 do
  begin
    entity_name := str[i];
    s := GetKey(entity_name, Text);
    if s <> '' then
    begin
      FKey := s;
      FEntityName := entity_name;
      str.Free;
      Result := True;
      Exit;
    end;
  end;
  str.Free;
end;


end.
