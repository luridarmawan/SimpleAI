{
This file is part of the SimpleAI package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit intents_lib;

{$mode objfpc}{$H+}

interface

uses
  common, logutil_lib,
  entities_lib,
  RegExpr, IniFiles, Classes, SysUtils;

const
  _SIMPLEAI_INTENT_DATA_FILENAME = 'files/intents.txt';
  _SIMPLEAI_INTENT_ACTIONKEY = 'action';
  _SIMPLEAI_OBJECT = 'object';
  _SIMPLEAI_CONTEXT = 'context';
  _SIMPLEAI_VARIABLE = 'var';
  _SIMPLEAI_BOUNDARY = 'boundary';
  _AI_VARKEY = 'varkey';

type

  { TIntentsFAI }

  TIntentsFAI = class
  private
    FAction: string;
    FBoundary: boolean;
    FContext: string;
    FData: TMemIniFile;
    FDataAsList: TStringList;
    FDebug: boolean;
    FIntentKey: string;
    FIntentKeySpecific: string;
    FIntentName: string;
    FEntities: TEntitiesFAI;
    FisLoaded: boolean;
    FObjectName: string;
    FParameters: TStringList;
    FPattern: string;

  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    function LoadData: boolean;
    function LoadDataFromFile(FileName: string =
      _SIMPLEAI_INTENT_DATA_FILENAME): boolean;

    function Exec(Text: string): boolean;
    function Explode(Str, Delimiter: string): TStrings;

    procedure Clear;
    function AddDataIntentFromFile(FileName: string): boolean;
    function AddDataEntitiesFromFile(FileName: string): boolean;
    function SetData(List: TStrings): boolean;
  published
    property Data: TMemIniFile read FData write FData;
    property Entities: TEntitiesFAI read FEntities;
    property Action: string read FAction;
    property Context: string read FContext;
    property ObjectName: string read FObjectName;
    property IntentName: string read FIntentName;
    property IntentKey: string read FIntentKey;
    property IntentKeySpecific: string read FIntentKeySpecific;
    property Entity: TEntitiesFAI read FEntities;

    property isLoaded: boolean read FisLoaded;
    property isBoundary: boolean read FBoundary write FBoundary;
    property Parameters: TStringList read FParameters;
    property PatternString: string read FPattern;
    property Debug: boolean read FDebug write FDebug;
  end;


implementation

{ TIntentsFAI }

function TIntentsFAI.Explode(Str, Delimiter: string): TStrings;
var
  i: integer;
  p: string;
begin
  Result := TStringList.Create;
  while Pos(Delimiter, Str) <> 0 do
  begin
    p := '';
    for i := 1 to Pos(Delimiter, Str) - 1 do
      p := p + Str[i];
    Result.Add(p);
    //Delete(s,1,Pos(Delimiter,Str));
    Delete(Str, 1, Pos(Delimiter, Str) + Length(Delimiter) - 1);
  end;
  //result.Add(s);
  if (Length(Str) <> 0) then
    Result.Add(Str);
end;

constructor TIntentsFAI.Create;
begin
  FisLoaded := False;
  FEntities := TEntitiesFAI.Create;
  FParameters := TStringList.Create;
  FDataAsList := TStringList.Create;

  FAction := '';
  FContext := '';
  FObjectName := '';
  FIntentName := '';
  FIntentKey := '';
  FDebug := False;
  FBoundary := True;
end;

destructor TIntentsFAI.Destroy;
begin
  if Assigned(FData) then
    FData.Free;

  FDataAsList.Free;
  FParameters.Free;
  FEntities.Free;
end;

function TIntentsFAI.Exec(Text: string): boolean;
var
  i, j, k, v, p, match_index: integer;
  Source, s, pattern, entity_name, intent_name, key_used, section_name: string;
  intent_list, item_list, pattern_str: TStrings;
  varsIndex: TStringList;
  tmp: TStrings;
  regex: TRegExpr;
begin
  Result := False;
  FIntentName := '';
  FIntentKey := '';
  FParameters.Clear;
  FIntentKeySpecific := '';
  Text := Trim(Text);
  if Text = '' then
    Exit;
  if not Assigned(FData) then
    Exit;

  intent_list := TStringList.Create;
  item_list := TStringList.Create;
  pattern_str := TStringList.Create;
  varsIndex := TStringList.Create;
  tmp := TStringList.Create;
  FData.ReadSections(intent_list);
  for i := 0 to intent_list.Count - 1 do
  begin
    intent_name := intent_list[i];
    FData.ReadSectionRaw(intent_name, item_list);

    FBoundary := True;
    for j := 0 to item_list.Count - 1 do
    begin
      tmp := Explode(item_list[j], '=');
      if tmp[0] = _SIMPLEAI_INTENT_ACTIONKEY then
        Continue;
      if tmp[0] = _SIMPLEAI_OBJECT then
        Continue;
      if tmp[0] = _SIMPLEAI_BOUNDARY then
      begin
        if tmp[1] = 'false' then
          FBoundary := False;
        Continue;
      end;
      if tmp[0] = _SIMPLEAI_VARIABLE then
        Continue;
      pattern := tmp[1];

      // if there are '=' in text
      if tmp.Count > 2 then
      begin
        pattern := '';
        for k := 1 to tmp.Count - 1 do
        begin
          if k = 1 then
            pattern := tmp[k]
          else
            pattern := pattern + '=' + tmp[k];
        end;
      end;

      FPattern := pattern;
      pattern_str := Explode(pattern, ' ');
      for k := pattern_str.Count - 1 downto 0 do
      begin
        s := pattern_str[k];
        p := pos('@', s);
        if p > 0 then
        begin
          key_used := '';
          entity_name := Copy(s, p);
          entity_name := StringReplace(entity_name, '@', '', [rfReplaceAll]);
          p := Pos(':', entity_name);
          if p > 0 then
          begin
            key_used := copy(entity_name, p + 1);
            entity_name := copy(entity_name, 0, p - 1);
          end;
          s := FEntities.GetPregString(entity_name, key_used);

          FParameters.Insert(0, entity_name + '=');
          if key_used <> '' then
            entity_name := entity_name + ':' + key_used;
          pattern := StringReplace(pattern, '@' + entity_name, s, [rfReplaceAll]);
        end;
        if k = 0 then
          Break;
      end;

      if FBoundary then
        if length(Text) > 1 then
          pattern := pattern + '\b';

      try
        regex := TRegExpr.Create;
        regex.Expression := pattern;
        if regex.Exec(Text) then
        begin
          FIntentName := intent_name;
          FAction := FData.ReadString(FIntentName, _SIMPLEAI_INTENT_ACTIONKEY, '');
          FContext := FData.ReadString(FIntentName, _SIMPLEAI_CONTEXT, '');
          FObjectName := FData.ReadString(FIntentName, _SIMPLEAI_OBJECT, '');

          key_used := '';
          match_index := 1;
          varsIndex.Clear;
          repeat
            if FParameters.Count > 0 then
              section_name := FParameters.Names[match_index - 1];
            s := regex.Match[match_index];
            key_used := FEntities.GetKey(section_name, s);
            if key_used = '' then
              key_used := s;
            if FParameters.Count > 0 then
              FParameters.Values[section_name] := key_used;

            //FParameters.Values[ '_'+section_name + '_value'] := regex.Match[ match_index];
            if trim(section_name) <> '' then
              FParameters.Values[section_name + '_value'] := regex.Match[match_index];
            varsIndex.Values['$' + i2s(match_index)] := regex.Match[match_index];
            Inc(match_index);
          until regex.Match[match_index] = '';
          if varsIndex.Count > 0 then
          begin
            for v := 0 to varsIndex.Count - 1 do
            begin
              FParameters.Values[varsIndex.Names[v]] := varsIndex.ValueFromIndex[v];
            end;
          end;
          FParameters.Sort;

          if FDebug then
            FParameters.Values['pattern'] := pattern;

          FIntentKey := tmp[0];
          k := pos(':', FIntentKey);
          if k > 0 then
          begin
            FIntentKeySpecific := copy(FIntentKey, k + 1);
            FParameters.Add(_AI_VARKEY + '=' + FIntentKeySpecific);
          end;

          regex.Free;
          tmp.Free;
          pattern_str.Free;
          item_list.Free;
          intent_list.Free;
          Result := True;
          Exit;
        end
        else
        begin
          FPattern := '';
        end;

      except
      end;

      FParameters.Clear;
      regex.Free;
    end;

  end;

  tmp.Free;
  varsIndex.Free;
  pattern_str.Free;
  item_list.Free;
  intent_list.Free;
end;

procedure TIntentsFAI.Clear;
begin
  FData.Clear;
  FEntities.Clear;
end;

function TIntentsFAI.AddDataIntentFromFile(FileName: string): boolean;
var
  lst: TStrings;
begin
  Result := False;
  if not FileExists(FileName) then
    exit;

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

function TIntentsFAI.AddDataEntitiesFromFile(FileName: string): boolean;
begin
  Result := FEntities.AddDataEntitiesFromFile(FileName);
end;

function TIntentsFAI.SetData(List: TStrings): boolean;
begin
  Result := False;
  if not Assigned(List) then
    Exit;

  if Assigned(FData) then
    FData.Free;
  FData := TMemIniFile.Create('');
  FData.Clear;

  FDataAsList.Add(List.Text);
  FData.SetStrings(List);

  FisLoaded := True;
  Result := True;
end;

function TIntentsFAI.LoadData: boolean;
begin
  FisLoaded := False;
  FEntities.LoadDataFromFile;
  LoadDataFromFile();

  Result := True;
end;

function TIntentsFAI.LoadDataFromFile(FileName: string): boolean;
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

end.



