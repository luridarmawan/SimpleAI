unit simpleai_lib;

{$mode objfpc}{$H+}

interface

uses
  entities_lib, intents_lib,
  Classes, SysUtils;

type

  { TSimpleAILib }

  TSimpleAILib = class
  private
    FIntents: TIntentsFAI;
    FisLoaded: boolean;
    function getAction: string;
    function getIntentName: string;
    function getParameters: TStrings;
    function getParameterValue(KeyName: string): string;
    function getPattern: string;
  public
    constructor Create; virtual;
    destructor Destroy; virtual;

    function Exec(Text: string): boolean;
    function LoadData: boolean;

    procedure Clear;
    function AddDataIntentFromFile(FileName: string): boolean;
    function AddDataEntitiesFromFile(FileName: string): boolean;

    property Values[KeyName: string]: string read getParameterValue; default;
  published
    property Intent: TIntentsFAI read FIntents;
    property Action: string read getAction;
    property IntentName: string read getIntentName;
    property Parameters: TStrings read getParameters;
    property Pattern: string read getPattern;
    property isLoaded: boolean read FisLoaded;

  end;


implementation

{ TSimpleAILib }


function TSimpleAILib.getIntentName: string;
begin
  Result := FIntents.IntentName;
end;

function TSimpleAILib.getAction: string;
begin
  Result := FIntents.Action;
end;

function TSimpleAILib.getParameters: TStrings;
begin
  Result := FIntents.Parameters;
end;

function TSimpleAILib.getParameterValue(KeyName: string): string;
begin
  Result := FIntents.Parameters.Values[KeyName];
end;

function TSimpleAILib.getPattern: string;
begin
  Result := FIntents.PatternString;
end;

constructor TSimpleAILib.Create;
begin
  FisLoaded := False;
  FIntents := TIntentsFAI.Create;
end;

destructor TSimpleAILib.Destroy;
begin
  FIntents.Free;
end;

function TSimpleAILib.Exec(Text: string): boolean;
begin
  Result := FIntents.Exec(LowerCase(Trim(Text)));
end;

function TSimpleAILib.LoadData: boolean;
begin
  FisLoaded := FIntents.LoadData;

  Result := FisLoaded;
end;

procedure TSimpleAILib.Clear;
begin
  FIntents.Clear;

end;

function TSimpleAILib.AddDataIntentFromFile(FileName: string): boolean;
begin
  Result := FIntents.AddDataIntentFromFile(FileName);
end;

function TSimpleAILib.AddDataEntitiesFromFile(FileName: string): boolean;
begin
  Result := FIntents.AddDataEntitiesFromFile(FileName);
end;



end.


