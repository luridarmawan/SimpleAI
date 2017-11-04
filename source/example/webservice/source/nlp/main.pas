{
This file is part of the SimpleAI package.
(c) Luri Darmawan <luri@carik.id>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit main;

{$mode objfpc}{$H+}

interface

uses
  simpleai_controller,
  Classes, SysUtils, fpcgi, HTTPDefs, fastplaz_handler, database_lib;

const
  BOTNAME = 'YourBotName';
  NLP_FILE_ENTITIES = 'files/nlp/ai-entities.txt';
  NLP_FILE_INTENTS = 'files/nlp/ai-intents.txt';
  NLP_FILE_RESPONSES = 'files/nlp/ai-response.txt';

  ERR_DEFAULT = 9;

type

  { TMainModule }

  TMainModule = class(TMyCustomWebModule)
  private
    procedure beforeRequestHandler(Sender: TObject; ARequest: TRequest);
  public
    NLP: TSimpleAI;
    constructor CreateNew(AOwner: TComponent; CreateMode: integer); override;
    destructor Destroy; override;

    procedure Get; override;
    procedure Post; override;
  end;

implementation

uses common;

constructor TMainModule.CreateNew(AOwner: TComponent; CreateMode: integer);
begin
  inherited CreateNew(AOwner, CreateMode);
  BeforeRequest := @BeforeRequestHandler;
  NLP := TSimpleAI.Create;
  //NLP.Debug := True;
  NLP.AIName := BOTNAME;

  NLP.AddEntitiesFromFile(NLP_FILE_ENTITIES);
  NLP.AddIntentFromFile(NLP_FILE_INTENTS);
  NLP.AddResponFromFile(NLP_FILE_RESPONSES);

end;

destructor TMainModule.Destroy;
begin
  NLP.Free;
  inherited Destroy;
end;

// Init First
procedure TMainModule.beforeRequestHandler(Sender: TObject; ARequest: TRequest);
begin
  Response.ContentType := 'application/json';
end;

// GET Method Handler
// how to access:
//   http://youraddress/?text=hai apa kabar?
//
// more better if you use POST method
procedure TMainModule.Get;
var
  Text, authstring: string;
begin
  authstring := Header['Authorization'];
  //TODO: you can add more security check from here

  Response.Content := '{"code" : "' + i2s(ERR_DEFAULT) + '"}';
  Text := _GET['text'];
  if Text = '' then
    Exit;

  NLP.Stemming := False;
  if not NLP.Exec(Text) then // if sentence not found in pattern
  begin
    //using default reponse message

    //save to DB/Log

  end;

  Response.Content := NLP.ResponseJson;
end;

// POST Method Handler
procedure TMainModule.Post;
begin
  Response.Content := '{}';
end;



end.



