unit app;

{$mode objfpc}{$H+}

interface

uses
  fastplaz_handler,
  simpleai_controller, fpjson, jsonparser, jsonscanner,
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterJScript, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, ComCtrls;

const
  BOTNAME = 'TESTER';
  FILE_ENTITIES = '*-entities.txt';
  FILE_INTENTS = '*-intents.txt';
  FILE_RESPONSES = '*-response.txt';

type

  { TLocalJSONParser }

  TLocalJSONParser = class(TJSONParser)
  private
    FScanner: TJSONScanner;
  public
    property Scanner: TJSONScanner read FScanner;
  end;

  { TfApp }

  TfApp = class(TForm)
    btn_Send: TButton;
    editor: TSynEdit;
    edt_Dir: TDirectoryEdit;
    edt: TEdit;
    Label1: TLabel;
    Page: TPageControl;
    Panel1: TPanel;
    pnl_Top: TPanel;
    pnl_Bottom: TPanel;
    pnl: TPanel;
    SynJScriptSyn1: TSynJScriptSyn;
    page_Result: TTabSheet;
    procedure btn_SendClick(Sender: TObject);
    procedure edtKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edt_DirAcceptDirectory(Sender: TObject; var Value: string);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  private
    FIsLoaded: boolean;
    NLP: TSimpleAI;
    function loadData: boolean;
    function loadDataEntities: boolean;
    function loadDataIntents: boolean;
    function loadDataResponses: boolean;
  public
  end;

var
  fApp: TfApp;

implementation

{$R *.lfm}

{ TfApp }

procedure TfApp.FormCreate(Sender: TObject);
begin
  editor.Clear;
  FIsLoaded := False;
  edt_Dir.Directory := ExtractFileDir(Application.ExeName);

  //demo
  edt.Text := 'hi';
end;

procedure TfApp.FormDestroy(Sender: TObject);
begin
  if Assigned(NLP) then
    NLP.Free;
end;

function jsonFormatter(JsonString: string): string;
  // error line : VJSONParser.Scanner.CurRow;
var
  VJSONData: TJSONData = nil;
  VJSONParser: TLocalJSONParser;
begin
  Result := '';
  JsonString := trim(JsonString);
  if JsonString = '' then
    Exit;

  VJSONParser := TLocalJSONParser.Create(JsonString);
  try
    try
      VJSONParser.Strict := True;
      VJSONData := VJSONParser.Parse;
      Result := VJSONData.FormatJSON([], 2);
      VJSONData.Free;
    except
      on E: Exception do
      begin
      end;
    end;
  finally
    VJSONParser.Free;
  end;
end;

procedure TfApp.btn_SendClick(Sender: TObject);
begin
  if Trim(edt.Text) = '' then
    Exit;

  NLP := TSimpleAI.Create;
  NLP.AIName := BOTNAME;
  NLP.Stemming := False;
  NLP.Debug := False;
  if not loadData then
  begin
    editor.Lines.Add(#13'Data can not loaded');
    exit;
  end;

  if not NLP.Exec(edt.Text) then
  begin
    // if text not exists in nlp data

  end;
  editor.Lines.Text := jsonFormatter(NLP.ResponseJson);

  if not FIsLoaded then
    editor.Lines.Add(#13'Data still not loaded');
  edt.Clear;
  NLP.Free;
end;

procedure TfApp.edtKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 13 then
  begin
    key := 0;
    btn_Send.Click;
  end;
end;

procedure TfApp.edt_DirAcceptDirectory(Sender: TObject; var Value: string);
begin
  loadData;
end;

procedure TfApp.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TfApp.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    Close;
  end;
end;

function TfApp.loadData: boolean;
begin
  FIsLoaded := False;
  if (loadDataEntities and loadDataIntents and loadDataResponses) then
    FIsLoaded := True;

  Result := FIsLoaded;
end;

function TfApp.loadDataEntities: boolean;
var
  fileInfo: TSearchRec;
begin
  Result := False;
  SetCurrentDir(edt_Dir.Directory);
  if FindFirst(FILE_ENTITIES, faAnyFile, fileInfo) = 0 then
  begin
    repeat
      NLP.AddEntitiesFromFile(fileInfo.Name);
    until FindNext(fileInfo) <> 0;
    editor.Lines.Add('Entites loaded');
    Result := True;
  end;
end;

function TfApp.loadDataIntents: boolean;
var
  fileInfo: TSearchRec;
begin
  Result := False;
  SetCurrentDir(edt_Dir.Directory);
  if FindFirst(FILE_INTENTS, faAnyFile, fileInfo) = 0 then
  begin
    repeat
      NLP.AddIntentFromFile(fileInfo.Name);
    until FindNext(fileInfo) <> 0;
    editor.Lines.Add('Intents loaded');
    Result := True;
  end;
end;

function TfApp.loadDataResponses: boolean;
var
  fileInfo: TSearchRec;
begin
  Result := False;
  SetCurrentDir(edt_Dir.Directory);
  if FindFirst(FILE_RESPONSES, faAnyFile, fileInfo) = 0 then
  begin
    repeat
      NLP.AddResponFromFile(fileInfo.Name);
    until FindNext(fileInfo) <> 0;
    editor.Lines.Add('Responses loaded');
    Result := True;
  end;
end;

end.

