{
This file is part of the SimpleBOT package.
(c) Luri Darmawan <luri@fastplaz.com>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit suggestion_controller;

{
  USAGE:
  send message with format:

  (suggest|saran) @answer : @question
  (suggest|saran) @answer: @question

}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TBotSuggestion }

  TBotSuggestion = class
  private
    FSuggestListFile: TextFile;
    FFileName: string;
    procedure setFileName(AValue: string);
    procedure AddString(Text: string);
  public
    constructor Create; virtual;
    destructor Destroy; virtual;
    function SuggestionHandler(const IntentName: string; Params: TStrings): string;
    property FileName: string read FFileName write setFileName;

  end;

implementation

{ TBotSuggestion }

procedure TBotSuggestion.setFileName(AValue: string);
begin
  if FFileName = AValue then
    Exit;
  FFileName := AValue;
end;

procedure TBotSuggestion.AddString(Text: string);
var
  s: string;
begin

end;

constructor TBotSuggestion.Create;
begin

end;

destructor TBotSuggestion.Destroy;
begin

end;

function TBotSuggestion.SuggestionHandler(const IntentName: string;
  Params: TStrings): string;
var
  s: string;
begin
  s := trim(Params.Values['question_value']) + '=' +
    trim(Params.Values['answer_value']);
  AddString(s);

  try
    AssignFile(FSuggestListFile, FileName);
    { $I+}
    if not FileExists(FileName) then
      Rewrite(FSuggestListFile)
    else
      Append(FSuggestListFile);
    //WriteLn(FSuggestListFile, FormatDateTime('YYYY-mm-dd hh:nn:ss', now) + ' | ' + s);
    WriteLn(FSuggestListFile, s);
    CloseFile(FSuggestListFile);
  except
    on E: Exception do
    begin
      s := E.Message;
    end;
  end;

  Result := '';
end;

end.
