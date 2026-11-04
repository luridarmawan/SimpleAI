program simpleai_test;
{
  SimpleAI / SimpleBOT unit tests.

  Jalankan tanpa compile manual:
    Windows : run_tests.bat
    Linux   : ./run_tests.sh

  Butuh FastPlaz (env FASTPLAZ_DIR, default ../../../../../FastPlaz/git/fastplaz/FastPlaz).
  Runner (fastplaz_testrunner) dipakai dari folder test FastPlaz.
}

{$mode objfpc}{$H+}

uses
  {$IFNDEF Windows}cthreads,{$ENDIF}
  Classes, consoletestrunner, fastplaz_testrunner, test_carik_controller;

type

  { TMyTestRunner }

  TMyTestRunner = class(TFastPlazTestRunner)
  protected
  end;

var
  App: TMyTestRunner;

begin
  DefaultFormat := fPlain;
  DefaultRunAllTests := True;
  App := TMyTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'SimpleAI - Unit Test';
  App.Run;
  App.Free;
end.
