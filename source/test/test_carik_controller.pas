unit test_carik_controller;
{
  Test TCarikController: IsCommand & ExecCommand (command "file:").

  ExecCommand membaca dokumen grup dari files/carik/docs/<grup>/<file>.
  Nama file berasal dari teks respons dan nama grup dari judul chat, keduanya
  bisa dikendalikan user, jadi path traversal harus ditolak.

  Setiap test berjalan di direktori temp sendiri (path controller relatif
  terhadap direktori kerja) dengan susunan:
    files/carik/docs/grp/note.txt      dokumen sah
    files/carik/docs/grp/.hidden       file tersembunyi
    files/carik/secret.txt             di luar direktori dokumen
    secret.txt                         di root direktori kerja
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, carik_controller;

type

  { TCarikControllerCommandTest }

  TCarikControllerCommandTest = class(TTestCase)
  private
    FOldDir: string;
    FRoot: string;
    FCarik: TCarikController;
    function Exec(const AGroup, ACommand: string): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // IsCommand
    procedure TestIsCommandWithArgument;
    procedure TestIsCommandWithoutArgument;
    procedure TestIsCommandUnknown;
    procedure TestIsCommandEmpty;
    // ExecCommand: dokumen sah
    procedure TestOpenFileInGroupDocs;
    procedure TestOpenFileStripsNewlineMarker;
    procedure TestOpenFileConvertsLineBreaks;
    procedure TestOpenFileMissingReturnsEmpty;
    procedure TestUnknownCommandReturnsEmpty;
    // ExecCommand: path traversal & input tidak valid
    procedure TestRejectParentTraversal;
    procedure TestRejectBackslashTraversal;
    procedure TestRejectSubdirectory;
    procedure TestRejectAbsolutePath;
    procedure TestRejectHiddenFile;
    procedure TestRejectMissingFileName;
    procedure TestRejectGroupNameTraversal;
    procedure TestRejectGroupNameDotDot;
  end;

implementation

procedure WriteTextFile(const AFileName, AText: string);
var
  lst: TStringList;
begin
  ForceDirectories(ExtractFilePath(AFileName));
  lst := TStringList.Create;
  try
    lst.Text := AText;
    lst.SaveToFile(AFileName);
  finally
    lst.Free;
  end;
end;

procedure RemoveTree(const ADir: string);
var
  rec: TSearchRec;
  path: string;
begin
  path := IncludeTrailingPathDelimiter(ADir);
  if FindFirst(path + '*', faAnyFile, rec) = 0 then
  begin
    repeat
      if (rec.Name = '.') or (rec.Name = '..') then
        Continue;
      if (rec.Attr and faDirectory) <> 0 then
        RemoveTree(path + rec.Name)
      else
        DeleteFile(path + rec.Name);
    until FindNext(rec) <> 0;
    FindClose(rec);
  end;
  RemoveDir(ExcludeTrailingPathDelimiter(path));
end;

{ TCarikControllerCommandTest }

function TCarikControllerCommandTest.Exec(const AGroup, ACommand: string): string;
begin
  FCarik.GroupName := AGroup;
  Result := FCarik.ExecCommand(ACommand);
end;

procedure TCarikControllerCommandTest.SetUp;
begin
  FOldDir := GetCurrentDir;
  FRoot := IncludeTrailingPathDelimiter(GetTempDir) + 'simpleai_carik_test_' +
    IntToStr(GetProcessID) + PathDelim;
  RemoveTree(FRoot);
  ForceDirectories(FRoot);

  WriteTextFile(FRoot + 'files/carik/docs/grp/note.txt', 'isi notulen');
  WriteTextFile(FRoot + 'files/carik/docs/grp/multi.txt', 'baris1' + LineEnding + 'baris2');
  WriteTextFile(FRoot + 'files/carik/docs/grp/.hidden', 'TERSEMBUNYI');
  WriteTextFile(FRoot + 'files/carik/docs/grp/sub/inner.txt', 'DI SUBFOLDER');
  WriteTextFile(FRoot + 'files/carik/secret.txt', 'RAHASIA-CARIK');
  WriteTextFile(FRoot + 'secret.txt', 'RAHASIA-ROOT');

  // path controller (files/carik/) relatif terhadap direktori kerja
  SetCurrentDir(FRoot);
  FCarik := TCarikController.Create;
end;

procedure TCarikControllerCommandTest.TearDown;
begin
  FreeAndNil(FCarik);
  SetCurrentDir(FOldDir);
  RemoveTree(FRoot);
end;

procedure TCarikControllerCommandTest.TestIsCommandWithArgument;
begin
  AssertTrue(FCarik.IsCommand('file:note.txt'));
end;

procedure TCarikControllerCommandTest.TestIsCommandWithoutArgument;
begin
  AssertFalse('tanpa ":"', FCarik.IsCommand('file'));
end;

procedure TCarikControllerCommandTest.TestIsCommandUnknown;
begin
  AssertFalse(FCarik.IsCommand('foo:bar'));
  AssertFalse(FCarik.IsCommand('http://example.com'));
end;

procedure TCarikControllerCommandTest.TestIsCommandEmpty;
begin
  AssertFalse(FCarik.IsCommand(''));
end;

// openFile mengganti #13 dan #10 masing-masing dengan "\n" literal, jadi akhir
// baris berbeda per OS (CRLF -> "\n\n"); cukup cek isi diawali teks yang benar.
procedure TCarikControllerCommandTest.TestOpenFileInGroupDocs;
begin
  AssertTrue(Pos('isi notulen\n', Exec('grp', 'file:note.txt')) = 1);
end;

procedure TCarikControllerCommandTest.TestOpenFileStripsNewlineMarker;
begin
  // respons NLP sering diakhiri "\n" literal
  AssertTrue('"\n" di akhir', Pos('isi notulen', Exec('grp', 'file:note.txt\n')) = 1);
  AssertTrue('spasi', Pos('isi notulen', Exec('grp', 'file: note.txt ')) = 1);
end;

procedure TCarikControllerCommandTest.TestOpenFileConvertsLineBreaks;
var
  s: string;
begin
  s := Exec('grp', 'file:multi.txt');
  AssertTrue('baris1 di awal', Pos('baris1\n', s) = 1);
  AssertTrue('baris2 setelah \n', Pos('\nbaris2', s) > 0);
  AssertTrue('tanpa CR/LF asli', (Pos(#13, s) = 0) and (Pos(#10, s) = 0));
end;

procedure TCarikControllerCommandTest.TestOpenFileMissingReturnsEmpty;
begin
  AssertEquals('', Exec('grp', 'file:tidakada.txt'));
end;

procedure TCarikControllerCommandTest.TestUnknownCommandReturnsEmpty;
begin
  AssertEquals('', Exec('grp', 'foo:note.txt'));
end;

procedure TCarikControllerCommandTest.TestRejectParentTraversal;
begin
  AssertEquals('../secret', '', Exec('grp', 'file:../../secret.txt'));
  AssertEquals('../../../secret', '', Exec('grp', 'file:../../../../secret.txt'));
  AssertEquals('note/../../', '', Exec('grp', 'file:note.txt/../../secret.txt'));
end;

procedure TCarikControllerCommandTest.TestRejectBackslashTraversal;
begin
  AssertEquals('', Exec('grp', 'file:..\..\secret.txt'));
end;

procedure TCarikControllerCommandTest.TestRejectSubdirectory;
begin
  // hanya satu segmen nama file yang diizinkan
  AssertEquals('', Exec('grp', 'file:sub/inner.txt'));
end;

procedure TCarikControllerCommandTest.TestRejectAbsolutePath;
begin
  AssertEquals('unix', '', Exec('grp', 'file:/etc/passwd'));
  AssertEquals('root dir', '', Exec('grp', 'file:' + FRoot + 'secret.txt'));
end;

procedure TCarikControllerCommandTest.TestRejectHiddenFile;
begin
  AssertEquals('', Exec('grp', 'file:.hidden'));
end;

procedure TCarikControllerCommandTest.TestRejectMissingFileName;
begin
  AssertEquals('"file:"', '', Exec('grp', 'file:'));
  AssertEquals('"file"', '', Exec('grp', 'file'));
  AssertEquals('"file:\n"', '', Exec('grp', 'file:\n'));
end;

procedure TCarikControllerCommandTest.TestRejectGroupNameTraversal;
begin
  // judul grup dikendalikan anggota grup
  AssertEquals('../..', '', Exec('../..', 'file:secret.txt'));
  AssertEquals('..\..', '', Exec('..\..', 'file:secret.txt'));
  AssertEquals('grp/../..', '', Exec('grp/../..', 'file:secret.txt'));
end;

procedure TCarikControllerCommandTest.TestRejectGroupNameDotDot;
begin
  // docs/../secret.txt -> files/carik/secret.txt
  AssertEquals('', Exec('..', 'file:secret.txt'));
end;

initialization
  RegisterTest(TCarikControllerCommandTest);

end.
