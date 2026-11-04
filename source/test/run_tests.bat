@echo off
rem Compile and run the SimpleAI unit tests with instantfpc.
rem Usage: run_tests.bat [runner options]   (default: -a)
rem
rem FastPlaz location: set FASTPLAZ_DIR to the FastPlaz folder (the one that
rem contains src\ and test\). Default: ..\..\..\..\..\FastPlaz\git\fastplaz\FastPlaz

setlocal
cd /d "%~dp0"

where instantfpc >nul 2>nul
if errorlevel 1 set "PATH=C:\lazarus\fpc\3.2.2\bin\x86_64-win64;%PATH%"

if not defined FASTPLAZ_DIR set "FASTPLAZ_DIR=%~dp0..\..\..\..\..\FastPlaz\git\fastplaz\FastPlaz"
if not exist "%FASTPLAZ_DIR%\src\library\common.pas" (
  echo FastPlaz not found: %FASTPLAZ_DIR%
  echo Set FASTPLAZ_DIR to the FastPlaz folder.
  exit /b 2
)
set "FP=%FASTPLAZ_DIR%"
set "SA=%~dp0..\package"

rem instantfpc does not accept -Fu paths on its command line, pass them here.
rem -B: always rebuild, the cache only tracks the main program source.
set "INSTANTFPCOPTIONS=-B -Fi%FP% -Fu%FP%\test -Fu%FP%\..\tools\src -Fu%FP%\src\systems -Fu%FP%\src\library -Fu%FP%\src\vendor -Fu%FP%\src\model -Fu%FP%\src\util -Fu%FP%\src\helper -Fu%FP%\src\integration -Fu%FP%\src\vendor\dcrypt -Fu%SA% -Fu%SA%\src -Fu%SA%\src\library -Fu%SA%\src\controller"

echo Compiling (instantfpc)...
if "%~1"=="" (
  instantfpc simpleai_test.lpr -a
) else (
  instantfpc simpleai_test.lpr %*
)
exit /b %errorlevel%
