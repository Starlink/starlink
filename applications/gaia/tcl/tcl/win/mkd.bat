@echo off
rem SCCS: @(#) mkd.bat 1.2 98/07/01 18:08:46

if exist %1 goto end

if %OS% == Windows_NT goto winnt

echo Add support for Win 95 please
goto end

goto success

:winnt
md %1
if errorlevel 1 goto end

:success
echo created directory %1

:end
