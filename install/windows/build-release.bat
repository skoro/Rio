
@echo off
set LAZBUILD=c:\lazarus\lazbuild
set PROJECT=..\..\source\http_inspector.lpi

set ARCH=%1

if "%ARCH%" == "" (
	echo Usage:
	echo for building release for i386: %0 32
	echo for building release for amd64: %0 64
)
if "%ARCH%" == "32" (
	%LAZBUILD% --build-mode="Release Win32" %PROJECT%
	"C:\Program Files (x86)\Inno Setup 5\iscc" /DWIN32=1 /O+ install.iss
)
if "%ARCH%" == "64" (
	%LAZBUILD% --build-mode="Release" %PROJECT%
	"C:\Program Files (x86)\Inno Setup 5\iscc" /DWIN64=1 /O+ install.iss
)
