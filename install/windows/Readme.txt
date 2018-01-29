HOW TO BUILD RELEASE FOR WINDOWS
================================

Install Lazarus IDE 1.8.0 (amd64) and cross compiler for i386 from:
https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Lazarus%201.8.0/

Install Inno Setup (5.5.9):
http://www.jrsoftware.org/isdl.php#stable

Build releases:
cd install\windows
build-release.bat 64
build-release.bat 32

Open Inno Setup and compile files: install32.iss and install64.iss
Setups will be in the root project "dist" directory.

