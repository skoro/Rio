HOW TO BUILD RELEASE FOR WINDOWS
================================

Install Lazarus IDE 1.8.0 (amd64) and cross compiler for i386 from:
https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Lazarus%201.8.0/

Install Inno Setup (5.5.9):
http://www.jrsoftware.org/isdl.php#stable

Build releases (assuming that Lazarus installed as C:\lazarus):
```
cd install\windows
build-release.bat 64
build-release.bat 32
```

Open Inno Setup and compile the following files:
install32.iss and install64.iss

Setups will be in the root project "dist" directory.

TODO:
-----
- one install.iss for both platforms
- expose application version to the install.iss (avoid manual editing)
