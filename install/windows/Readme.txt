HOW TO BUILD RELEASE FOR WINDOWS
================================

Install Lazarus IDE 1.8.0 (amd64) and cross compiler for i386 from:
https://sourceforge.net/projects/lazarus/files/Lazarus%20Windows%2064%20bits/Lazarus%201.8.0/

Install Inno Setup (5.5.9):
http://www.jrsoftware.org/isdl.php#stable

Build releases (assuming that Lazarus installed as C:\lazarus and Inno Setup installed on disk C):
```
cd install\windows
build-release.bat 64
build-release.bat 32
```
These commands will compile releases and generate installable distributions.
Setups will be in the root project "dist" directory.

TODO:
-----
- expose application version to the install.iss (avoid manual editing)
