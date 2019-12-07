## Rio

Rio is a cross-platform REST client, available for Windows and Linux.

### Screenshots

Main window

![Main window](https://raw.githubusercontent.com/skoro/rio/master/docs/screenshots/Screenshot_0.1-xfce.png)

### Compile from sources

For compiling from sources you need the following tools:
- [Lazarus IDE](http://www.lazarus-ide.org/) 1.8
- [FreePascal compiler](https://www.freepascal.org/) 3.0.4
- **Qt5Pas** for Qt5 widget set, can be installed by `sudo apt install libqt5pas1`
  on Ubuntu

Before compiling the application you need to install a components package
`components/rest_explorer`. In Lazarus from the main menu
`Package / Open package file (.lpk)...` open the file
`components/rest_explorer/rest_explorer.lpk` compile it and install the
package.
Please ensure that the package *TAChart* is also installed.

On Linux you can compile and create the install package (deb) by Makefile.
For example:
- `make linux32` for compiling a release binary for i386
- `make linux64` or for amd64
- `make linux64-qt` for compiling a release with Qt5 widget set
- `make deb-linux32` creates a deb package for i386
- `make appimage` creates AppImage binary (only amd64)

For more targets see output of `make`.

Refer to `install/windows/Readme.txt` for information how to compile on
Windows and to create Windows binary installers.

### Notes for Windows

The HTTPS requests require the OpenSSL DLLs to be on the system. These DLLs
you can get from https://indy.fulgan.com/SSL/ or from "install/windows"
directory. Put these DLLs along compiled exe binary.

Setup distributions already have these OpenSSL DLLs.
