## HTTP request inspector

The crossplatform desktop tool for inspecting http requests.
The application allows you to send http requests GET, POST, PUT, etc and
analyse its responses.

### Usage

Keyboard:

* `F9` submits a request.
* `F2` saves a request content to a file.
* `Control-N` starts a new request.
* `Control-L` moves focus to the url text field.
* `Control-P` moves focus to the request methods dropdown list.
* `Control-E` collapses or expands tree node and its children recursively (
  in case of json response only).
* `Control-I` inserts a request header and value from the predefined headers
  list.
* `Control-Q` exits application.

### Compile from sources

For compiling from sources you need the following requirements:
- Lazarus 1.8
- FreePascal 3.0.4

Before compiling the application you need to install a components package
from the `components/rest_explorer` folder. In Lazarus open in main
menu `Package / Open package file (.lpk)...` and open the file
`components/rest_explorer/rest_explorer.lpk` then compile and install the
package.

On Linux you can compile and create the install package (deb) by Makefile.
For example:
- `make linux32` for compiling release binary for i386
- `make linux64` or for amd64
- `make deb-linux32` creates a deb package for i386

For more targets see output of `make`.

### Notes for Windows

The HTTPS requests require the OpenSSL DLLs to be on the system. These DLLs
you can get from https://indy.fulgan.com/SSL/ or from "install/windows"
directory. Put these DLLs along compiled exe binary.

Setup distributions already has these OpenSSL DLLs.
