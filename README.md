## HTTP request inspector

The desktop tool for inspecting http requests. The application allows you
to send http requests GET, POST, PUT, etc and analyse its responses.

### Usage

Keyboard:

* `F9` submits a request.
* `Control-L` moves focus to the url text field.
* `Control-P` moves focus to the request methods dropdown list.
* `Control-E` collapses or expands tree node and its children recursively (
  in case of json response only).
* `Control-I` inserts a request header and value from the predefined headers
  list.
* `Control-Q` exits application.

### Compile from sources

Requirements:
- Lazarus 1.8
- FreePascal 3.0.4

### Notes for Windows

The HTTPS requests require the OpenSSL DLLs to be on the system. These DLLs
you can get from https://indy.fulgan.com/SSL/ or from "install/windows"
directory. Put these DLLs along compiled exe binary.

Setup distributions already has these OpenSSL DLLs.
