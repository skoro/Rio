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

### Notes

On Windows for HTTPS requests you need the OpenSSL library. Get it
from https://indy.fulgan.com/SSL/ (i386 or x64_86 depending of your
host architecture) and unpack to the directory where the application
binary resides.
