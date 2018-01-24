
PROJECT=source/http_inspector.lpr
VER=$(shell grep APP_VER source/version.inc|grep -o "'.*'"|sed "s/'//g")
BIN=http-inspector
LINUX64_BIN=bin/x86_64-linux/$(BIN)
LINUX64_DIST_BIN=$(BIN)_$(VER)_linux64

all: clean-build linux64-release

clean-build:
	rm -rf bin build

clean-backup:
	rm -rf source/backup

linux64-release: $(LINUX64_BIN)
$(LINUX64_BIN):
	lazbuild --os=linux --cpu=x86_64 --build-mode=Release $(PROJECT)

linux64-debug:
	lazbuild --os=linux --cpu=x86_64 --build-mode=Debug $(PROJECT)

dist-linux64-bin: linux64-release
	mkdir -p dist
	rm -f dist/$(LINUX64_DIST_BIN).gz
	cp bin/x86_64-linux/$(BIN) dist/$(LINUX64_DIST_BIN)
	chmod +x dist/$(LINUX64_DIST_BIN)
	gzip dist/$(LINUX64_DIST_BIN)

dist-linux64-deb:
	( cd install/debian && ./create-deb.sh amd64 )
