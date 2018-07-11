
PROJECT=source/http_inspector.lpr
VER=$(shell grep APP_VER source/version.inc|grep -o "'.*'"|sed "s/'//g")
LAZBUILD?=lazbuild
APP=http-inspector

all:
	@echo ""
	@echo "Select target:"
	@echo ""
	@echo "  linux32          Compile for Linux i386"
	@echo "  linux32-debug    Compile for Linux i386 with debug info"
	@echo "  linux64          Compile for Linux amd64"
	@echo "  linux64-debug    Compile for Linux amd64 with debug info"
	@echo "  linuxarm         Compile for Linux ARM"
	@echo "  linuxarm-debug   Compile for Linux ARM with debug info"
	@echo "  linux-all        Compile for all the targets"
	@echo "  deb-linux32      Create a debian package for i386"
	@echo "  deb-linux64      Create a debian package for amd64"
	@echo "  deb-linuxarm     Create a debian package for ARM"
	@echo "  deb-all          Create debian package for all the archs"
	@echo "  bin-linux32      Create a binary app archive for i386"
	@echo "  bin-linux64      Create a binary app archive for amd64"
	@echo "  bin-linuxarm     Create a binary app archive for ARM"
	@echo "  bin-all          Create binary app archives for all the archs"
	@echo ""

clean: clean-build

clean-build:
	rm -rf bin build

clean-backup:
	rm -rf source/backup

.PHONY: linux32 linux64-debug deb-linux32 bin-linux32 linux-arch-32 \
	linux64 linux64-debug deb-linux64 bin-linux64 linux-arch-64 \
	linuxarm linuxarm-debug deb-linuxarm bin-linuxarm linux-arch-arm \
	os-linux release-mode debug-mode bin build \
	deb-package linux-all deb-all bin-all

linux-all:
	make linux32
	make linux64
	make linuxarm
deb-all:
	make deb-linux32
	make deb-linux64
	make deb-linuxarm
bin-all:
	make bin-linux32
	make bin-linux64
	make bin-linuxarm

linux32: linux-arch-32 release-mode build
linux32-debug: linux-arch-32 debug-mode build
deb-linux32: linux32 deb-package
bin-linux32: linux32 bin-package

linux64: linux-arch-64 release-mode build
linux64-debug: linux-arch-64 debug-mode build
deb-linux64: linux64 deb-package
bin-linux64: linux64 bin-package

linuxarm: linux-arch-arm release-mode build
linuxarm-debug: linux-arch-arm debug-mode build
deb-linuxarm: linuxarm deb-package
bin-linuxarm: linuxarm bin-package

os-linux:
	$(eval OS=linux)
linux-arch-32: os-linux
	$(eval CPU=i386)
linux-arch-64: os-linux
	$(eval CPU=x86_64)
linux-arch-arm: os-linux
	$(eval CPU=arm)
release-mode:
	$(eval BUILD_MODE=Release)
debug-mode:
	$(eval BUILD_MODE=Debug)
bin:
	$(eval BIN=./bin/$(CPU)-$(OS)/$(APP))
build: bin $(BIN)
	$(LAZBUILD) --os=$(OS) --cpu=$(CPU) --build-mode=$(BUILD_MODE) $(PROJECT)

deb-package:
	( cd ./install/debian && ./create-deb.sh $(CPU) )

bin-package:
	mkdir -p ./dist
	$(eval DIST=./dist/$(APP)_$(VER)-$(OS)-$(CPU)-bin)
	rm -f $(DIST)
	cp ./bin/$(CPU)-$(OS)/$(APP) $(DIST)
	chmod +x $(DIST)
	gzip $(DIST)
