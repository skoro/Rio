
PROJECT=source/http_inspector.lpr

all: clean-build linux-release

clean-build:
	rm -rf bin build

clean-backup:
	rm -rf source/backup

linux-release:
	lazbuild --os=linux --build-mode=Release $(PROJECT)

linux-debug:
	lazbuild --os=linux --build-mode=Debug $(PROJECT)
