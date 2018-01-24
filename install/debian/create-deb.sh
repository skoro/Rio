#!/bin/sh

# Script for creating Debian package.
#
# ./create-deb.sh [arch]
# Where arch is one of "amd64", "x86_64", "i386".
# Argument arch is optional, current host architecture will be
# used for package creation.

if [ $# = 0 ]; then
   ARCH=$(arch)
else
   ARCH=$1
fi
case $ARCH in
     x86_64|amd64)
          target=64
          ARCH=amd64
          ;;
     i386)
          target=32
          ;;
     *)
          echo "Unsupported architecture $ARCH"
          exit
esac

( cd ../../ && make linux${target}-release ) || exit

TMP=$(mktemp -d)
VER=$(grep APP_VER ../../source/version.inc|grep -o "'.*'"|sed "s/'//g")
DEB=http-inspector_${VER}_amd64.deb

mkdir -p $TMP/usr/bin
mkdir -p $TMP/usr/share/applications
mkdir -p $TMP/usr/share/pixmaps
mkdir -p $TMP/DEBIAN

sed "s/@arch@/$ARCH/" control > $TMP/DEBIAN/control
cp ../resources/http-inspector.desktop $TMP/usr/share/applications
cp ../resources/http-inspector.png $TMP/usr/share/pixmaps
cp ../../bin/x86_64-linux/http-inspector $TMP/usr/bin

dpkg-deb --build $TMP ../../dist/$DEB

rm -rf $TMP
