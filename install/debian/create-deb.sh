#!/bin/sh

# Script for creating Debian package.

( cd ../../ && make linux64-release )
[ $? != 0 ] && exit

TMP=$(mktemp -d)
VER=$(grep APP_VER ../../source/version.inc|grep -o "'.*'"|sed "s/'//g")
DEB=http-inspector_${VER}_amd64.deb

mkdir -p $TMP/usr/bin
mkdir -p $TMP/usr/share/applications
mkdir -p $TMP/usr/share/pixmaps
mkdir -p $TMP/DEBIAN

cp control $TMP/DEBIAN
cp http-inspector.desktop $TMP/usr/share/applications
cp http-inspector.png $TMP/usr/share/pixmaps
cp ../../bin/x86_64-linux/http-inspector $TMP/usr/bin

dpkg-deb --build $TMP ../../dist/$DEB

rm -rf $TMP
