#!/bin/sh

# Script is for creating Debian package.
#
# ./create-deb.sh [arch]
# Where arch is one of "amd64", "x86_64", "i386", "arm".
# Argument arch is optional, current host architecture will be
# used for package creation.

if [ $# = 0 ]; then
   ARCH=$(arch)
else
   ARCH=$1
fi
case $ARCH in
     x86_64|amd64)
          ARCH=amd64
          bin=x86_64
          ;;
     i386)
          ARCH=i386
          bin=i386
          ;;
     arm)
          ARCH=armhf
          bin=arm
          ;;
     *)
          echo "Unsupported architecture $ARCH"
          exit
esac

if [ ! -f ../../bin/${bin}-linux/http-inspector ]; then
    echo "Please, build the project."
    exit 1
fi

. ../../source/version.inc
TMP=$(mktemp -d)
DEB=http-inspector_${APP_VER}_${ARCH}.deb

mkdir -p $TMP/usr/bin
mkdir -p $TMP/usr/share/applications
mkdir -p $TMP/usr/share/pixmaps
mkdir -p $TMP/DEBIAN

cp ../../resources/http-inspector.desktop $TMP/usr/share/applications
cp ../../resources/icons/http-inspector.png $TMP/usr/share/pixmaps
cp ../../bin/${bin}-linux/http-inspector $TMP/usr/bin

SIZE=$(du -ks $TMP | awk '{ print $1 }')
[ -z "$SIZE" ] && { echo "Error. Cannot get size of $TMP"; exit; }

cat control |               \
    sed "s/@size@/$SIZE/" | \
    sed "s/@arch@/$ARCH/" | \
    sed "s/@version@/$APP_VER/" > $TMP/DEBIAN/control

dpkg-deb --build $TMP ../../dist/$DEB

rm -rf $TMP
