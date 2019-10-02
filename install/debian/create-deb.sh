#!/bin/sh

# Script is for creating Debian package.
#
# ./create-deb.sh [arch] [widget]
# Where "arch" is one of "amd64", "x86_64", "i386", "arm"
# and "widget" is "gtk" or "qt5".
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

WIDGET=$2
case $WIDGET in
     gtk)
         depends="libgtk2.0-0 (>= 2.20.0)"
         ;;
     qt5)
         depends="libqt5pas1 (>= 2.6.0)"
         ;;
     *)
         echo "Widget set must be one of: gtk, qt5"
         exit
esac

if [ ! -f ../../bin/${bin}-linux/rio ]; then
    echo "Please, build the project."
    exit 1
fi

. ../../source/version.inc
TMP=$(mktemp -d)
DEB=rio_${APP_VER}_${ARCH}_${WIDGET}.deb

mkdir -p $TMP/usr/bin
mkdir -p $TMP/usr/share/applications
mkdir -p $TMP/usr/share/pixmaps
mkdir -p $TMP/DEBIAN

cp ../../resources/rio.desktop $TMP/usr/share/applications
cp ../../resources/icons/rio.png $TMP/usr/share/pixmaps
cp ../../bin/${bin}-linux/rio $TMP/usr/bin

SIZE=$(du -ks $TMP | awk '{ print $1 }')
[ -z "$SIZE" ] && { echo "Error. Cannot get size of $TMP"; exit; }

cat control |               \
    sed "s/@size@/$SIZE/" | \
    sed "s/@arch@/$ARCH/" | \
    sed "s/@depends@/$depends/" | \
    sed "s/@version@/$APP_VER/" > $TMP/DEBIAN/control

dpkg-deb --build $TMP ../../dist/$DEB

rm -rf $TMP
