#!/bin/bash
cd ~/src/emacs
export CC="gcc-14"
# export CC=/usr/bin/gcc-14 && export CXX=/usr/bin/gcc-14

git clean -xfd
./autogen.sh all
./configure --with-native-compilation=aot --with-json --with-tree-sitter --with-cairo-xcb --with-mailutils --with-pop --with-modules --with-x-toolkit=gtk3 --with-modules
# --with-pgtk

cd ~/src/emacs
make -j 16
