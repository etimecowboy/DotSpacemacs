#!/bin/bash
cd ~/src/emacs
export CC="gcc-14"
# export CC=/usr/bin/gcc-14 && export CXX=/usr/bin/gcc-14

./configure --with-native-compilation=aot --with-tree-sitter --with-cairo-xcb --with-mailutils --with-pop
# --with-pgtk

cd ~/src/emacs
make -j 16
