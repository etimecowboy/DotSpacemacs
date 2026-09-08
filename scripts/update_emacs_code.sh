#!/bin/bash
cd ~/src/emacs
git checkout src/image.c
git fetch --all
git pull
git apply ~/src/DotSpacemacs/scripts/image.c.patch
