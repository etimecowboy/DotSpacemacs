#!/bin/bash
cd ~/src/emacs
sudo make uninstall

make distclean
git clean -f
rm $(find . -type f -name "*.elc")
rm $(find . -type f -name "*.eln")
