#!/bin/bash
sudo add-apt-repository ppa:ubuntu-toolchain-r/ppa
sudo apt update
sudo apt install gcc-14 libgccjit0 libgccjit-14-dev libjansson4 libjansson-dev libtree-sitter0 libtree-sitter-dev
sudo apt build-dep emacs
