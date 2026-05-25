#!/bin/bash
cd ~/src/emacs
sudo make install

update-desktop-database

systemctl --user daemon-reload
