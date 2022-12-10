#!/bin/bash
sudo apt install -y python-is-python3

pip install --upgrade --no-input --user "python-lsp-server[all]"

pip install --upgrade --user "ptvsd"

pip install --upgrade --user flake8

pip install --upgrade --user pytest

pip install --upgrade --user black

pip install --upgrade --user autoflake

pip install --upgrade --user importmagic epc
