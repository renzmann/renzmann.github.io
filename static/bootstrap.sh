#!/bin/sh
[ -d .vim ] && rm -r .vim
[ -d .config/nvim ] && rm -r .config/nvim
git init
git remote add origin git@github.com:renzmann/renzmann
git pull
git checkout --force main
git submodule update --init --recursive
