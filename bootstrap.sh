#!/bin/sh
[ -d .vim ] && rm -r .vim
[ -d .config/nvim ] && rm -r .config/nvim
[ -d .emacs.d ] && rm -r .emacs.d
git init
git remote add origin git@github.com:renzmann/renzmann
git pull
git checkout --force main
git submodule update --init --recursive
