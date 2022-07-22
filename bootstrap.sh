#!/bin/sh
rm .bashrc .bash_profile
rm -r .vim
rm -r .config/nvim
git init
git remote add origin git@github.com:renzmann/renzmann
git pull
git checkout main
git submodule update --init --recursive
