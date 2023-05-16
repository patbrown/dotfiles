#!/bin/sh

install() {
    rm $HOME/.zshrc
    stow -t $HOME .
}

install
