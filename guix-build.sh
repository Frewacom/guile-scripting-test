#!/bin/sh
guix environment --ad-hoc pkg-config libxkbcommon wlroots guile -- make clean parser guile CC=gcc
