#!/bin/sh
guix environment --ad-hoc pkg-config libxkbcommon wlroots guile -- make install CC=gcc
