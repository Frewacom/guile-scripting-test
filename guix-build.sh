#!/bin/sh
guix environment --ad-hoc pkg-config wlroots guile -- make install CC=gcc
