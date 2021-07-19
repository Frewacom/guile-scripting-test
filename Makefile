CC=gcc
DEPS:=$(shell guile-config compile)
FLAGS:=$(shell guile-config link)

install: main.c
	${CC} main.c -o main ${DEPS} ${FLAGS}
