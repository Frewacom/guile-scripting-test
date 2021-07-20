CC=gcc

PKGS = wlroots guile-3.0
CFLAGS += $(foreach p,$(PKGS),$(shell pkg-config --cflags $(p)))
LDLIBS += $(foreach p,$(PKGS),$(shell pkg-config --libs $(p)))

install: main.c
	${CC} $^ ${CFLAGS} ${LDLIBS} -DWLR_USE_UNSTABLE -o main
