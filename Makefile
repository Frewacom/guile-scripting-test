CC=gcc

PKGS = wlroots guile-3.0 xkbcommon
BUILD_DIR = build
CFLAGS += $(foreach p,$(PKGS),$(shell pkg-config --cflags $(p))) -DWLR_USE_UNSTABLE
LDLIBS += $(foreach p,$(PKGS),$(shell pkg-config --libs $(p)))

parser: src/parser.c
	mkdir -p ${BUILD_DIR}
	${CC} $^ ${CFLAGS} ${LDLIBS} -o ${BUILD_DIR}/parser

guile: src/guile.c
	mkdir -p ${BUILD_DIR}
	${CC} -shared -o ${BUILD_DIR}/dwl.so ${CFLAGS} ${LDLIBS} -fPIC $^

clean:
	rm -rf ./${BUILD_DIR}
