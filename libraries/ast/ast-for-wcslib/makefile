
# Defaults suitable for linux (redhat 9): override these with values 
# suitable to the OS being used.
CC = gcc
CFLAGS = -ansi -O -fPIC
AR_IN = ar -r
RANLIB = ranlib
LDFLAGS = -lm

C_ROUTINES = channel.c error.c mapping.c mathmap.c memory.c object.c \
pointset.c unit.c unitmap.c zoommap.c err_null.c loader.c

OBJECT_FILES = $(C_ROUTINES:.c=.o)

.c.o:
	$(CC) $(CFLAGS) -I. -c $<

build:	libast.a

libast.a: $(OBJECT_FILES)
	$(AR_IN) $@ $?
	$(RANLIB) $@

clean:
	@- rm -f $(OBJECT_FILES) >/dev/null 2>/dev/null

check test: astTester.c libast.a
	$(CC) -o asttest astTester.c libast.a $(LDFLAGS); \
	./asttest; \
	rm -f asttest >/dev/null 2>/dev/null

