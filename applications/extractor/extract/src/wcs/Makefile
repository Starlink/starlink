# Custom makefile for the WCS library
# E. Bertin 27/05/97
#
#
# where to put the result
DEST	= ..
include $(DEST)/Macrodefs.$(SEXMACHINE)
DEFINES = -DCOPYSIGN -DSIGNBIT
LIBS	= -lm
SOURCES	= cel.c lin.c proj.c sph.c wcs.c wcstrig.c
OBJECTS	= $(SOURCES:.c=.o)
INCLUDES= cel.h lin.h proj.h wcs.h wcstrig.h
LIBRARY = $(DEST)/libwcs_c.a

$(LIBRARY): $(OBJECTS)
	$(RM) $@
	ar rc $@ $(OBJECTS)
	-ranlib $@

.c.o: $(INCLUDES)
	$(CC) $(COPTS) $(DEFINES) -c $*.c

clean:
	$(RM) $(OBJECTS) $(LIBRARY)

