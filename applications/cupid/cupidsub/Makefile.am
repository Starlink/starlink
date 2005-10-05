## Process this file with automake to produce Makefile.in


# .cupid files are preprocessed using the sed-bases rules in this
# makefile to create a corresponding .c file for each supported data 
# type.
GENERIC_C_ROUTINES = cupidfindmax.cupid cupiditerate.cupid cupidrms.cupid \
cupidsetinit.cupid cupidcubfit.cupid cupidupdatearrays.cupid

# The .c files which are built from the above .cupid files.
BUILT_C_ROUTINES = $(GENERIC_C_ROUTINES:.cupid=D.c) \
                   $(GENERIC_C_ROUTINES:.cupid=F.c)

# All the .c files which are put into the libcupidsub library. This
# includes the type-specific files which are created from the above
# generic .cupid files.
C_ROUTINES = cupidclumpfind.c cupidgaussclumps.c cupidconfigD.c \
cupidconfigI.c cupidlistclump.c \
$(BUILT_C_ROUTINES)

noinst_LTLIBRARIES = libcupidsub.la

libcupidsub_la_SOURCES = $(C_ROUTINES)

AM_CFLAGS = -I..

$(C_ROUTINES:.c=.o): ../cupid.h

# Create the .c file for handling double data from the corresponding
# .cupid file.
$(GENERIC_C_ROUTINES:.cupid=D.c): %D.c : %.cupid
	sed -e 's#:X:#D#g' \
	    -e 's#:Xtype:#double#g' \
	    -e 's#:Badval:#VAL__BADD#g' \
		 $< > $@

# Create the .c file for handling float data from the corresponding
# .cupid file.
$(GENERIC_C_ROUTINES:.cupid=F.c): %F.c : %.cupid
	sed -e 's#:X:#F#g' \
	    -e 's#:Xtype:#float#g' \
	    -e 's#:Badval:#VAL__BADR#g' \
		 $< > $@


