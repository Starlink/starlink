# Makefile for SPECX: COMMAND

#  Copyright:
#     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

#  Author:
#     Tim Jenness (JACH)

# Include directories
# Should really inherit values from parent makefile

STAR_INC = /star/include
SPX_INC  = ../inc

# Package and subsystem name

LIB = command
PKG = specx

# Compiler definitions

FC     = f77
FFLAGS = -g -C
CFLAGS = 
LINK   = ln -s

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= do_command.f init_specx.f initsp.f specx_initab.f

CSOURCES = 

# These are the external include files
EXTERNAL_INCLUDES =    DAT_PAR \
COMMAND_TABLE CUBE ERRORS FILES FILHD FLAGCOMM \
MAPHD MAPS NEWXY PLOT2D SPECX_PARS STACKCOMM STAKPAR \
GSD_VAR IODATA NOKEEP MAPTITLES 


SOURCE   = ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libcommand.a: ${OBJS} 
	ar r $@ $?

all: lib${LIB}.a

# Remove intermediates
clean:	
	rm -f $(OBJS) 1>/dev/null 2>/dev/null; \
	rm -f $(EXTERNAL_INCLUDES) 1>/dev/null 2>/dev/null;

# Remove everything
distclean: clean
	rm -f lib${LIB}.a 1>/dev/null 2>/dev/null; \
	rm -f ${PKG}_${LIB}.tar 1>/dev/null 2>/dev/null;


install: lib${LIB}
	${INSTALL} ${INSTOPTS} lib${LIB}.a ${INSTDIR}

export: ${PKG}_${LIB}.tar

specx_command.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

DAT_PAR:       $(STAR_INC)/dat_par;   $(LINK) $? $@

GSD_VAR:       ../exprt/gsd_var.inc;  $(LINK) $? $@

COMMAND_TABLE: $(SPX_INC)/commands.inc;   $(LINK) $? $@
CUBE:	       $(SPX_INC)/cube.inc;	  $(LINK) $? $@
ERRORS:        $(SPX_INC)/errors.inc;     $(LINK) $? $@
FILES:         $(SPX_INC)/files.inc;      $(LINK) $? $@
FILHD:         $(SPX_INC)/filhd.inc;      $(LINK) $? $@
FLAGCOMM:      $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
IODATA:        $(SPX_INC)/iodata.inc;     $(LINK) $? $@
MAPHD:         $(SPX_INC)/maphd.inc;      $(LINK) $? $@
MAPS:          $(SPX_INC)/maps.inc;       $(LINK) $? $@
MAPTITLES:     $(SPX_INC)/maptitles.inc;  $(LINK) $? $@
NEWXY:         $(SPX_INC)/newxy.inc;      $(LINK) $? $@
NOKEEP:        $(SPX_INC)/nokeep.inc;     $(LINK) $? $@
PLOT2D:        $(SPX_INC)/plot2d.inc;     $(LINK) $? $@
SPECX_PARS:    $(SPX_INC)/specx_pars.inc; $(LINK) $? $@
STACKCOMM:     $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@
STAKPAR:       $(SPX_INC)/stakpar.inc;    $(LINK) $? $@

#  Include file dependencies.
#  ==========================

#  Object file dependencies on include files (or links to those include
#  files).

do_command.o: STACKCOMM SPECX_PARS MAPS STAKPAR FLAGCOMM
initsp.o: DAT_PAR \
   CUBE FILES FLAGCOMM NEWXY MAPS PLOT2D SPECX_PARS STACKCOMM STAKPAR \
   COMMAND_TABLE IODATA MAPTITLES NOKEEP
init_specx.o: FLAGCOMM COMMAND_TABLE MAPTITLES
specx_initab.o: \
   STACKCOMM FILHD FLAGCOMM GSD_VAR CUBE MAPHD MAPS ERRORS

