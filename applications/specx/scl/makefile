# Makefile for SPECX: SCL

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

LIB = scl
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

FSOURCES= commands.f do_syscomm.f init_symbols.f scl_commsyms.f \
scl_continue.f scl_do.f scl_help.f scl_if.f \
scl_matchcomm.f scl_parse.f scl_symbols.f scl_wild.f

CSOURCES = 

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = COMMAND_TABLE ERRORS SYMBOLS IODATA JPI

# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES = scl_buffer.inc scl_do.inc scl_ifdo.inc

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libscl.a: ${OBJS} 
	ar r $@ $?

all: lib${LIB}.a

# Remove intermediates
clean:	
	rm -f $(OBJS) 1>/dev/null 2>/dev/null; \
	rm -f $(EXTERNAL_INCLUDES) 1>/dev/null 2>/dev/null; \
	rm -f `echo $(PRIVATE_INCLUDES) | tr "[:lower:]" "[:upper:]"` 1>/dev/null 2>/dev/null;

# Remove everything
distclean: clean
	rm -f lib${LIB}.a 1>/dev/null 2>/dev/null; \
	rm -f ${PKG}_${LIB}.tar 1>/dev/null 2>/dev/null;


install: lib${LIB}
	${INSTALL} ${INSTOPTS} lib${LIB}.a ${INSTDIR}

export: ${PKG}_${LIB}.tar

specx_scl.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

#  Include file dependencies.
#  ==========================

SCL_BUFFER.INC: scl_buffer.inc;      $(LINK) $? $@
SCL_IFDO.INC:   scl_ifdo.inc;        $(LINK) $? $@
SCL_DO.INC:     scl_do.inc;          $(LINK) $? $@

COMMAND_TABLE:  $(SPX_INC)/commands.inc; $(LINK) $? $@
ERRORS:         $(SPX_INC)/errors.inc;   $(LINK) $? $@
IODATA:         $(SPX_INC)/iodata.inc;   $(LINK) $? $@
JPI:            $(SPX_INC)/jpi.inc;      $(LINK) $? $@
SYMBOLS:        $(SPX_INC)/symbols.inc;  $(LINK) $? $@

#  Object file dependencies on include files (or links to those include
#  files).

commands.o: COMMAND_TABLE
init_symbols.o: SYMBOLS COMMAND_TABLE
scl_commsyms.o: SYMBOLS COMMAND_TABLE IODATA
do_syscomm.o: SCL_IFDO.INC SCL_BUFFER.INC JPI IODATA
scl_help.o: COMMAND_TABLE
scl_continue.o: ERRORS
scl_do.o: SCL_BUFFER.INC SCL_DO.INC SCL_IFDO.INC
scl_matchcomm.o: COMMAND_TABLE
