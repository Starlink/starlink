# Makefile for SPECX: EXPRT

#  Copyright:
#     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
#     Research Council. All Rights Reserved.

#  Author:
#     Tim Jenness (JACH)

# Include directories
# Should really inherit values from parent makefile

STAR_INC = /star/include
STAR_LIB = /star/lib
SPX_INC  = ../inc

# Package and subsystem name

PKG = specx

# Compiler definitions

FC     = f77
FFLAGS = -g -C
CFLAGS = -I${STAR_INC}
LINK   = ln -s

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Specx libraries

#SPXLIBS = -L../astro -lastro -L../command -lcommand -L../exprt -lexprt \
#-L../external -lexternal -L../fitting -lfitting -L../fv4 -lfv4 \
#-L../gen -lgen -L../graph -Lgraph -L../mapdis4 -lmapdis4 -L../mv4 -lmv4 \
#-L../plots -lplots -L../reduce -lreduce -L../scl -lscl -L../tek -ltek \
#-L../term -lterm -L../util -lutil -L../vms -lvms

SPXLIBS = ../command/libcommand.a ../exprt/libexprt.a \
../external/libexternal.a \
../reduce/libreduce.a ../scl/libscl.a ../fitting/libfitting.a \
../plots/libplots.a ../term/libterm.a ../mapdis4/libmapdis4.a \
../tek/libtek.a ../plots/libplots.a ../mapdis4/libmapdis4.a ../mv4/libmv4.a \
../fv4/libfv4.a ../util/libutil.a ../graph/libgraph.a ../astro/libastro.a \
../gen/libgen.a ../vms/libvms.a ../gsd/libgsd.a \
../getaline/libgetaline.a

EXTLIBS = -L${STAR_LIB} `./specx_link` -lreadline
HEXTLIBS = -L${STAR_LIB} `./specx_help_link`

# Source code

FSOURCES= scl_main.f

CSOURCES = run_specx.c

# For the help task
HELPSOURCE = helpm.c helpc.c

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = DAT_PAR \
   COMMAND_TABLE ERRORS SYMBOLS SPECX_PARS \
   CUBE DOPPLER FILES FILHD FILHDI2 FLAGCOMM HEADER MAPHD MAPS MAPV4 \
   NEWXY PLOT2D PLOTPAR1 PROTOTYPE SPECX_FITS STACKCOMM STAKPAR WEIGHTS


# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'
PRIVATE_INCLUDES = title.inc

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES} \
 ${HELPSOURCE}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}
HOBJS = ${HELPSOURCE:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

specx: ${OBJS}
	${FC} ${CFLAGS} ${SPXLIBS} ${EXTLIBS} ${OBJS} -o $@

specx_help: ${HOBJS}
	${FC} ${CFLAGS} ${HOBJS} ${HEXTLIBS} -o $@

all: specx specx_help

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

specx_exprt.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

#  Include file dependencies.
#  ==========================

DAT_PAR:      $(STAR_INC)/dat_par; $(LINK) $? $@
COMMAND_TABLE: $(SPX_INC)/commands.inc;   $(LINK) $? $@
ERRORS:        $(SPX_INC)/errors.inc;     $(LINK) $? $@
SYMBOLS:       $(SPX_INC)/symbols.inc;    $(LINK) $? $@
SPECX_PARS:    $(SPX_INC)/specx_pars.inc; $(LINK) $? $@
CUBE:          $(SPX_INC)/cube.inc;       $(LINK) $? $@
DOPPLER:       $(SPX_INC)/doppler.inc;    $(LINK) $? $@
FILES:         $(SPX_INC)/files.inc;      $(LINK) $? $@
FILHD:         $(SPX_INC)/filhd.inc;      $(LINK) $? $@
FILHDI2:       $(SPX_INC)/filhdi2.inc;    $(LINK) $? $@
FLAGCOMM:      $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
HEADER:        $(SPX_INC)/header.inc;     $(LINK) $? $@
MAPHD:         $(SPX_INC)/maphd.inc;      $(LINK) $? $@
MAPS:          $(SPX_INC)/maps.inc;       $(LINK) $? $@
MAPV4:         $(SPX_INC)/mapv4.inc;      $(LINK) $? $@
NEWXY:         $(SPX_INC)/newxy.inc;      $(LINK) $? $@
PLOT2D:        $(SPX_INC)/plot2d.inc;     $(LINK) $? $@
PLOTPAR1:      $(SPX_INC)/plotpar1.inc;   $(LINK) $? $@
PROTOTYPE:     $(SPX_INC)/prototype.inc;  $(LINK) $? $@
SPECX_FITS:    $(SPX_INC)/specx_fits.inc; $(LINK) $? $@
STACKCOMM:     $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@ 
STAKPAR:       $(SPX_INC)/stakpar.inc;    $(LINK) $? $@
WEIGHTS:       $(SPX_INC)/weights.inc;    $(LINK) $? $@

TITLE:         title.inc;          $(LINK) $? $@

#  Object file dependencies on include files (or links to those include
#  files).

scl_main.o: CUBE DOPPLER FILES FILHD FILHDI2 FLAGCOMM HEADER MAPHD \
  MAPS MAPV4 NEWXY PLOT2D PLOTPAR1 PROTOTYPE SPECX_FITS STAKPAR WEIGHTS \
  TITLE DAT_PAR SPECX_PARS COMMAND_TABLE ERRORS SYMBOLS
