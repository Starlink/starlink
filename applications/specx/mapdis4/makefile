# Makefile for SPECX: MAPDIS4

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

LIB = mapdis4
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

FSOURCES= anm_block.f anm_col.f contour_map4.f convarr.f \
copy_mappos.f draw_mapconb.f export_map4.f \
get_cube_data.f getmap2.f getpars.f goodpt.f \
grid_map.f interp_map.f interparr.f label_map.f \
listmp.f make_chan4.f make_line4.f make_map4.f \
map_coords.f map_integ.f map_locate.f map_maxmin.f \
map_window.f mapimage.f mark_point.f mark_samples.f \
mwindo.f new_scales.f plot2d_range.f plot_chan4.f \
plot_line4.f plot_map.f scale_bar.f set_disp_siz.f \
setccol.f setclevs.f setgscal.f setpix.f zlabel.f


CSOURCES = 

# These are the external include files
# They will be deleted by make clean (they are soft links).
EXTERNAL_INCLUDES = CUBE FLAGCOMM MAPHD MAPS PLOT2D PROTOTYPE \
   SPECX_PARS STACKCOMM MAPTITLES

# These are the actual private include files that are archived.
# Upper case versions are removed with 'make clean'

PRIVATE_INCLUDES = anm_col.inc anm_logcol.inc

SOURCE   = ${PRIVATE_INCLUDES} ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libmapdis4.a: ${OBJS} 
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

specx_mapdis4.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

CUBE:           $(SPX_INC)/cube.inc;       $(LINK) $? $@
FLAGCOMM:       $(SPX_INC)/flagcomm.inc;   $(LINK) $? $@
MAPHD:          $(SPX_INC)/maphd.inc;      $(LINK) $? $@
MAPS:           $(SPX_INC)/maps.inc;       $(LINK) $? $@
MAPTITLES:      $(SPX_INC)/maptitles.inc;  $(LINK) $? $@
PLOT2D:         $(SPX_INC)/plot2d.inc;     $(LINK) $? $@
PROTOTYPE:      $(SPX_INC)/prototype.inc;  $(LINK) $? $@
SPECX_PARS:     $(SPX_INC)/specx_pars.inc; $(LINK) $? $@
STACKCOMM:      $(SPX_INC)/stackcomm.inc;  $(LINK) $? $@

ANM_COL.INC:    anm_col.inc;           $(LINK) $? $@
ANM_LOGCOL.INC: anm_logcol.inc;        $(LINK) $? $@

#  Include file dependencies.
#  ==========================

#  Object file dependencies on include files (or links to those include
#  files).

anm_block.o: ANM_COL.INC
anm_col.o: ANM_COL.INC ANM_LOGCOL.INC FLAGCOMM
axrang.o: FLAGCOMM
contour_map4.o: MAPHD MAPS FLAGCOMM STACKCOMM PLOT2D
export_map4.o: FLAGCOMM PLOT2D
getmap2.o: CUBE FLAGCOMM MAPHD MAPS PLOT2D
getpars.o: CUBE MAPS MAPHD PLOT2D FLAGCOMM PROTOTYPE
goodpt.o: FLAGCOMM MAPHD PLOT2D
grid_map.o: FLAGCOMM MAPHD MAPS PLOT2D CUBE SPECX_PARS STACKCOMM MAPTITLES
label_map.o: FLAGCOMM MAPHD PLOT2D MAPTITLES
listmp.o: CUBE MAPHD MAPS FLAGCOMM STACKCOMM
make_chan4.o: MAPHD MAPS CUBE PLOT2D FLAGCOMM STACKCOMM MAPTITLES
make_line4.o: MAPHD MAPS PLOT2D CUBE FLAGCOMM STACKCOMM MAPTITLES
make_map4.o: MAPHD MAPS CUBE FLAGCOMM PLOT2D STACKCOMM
map_integ.o: FLAGCOMM PLOT2D SPECX_PARS
map_coords.o: MAPTITLES
map_locate.o: FLAGCOMM PLOT2D
map_maxmin.o: PLOT2D
map_window.o: MAPHD FLAGCOMM PLOT2D
mark_samples.o: FLAGCOMM PLOT2D
mwindo.o: MAPHD FLAGCOMM PLOT2D
new_scales.o: PLOT2D FLAGCOMM 
plot2d_range.o: MAPHD PLOT2D MAPTITLES
plot_chan4.o: MAPHD MAPS CUBE PLOT2D FLAGCOMM STACKCOMM MAPTITLES
plot_line4.o: MAPHD MAPS MAPTITLES PLOT2D FLAGCOMM STACKCOMM
plot_map.o: FLAGCOMM MAPHD MAPS PLOT2D MAPTITLES
setclevs.o: FLAGCOMM
setgscal.o: FLAGCOMM
