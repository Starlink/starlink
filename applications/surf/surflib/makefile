FC    = f77
FFLAGS = -g -C
CFLAGS = 
LINK   = ln -s

STAR_INC = /star/include

LIB = surflib
PKG = surf

# install

INSTALL = /usr/ucb/install
INSTDIR = /jcmt_sw/sun4_Solaris/lib
INSTOPTS = -g jcmtsw

# Source code

FSOURCES= surflib_diff_despike.f surflib_calc_output_grid.f \
	surflib_calc_ijpos.f surflib_histogram_grid.f \
	surflib_fill_grid.f surflib_plot_grid.f \
	surflib_clip_grid.f surflib_calc_gridij.f \
	surflib_decode_remsky_string.f surflib_stats_grid.f \
	surflib_remove_dc_from_exp.f surflib_2dft_chop.f \
	surflib_rem_grid.f surflib_rem_timeseries.f \
	surflib_median_regrid.f

CSOURCES = 

# These are the external include files
EXTERNAL_INCLUDES = SAE_PAR MSG_PAR PRM_PAR NDF_FUNC PAR_ERR


SOURCE   = ${FSOURCES} ${FCPPSOURCES} ${CSOURCES}

OBJS = ${FSOURCES:.f=.o} ${FCPPSOURCES:.F=.o} ${CSOURCES:.c=.o}

.F.o:
	$(FC) $(FFLAGS) $(FCPPFLAGS) -c $<

.f.o:
	${FC} ${FFLAGS} -c $<

libsurflib.a: ${OBJS} 
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

surf_surflib.tar: $(SOURCE)
	rm -f $@ 1>/dev/null 2>/dev/null; \
	tar cf $@ $(SOURCE) 

# This is the dependency stuff

SAE_PAR:     $(STAR_INC)/sae_par;     $(LINK) $? $@
MSG_PAR:     $(STAR_INC)/msg_par;     $(LINK) $? $@
PRM_PAR:     $(STAR_INC)/prm_par;     $(LINK) $? $@
PAR_ERR:     $(STAR_INC)/par_err;     $(LINK) $? $@
NDF_FUNC:    $(STAR_INC)/ndf_func;    $(LINK) $? $@

surflib_2dft_chop.o: SAE_PAR
surflib_calc_gridij.o: PRM_PAR SAE_PAR
surflib_calc_ijpos.o: PRM_PAR SAE_PAR
surflib_calc_output_grid.o: PRM_PAR SAE_PAR
surflib_clip_grid.o: MSG_PAR PAR_ERR PRM_PAR SAE_PAR
surflib_decode_remsky_string.o: MSG_PAR PRM_PAR SAE_PAR
surflib_diff_despike.o: MSG_PAR NDF_FUNC PRM_PAR SAE_PAR
surflib_fill_grid.o: NDF_FUNC PRM_PAR SAE_PAR
surflib_histogram_grid.o: NDF_FUNC PRM_PAR SAE_PAR
surflib_median_regrid.o: MSG_PAR PAR_ERR PRM_PAR SAE_PAR
surflib_plot_grid.o: MSG_PAR PAR_ERR PRM_PAR SAE_PAR
surflib_rem_grid.o: PRM_PAR SAE_PAR
surflib_rem_timeseries.o: PRM_PAR SAE_PAR
surflib_remove_dc_from_exp.o: MSG_PAR PRM_PAR SAE_PAR
surflib_stats_grid.o: PRM_PAR SAE_PAR
