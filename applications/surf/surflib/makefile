FC    = f77
FFLAGS = -g -C
CFLAGS = 

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

OBJS = ${FSOURCES:.f=.o}

.f.o:
	${FC} ${FFLAGS} -c $<

lib:	${OBJS} ${CSOURCES:.c=.o}
	ar r libsurf_sub.a $?

clean:	
	/bin/rm -f libsurf_sub.a *.o

install: lib
	${INSTALL} ${INSTOPTS} libsurf_sub.a ${INSTDIR}

