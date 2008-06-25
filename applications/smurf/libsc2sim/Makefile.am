if HAVE_GCC
GCC_DEBUG_FLAGS = -Wall -Wextra -Wcast-qual -Wcast-align -Wwrite-strings \
-Wstrict-prototypes -Wmissing-prototypes -Wmissing-declarations	         \
-Wredundant-decls -fstack-protector
endif
AM_CFLAGS = $(GCC_DEBUG_FLAGS)

noinst_LTLIBRARIES = libsc2sim.la

libsc2sim_la_SOURCES = $(C_ROUTINES)

C_ROUTINES =  \
sc2sim_addpnoise.c sc2sim_atmsky.c sc2sim_atmtrans.c \
sc2sim_calctime.c sc2sim_calctrans.c sc2sim_digitise.c sc2sim_drand.c \
sc2sim_fft2d.c sc2sim_fitheat.c sc2sim_four1.c sc2sim_getast_wcs.c \
sc2sim_getbilinear.c sc2sim_getcoordframe.c sc2sim_getbous.c \
sc2sim_getcurvepong.c sc2sim_getinvf.c sc2sim_getliss.c \
sc2sim_getmicstp.c \
sc2sim_getobsmode.c sc2sim_getobspar.c sc2sim_getpat.c \
sc2sim_getpongends.c sc2sim_getpongvert.c \
sc2sim_getscaling.c sc2sim_getscanseg.c sc2sim_getscansegsize.c \
sc2sim_getsigma.c sc2sim_getsimpar.c sc2sim_getsinglescan.c \
sc2sim_getspread.c sc2sim_getstraightpong.c sc2sim_getweights.c \
sc2sim_heatrun.c sc2sim_instrinit.c sc2sim_invf.c sc2sim_invf2d.c \
sc2sim_ndfwrdata.c sc2sim_ndfwrheat.c sc2sim_ptoi.c sc2sim_response.c \
sc2sim_sex2double.c sc2sim_simframe.c sc2sim_simulate.c \
sc2sim_smupath.c sc2sim_smupos.c sc2sim_dateobs.c sc2sim_instap_calc.c \
sc2sim_simstats.c sc2sim_get_recipe.c sc2sim_get_drgroup.c

noinst_INCLUDES = sc2sim.h sc2sim_par.h sc2sim_struct.h

