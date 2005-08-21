## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libutil.la
AM_FCFLAGS = -I../inc

libutil_la_SOURCES = $(F_ROUTINES) $(FPP_ROUTINES) $(PRIVATE_INCLUDES)

F_ROUTINES = add.f compressq.f convol.f copybf.f dasmerge.f dectim.f  \
doquad.f exchnge.f four2.f frqfix.f get_subarr.f icheck.f ichkq2.f    \
init_array.f initlun.f initnpnew.f initvm.f inquad.f interp.f	      \
islct1q.f islctq.f lmm2.f lun_hndl.f mult.f npts2.f ntot.f ntot2.f    \
ntrans.f pda_dsort.f pda_xermsg.f pop.f push.f qlim.f qmult.f range.f \
recall.f roll.f setx.f solve4.f specx_reorder.f spliceq.f storex.f    \
su.f swap2.f swap8.f swap_arr.f tran_date.f tran_time.f uread.f	      \
uwrite.f vm_hndl.f xcopy.f xsnart.f xsnart8.f xtrans.f xtrans8.f      \
xtrctq.f xy.f parse_date_obs.f

FPP_ROUTINES = ndlib.F

PRIVATE_INCLUDES = VMTAB.INC LUNTAB.INC
