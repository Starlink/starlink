## Process this file with automake to produce Makefile.in

noinst_LTLIBRARIES = libphotom_subs.la

libphotom_subs_la_SOURCES = $(F_ROUTINES) $(BLOCKDATA)

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately). Keep this list in alphabetical
#  order so that the subroutine description html comes out in the right 
#  order.

F_ROUTINES = \
  aif_antmp.f aif_temp.f aptop.f autom.f back.f boxell.f chparr.f \
  clgrid.f covsort.f curfit.f cushap.f cyc_dat.f cyc_par.f dgauss.f \
  dosum.f endsgs.f extr.f fix.f fpeak.f gauss.f gaussj.f gfit.f header.f \
  hinsec.f inside.f intell.f limit.f locate.f makell.f marquardt.f \
  mean1.f mean2s.f measur.f merge.f mmm.f mrqcof.f nthmin.f ohead.f \
  outmen.f outopt.f outpsf.f outres.f outval.f pho1_agrp.f pho1_asfio.f \
  pho1_auto.f pho1_back.f pho1_etime.f pho1_gcalc.f pho1_gkey.f \
  pho1_gopt.f pho1_gpsf.f pho1_prcur.f pho1_ragged.f pho1_wrres.f \
  plotel.f polare.f polins.f polpre.f polyin.f psfcal.f ragged.f \
  ref_split.f reset.f setsgs.f shsort.f side.f skycon.f sumflx.f \
  svd.f tetime.f tgauss.f trarea.f update.f vinsec.f ysweep.f

BLOCKDATA = blockdata.f
