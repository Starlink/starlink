## Process this file with automake to produce Makefile.in

SUBDIRS = @subdirs@

bin_SCRIPTS = datacube.csh datacube.icl \
  compare.csh  compare_x.csh  multistack.csh ripper.csh  step.csh \
  passband.csh squash.csh velmap.csh peakmap.csh stacker.csh

bin_MONOLITHS = datacube_mon

datacube_mon_SOURCES = datacube_mon.f 
datacube_mon_TASKS = copyaxis getbound putaxis 
   
datacube_mon_LDADD = \
   tasks/libdatacube_tasks.la subs/libdatacube_subs.la  `ndf_link_adam`

dist_pkgdata_DATA = datacube.news
