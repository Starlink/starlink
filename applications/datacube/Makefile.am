## Process this file with automake to produce Makefile.in

bin_SCRIPTS = datacube.csh datacube.icl

dist_bin_SCRIPTS = compare.csh compare_x.csh datacube_link_adam \
                   multistack.csh passband.csh peakmap.csh \
                   ripper.csh squash.csh stacker.csh step.csh \
                   velmap.csh

bin_MONOLITHS = datacube_mon

# stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

datacube_mon_SOURCES = datacube_mon.f 
datacube_mon_TASKS = copyaxis getbound putaxis 
   
datacube_mon_LDADD = \
   tasks/libdatacube_tasks.la subs/libdatacube_subs.la  `ndf_link_adam`

dist_starnews_DATA = datacube.news
dist_noinst_HEADERS = include/CUB1_PAR

SUBDIRS = demo help subs tasks sc16 sun237
            
CLEANFILES = version.dat getbound.ifc putaxis.ifc copyaxis.ifc \
             datacube_mon datacube_mon.ifc datacube_mon.ifl
