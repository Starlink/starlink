## Process this file with automake to produce Makefile.in

SUBDIRS = kapsub

bin_SCRIPTS = $(DYNSCRIPTS) version.dat

dist_noinst_SCRIPTS = kaprh_link_adam

LOCAL_LINK = kapsub/libkapsub.la `$(srcdir)/kaprh_link_adam`

bin_MONOLITHS = kaprh_mon 
kaprh_mon_SOURCES = kaprh_mon.f $(KAPRH_F_ROUTINES) 
kaprh_mon_LDADD = $(LOCAL_LINK)
kaprh_mon_TASKS = $(KAPRH_TASKS)

#  version.dat is just the package verion in a file for kapversion command
#  This is for backwards compatibility since, of course, we could make
#  kapversion have the value compiled in
version.dat:
	-rm -f version.dat
	echo $(PACKAGE_VERSION) > version.dat

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines and the main routine, which are treated separately).

F_ROUTINES = $(KAPRH_F_ROUTINES) $(KAPSUB_F_ROUTINES) \
             $(MONO_SOURCE) 

#  kaprh routines for which the atask action name can be derived from the 
#  the f file name by remving the .f file suffix.
KAPRH_F_ROUTINES = \
crelut.f greyplot.f contover.f snapshot.f turbocont.f inspect.f krhhelp.f \
mosaic.f quilt.f lutflip.f luthilite.f lutrot.f luttweak.f idinvisible.f \
idpazo.f idset.f idclear.f idstate.f ovset.f ovclear.f \
transformer.f traninvert.f tranjoin.f tranmake.f trantrace.f

MONO_SOURCE = kaprh_mon.f

#  Define the tasks for each monolith.

KAPRH_TASKS = $(KAPRH_F_ROUTINES:.f=) 


#  Define the set of all interface files.

IFL_FILES = $(KAPRH_IFL_FILES) 

#  Define the interface files required for compilation.

KAPRH_IFL_FILES = $(KAPRH_F_ROUTINES:.f=.ifl)
MONO_IFL = $(MONO_SOURCE:.f=.ifl)

DYNSCRIPTS = kaprh.csh kaprh.icl kaprh.sh

#  Generate ifl and startup scripts from the ifd file and then do any 
#  necessary post processing of the generated files to overcome 
#  deficiencies in ifd2star
$(IFL_FILES) $(DYNSCRIPTS): kaprh.ifd
	$(IFD2STAR) kaprh
	sed -e 's#prefix=""#prefix=\\"\\"#g' kaprh.csh > temp
	mv temp kaprh.csh
	sed -e 's#prefix=""#prefix=\\"\\"#g' kaprh.sh > temp
	mv temp kaprh.sh


# Help source file
starhelp_DATA = kaprh.shl

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

HELP_SOURCES = kaprh_master.hlp

.hlp.shl:
	@HLIB@ $<

kaprh.hlp: $(KAPRH_F_ROUTINES) kaprh_master.hlp
	cat $(KAPRH_F_ROUTINES) > all.f
	PATH="$(PATH)":$(STARLINK)/bin/sst; \
	export PATH ; \
	@PROHLP@ in=all.f out=all.hlp single=no
	cat kaprh_master.hlp all.hlp > kaprh.hlp
	-rm -f all.hlp all.f

dist_starnews_DATA = kaprh.news

EXTRA_DIST = $(HELP_SOURCES) 

MISC_FILES = kaprh.shl kaprh.hlp kaprh_mon version.dat kaprh.csh \
kaprh.icl kaprh.sh

IFC_FILES = contover.ifc crelut.ifc greyplot.ifc idclear.ifc idinvisible.ifc \
idpazo.ifc idset.ifc idstate.ifc inspect.ifc krhhelp.ifc \
lutflip.ifc luthilite.ifc lutrot.ifc luttweak.ifc \
mosaic.ifc ovclear.ifc ovset.ifc quilt.ifc snapshot.ifc \
traninvert.ifc tranjoin.ifc tranmake.ifc transformer.ifc \
trantrace.ifc turbocont.ifc kaprh_mon.ifc

CLEANFILES = $(IFL_FILES) $(MISC_FILES) $(IFC_FILES)
             
