## Process this file with automake to produce Makefile.in

##  EXTRACTOR is a little different in that it uses two pre-existing
##  directories (which are automake/autoconfed using the SExtractor
##  methodology). These do most of the work, this is just a wrapper
##  for extracting the parts needed and building the EXTRACTOR atask
##  and finally installing.

nodist_bin_SCRIPTS = extractor.csh extractor.sh extractor.icl extractor.ifc

bin_MONOLITHS = extractor
extractor_SOURCES = extract/src/extractor.c
# no variable extractor_TASKS -- extractor is an ATASK
extractor_LDADD = $(LDADD) libextractor.a libfits.a `ndg_link_adam`

bin_PROGRAMS = sex

noinst_LIBRARIES = libextractor.a libfits.a

# Files for the docs, directory. Some of these may present in the .htx
# subdirectory as they are needed by the hypertext document, this just
# copies the files out of there and into "star/docs" when necessary.
stardocs_DATA = @STAR_LATEX_DOCUMENTATION@ sun226.htx/mud165.pdf sun226_fig.pdf


##  There are no local sources for these libraries and programs.
libextractor_a_SOURCES =
libfits_a_SOURCES =
sex_SOURCES =

## Require the extractor library and sex binary to be built sources.
## These are actually created in sub-directories, so we need a target to get
## them into the local dependencies.

BUILT_SOURCES = libextractor.a libfits.a sex$(EXEEXT) extractor.ifl

#  Data for share/extractor. Currently this is any misc docs (news file) 
#dist_pkgdata_DATA = extractor.news
dist_starnews_DATA = extractor.news

#  Configuration information (in this case the SExtractor config
#  directory, keep this upto date).
extractorsupportdir = $(bindir)/config
extractorsupport_DATA = \
 extract/src/config/block_3x3.conv extract/src/config/gauss_3.0_5x5.conv\
 extract/src/config/mexhat_5.0_11x11.conv extract/src/config/default.conv\
 extract/src/config/gauss_3.0_7x7.conv extract/src/config/README\
 extract/src/config/default.nnw extract/src/config/gauss_4.0_7x7.conv\
 extract/src/config/tophat_1.5_3x3.conv extract/src/config/default.param\
 extract/src/config/gauss_5.0_9x9.conv extract/src/config/tophat_2.0_3x3.conv\
 extract/src/config/default.sex extract/src/config/mexhat_1.5_5x5.conv\
 extract/src/config/tophat_2.5_3x3.conv extract/src/config/gauss_1.5_3x3.conv \
 extract/src/config/mexhat_2.0_7x7.conv extract/src/config/tophat_3.0_3x3.conv\
 extract/src/config/gauss_2.0_3x3.conv extract/src/config/mexhat_2.5_7x7.conv\
 extract/src/config/tophat_4.0_5x5.conv extract/src/config/gauss_2.0_5x5.conv\
 extract/src/config/mexhat_3.0_9x9.conv extract/src/config/tophat_5.0_5x5.conv\
 extract/src/config/gauss_2.5_5x5.conv  extract/src/config/mexhat_4.0_9x9.conv

## Local rules for building the (pre-configured) sub-directories and
## extracting the required products. When developing you need to use the
## makefiles in the sub-directories and return to this level to complete the
## build.

#  EXTRACTOR needs the libextractor.a and libfits.a libraries.

libextractor.a: extract/src/libextractor.a

extract/src/libextractor.a:
	(cd extract ;\
	export INSTALL; unset INSTALL;\
	export SHELL; unset SHELL;\
	export LINK; unset LINK; \
        make ;\
        cp -p src/libextractor.a .. ;\
        cp -p src/fits/libfits.a .. ;\
        cp -p src/extractor.o ..)

#  SExtractor runs to completion.

sex$(EXEEXT): sextractor/src/sex$(EXEEXT)

sextractor/src/sex$(EXEEXT):
	(cd sextractor ;\
        export INSTALL; unset INSTALL;\
        export SHELL; unset SHELL;\
        export LINK; unset LINK; \
        make ;\
        cp -p src/sex$(EXEEXT) ..)

##  Clean recurses into these directories.
clean-local:
	- (cd extract; make clean )
	- (cd sextractor; make clean )

distclean-local:
	- (cd extract; make distclean )
	- (cd sextractor; make distclean )

extractor.ifl: extract/src/extractor.ifl
	cp -p extract/src/extractor.ifl .

#  Rule IFL to IFC compilation.
.ifl.ifc:
	@COMPIFL@ $<

## Clean up extra generated files.
MAINTAINERCLEANFILES = extractor.ifl

##  Need all the source code in the sub-directories.
EXTRA_DIST = extract sextractor extractor.csh.in extractor.icl.in \
	extractor.sh.in

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the CVS trace.
dist-hook:
	rm -rf `find $(distdir) -name CVS`
	(cd $(distdir)/extract; make distclean);
	(cd $(distdir)/sextractor; make distclean);

# Fixup scripts with bin location
# The scripts needs to have installation locations edited into them
edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@starhelpdir\@,$(starhelpdir),g' \
	-e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g'

extractor.csh: extractor.csh.in
	-rm -f extractor.csh
	$(edit) $(srcdir)/extractor.csh.in > extractor.csh.tmp
	mv extractor.csh.tmp extractor.csh

extractor.sh: extractor.sh.in
	-rm -f extractor.sh
	$(edit) $(srcdir)/extractor.sh.in > extractor.sh.tmp
	mv extractor.sh.tmp extractor.sh

CLEANFILES = extractor.csh version.dat extractor extractor.ifc \
             extractor.icl extractor.sh
