## Process this file with automake to produce Makefile.in

##  EXTRACTOR is a little different in that it uses two pre-existing
##  directories (which are automake/autoconfed using the SExtractor 
##  methodology). These do most of the work, this is just a wrapper
##  for extracting the parts needed and building the EXTRACTOR atask
##  and finally installing.

bin_SCRIPTS = extractor.csh extractor.icl extractor.ifc

bin_PROGRAMS = extractor sex

noinst_LIBRARIES = libextractor.a libfits.a

##  There are no local sources for these libraries and programs.
libextractor_a_SOURCES =
libfits_a_SOURCES =
extractor_SOURCES =
sex_SOURCES =

## Require the extractor library and sex binary to be built sources. 
## These are actually created in sub-directories, so we need a target to get
## them into the local dependencies.

BUILT_SOURCES = libextractor.a libfits.a sex

#  SExtractor config directory. Keep this upto date.
dist_pkgdata_DATA = \
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
## extracting the required products. ?? What to make these dependent on?

#  EXTRACTOR needs the libextractor.a and libfits.a libraries.
libextractor.a:
	(cd extract ;\
	export INSTALL; unset INSTALL;\
	export SHELL; unset SHELL;\
	export LINK; unset LINK; \
        make ;\
        cp -p src/libextractor.a .. ;\
        cp -p src/fits/libfits.a .. ;\
        cp -p src/extractor.o ..)

#  SExtractor runs to completion.
sex:
	(cd sextractor ;\
        export INSTALL; unset INSTALL;\
        export SHELL; unset SHELL;\
        export LINK; unset LINK; \
        make ;\
        cp -p src/sex$(EXEEXT) ..)

##  Clean recurses into these directories.
clean-local:
	(cd extract; make clean )
	(cd sextractor;make clean )

distclean-local:
	(cd extract; make distclean )
	(cd sextractor;make distclean )

##  Link the main extractor program as an atask. Note this isn't a monolith so
##  we cannot use the MONOLITH builtin rules.
extractor: extract
	@ALINK@ extractor.o -o extractor libextractor.a libfits.a \
		$(ADDLIBS) `ndf_link_adam`

extractor.ifl:
	cp -p extract/src/extractor.ifl .

extractor.icl:
	cp -p extract/src/extractor.icl .

#  IFL to IFC
.ifl.ifc:
	@COMPIFL@ $<
