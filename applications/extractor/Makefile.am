## Process this file with automake to produce Makefile.in

bin_SCRIPTS = extractor.csh extractor.icl extractor.ifc

bin_PROGRAMS = extractor sex

noinst_LIBRARIES = libextractor.a libfits.a

libextractor_a_SOURCES =
libfits_a_SOURCES =
extractor_SOURCES =
sex_SOURCES =

## Require the extractor library and sex binary to be built sources. These are
## actually created in sub-directories, so we need a target to get them.

BUILT_SOURCES = libextractor.a libfits.a sex

#  config directory....


## Local rules for building the (pre-configured) sub-directories.

libextractor.a: extractor
	(cd extract ;\
	export INSTALL; unset INSTALL;\
	export SHELL; unset SHELL;\
	export LINK; unset LINK; \
        make ;\
        cp -p src/libextractor.a .. ;\
        cp -p src/fits/libfits.a .. ;\
        cp -p src/extractor.o ..)

sex: sextractor
	(cd sextractor ;\
        export INSTALL; unset INSTALL;\
        export SHELL; unset SHELL;\
        export LINK; unset LINK; \
        make ;\
        cp -p src/sex ..)

extractor: extract
	@ALINK@ extractor.o -o extractor libextractor.a libfits.a \
		$(ADDLIBS) `@NDF_LINK_ADAM@`

extractor.ifl:
	cp -p extract/src/extractor.ifl .

extractor.icl:
	cp -p extract/src/extractor.icl .

#  IFL to IFC
.ifl.ifc:
	@COMPIFL@ $<
