# Makefile for ssn70 - documentation


TOP=$$PWD/../..

TARBALLS=ssn70.htx_tar ssn70.texml_tar ssn70.tex_tar

# all: ssn70.htx_tar ssn70.texml_tar

ssn70.htx_tar: ssn70.sgml ReleaseNotes \
		../dtd2sgml/sgmltext.stamp ../../misc/misc.stamp tmp-sgml2docs
	SGML_CATALOG_FILES=$$PWD/../documents/CATALOG:$(TOP)/dtd/CATALOG \
		BIBINPUTS=$$PWD \
		BSTINPUTS=$(TOP)/lib \
		./tmp-sgml2docs --html --tarball=$@ ssn70.sgml

# Doesn't work, unless you have a jade patched to include the latex output type
#ssn70.tex_tar: ssn70.sgml ReleaseNotes \
#		../dtd2sgml/sgmltext.stamp ../../misc/misc.stamp tmp-sgml2docs
#	SGML_CATALOG_FILES=$$PWD/../documents/CATALOG:$(TOP)/dtd/CATALOG \
#		BIBINPUTS=$$PWD \
#		BSTINPUTS=$(TOP)/lib \
#		./tmp-sgml2docs --latex --tarball=$@ ssn70.sgml

ssn70.texml_tar: ssn70.sgml ReleaseNotes \
		../dtd2sgml/sgmltext.stamp ../../misc/misc.stamp tmp-sgml2docs
	SGML_CATALOG_FILES=$$PWD/../documents/CATALOG:$(TOP)/dtd/CATALOG \
		BIBINPUTS=$$PWD \
		BSTINPUTS=$(TOP)/lib \
		TEXMLDIR=$(TOP)/lib/texml/bin \
		./tmp-sgml2docs --texml --tarball=$@ ssn70.sgml

../../misc/misc.stamp:
	cd ../../misc; $(MAKE) misc.stamp
../dtd2sgml/sgmltext.stamp:
	cd ../dtd2sgml; $(MAKE) sgmltext.stamp
# sgml2docs has an installation location edited into it at build time.
# Even if that has happened at this stage in the build, the kit
# hasn't been installed, so it won't be correct.  Make a temporary
# version of the script, just for this directory.
tmp-sgml2docs: ../../bin/sgml2docs.x
	sed -e 's,!!pkglibdir!!,'`cd $(TOP);pwd`,g ../../bin/sgml2docs.x >$@
	chmod +x $@

maintainer-clean: distclean
	rm -f tmp-sgml2docs
	rm -f $(TARBALLS)
distclean clean:
