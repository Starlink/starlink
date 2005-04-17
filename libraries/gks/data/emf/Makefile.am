
#  Note that the raw error message file contains additional errors
#  [2100 - 2104] for the "mingks" utilities' dummy routines to use.

# Need simple version number
RLSE = $(PACKAGE_VERSION_MAJOR).$(PACKAGE_VERSION_MINOR)

## Makefile simply needs to build the font database file
noinst_PROGRAMS = gksemf

gksemf_SOURCES = gksemf.f
gksemf_LDADD = ../../base/libgks_base.la

staretc_DATA = gks.emf-$(RLSE)

# Create the font database from the ascii font files

gks.emf-$(RLSE): gksemf gksemf.dat
	rm -f gks.emf $@
	./gksemf
	mv gks.emf $@

CLEANFILES = gks.emf-$(RLSE)

EXTRA_DIST = gksemf.dat


