
#  Note that the raw error message file contains additional errors
#  [2100 - 2104] for the "mingks" utilities' dummy routines to use.

# Need simple version number
RLSE = $(PACKAGE_VERSION_MAJOR).$(PACKAGE_VERSION_MINOR)

## Makefile simply needs to build the font database file
noinst_PROGRAMS = gksemf

gksemf_SOURCES = gksemf.f
gksemf_LDADD = ../../base/libgks_base.la

# Create the font database from the ascii font files

gks.emf-$(RLSE): gksemf gksemf.dat
	./gksemf
	mv gks.emf gks.emf-$(RLSE)


