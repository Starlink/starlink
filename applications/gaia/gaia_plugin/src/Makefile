# E.S.O. - VLT project / ESO Archive
# "@(#) $Id: Makefile,v 1.1 1998/04/06 23:55:42 abrighto Exp $"
#
# Makefile 
#
# This Makefile can be used to configure and make the entire package.
# It is included here to conform to a VLT standard which expects 
# <module>/src/Makefile to define the targets: "all", "clean",
# "install" and "man".
#

# default prefix of install dir
PREFIX=/usr/local

# additional configure flags
CONFIGURE_FLAGS="--with-gcc --enable-shared"

# configure and make
all: 
	(cd ..; PWD=`pwd`; export PWD; \
	 configure -prefix $(PREFIX) $(CONFIGURE_FLAGS); \
	 $(MAKE) all)

clean: 
	-(cd ..; $(MAKE) clean distclean)

install:
	(cd ..; $(MAKE) install)

man:
	(cd ..; $(MAKE) man)


