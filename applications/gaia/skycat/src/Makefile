# E.S.O. - VLT project / ESO Archive
# "@(#) $Id: Makefile,v 1.11 1998/09/30 16:40:15 abrighto Exp $"
#
# Makefile 
#
# This Makefile can be used to configure and make all of the modules in 
# the parent directory (rtd, cat, skycat, ...).
# It is included here to conform to a VLT standard which expects 
# <module>/src/Makefile to define the targets: "all", "clean",
# "install" and "man".
#

# default prefix of install dir
PREFIX=/usr/local

# additional configure flags
CONFIGURE_FLAGS=--with-gcc --enable-shared

# configure and make
all: 
	(cd ..; configure -prefix $(PREFIX) $(CONFIGURE_FLAGS); make all)

clean: 
	-(cd ..; $(MAKE) clean distclean)

install:
	(cd ..; $(MAKE) install)

man:
	(cd ..; $(MAKE) man)


