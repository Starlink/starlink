# Main Makefile for SExtractor
# Copyright (C) 2002-2005 Emmanuel Bertin.
AUTOMAKE_OPTIONS	= foreign no-dependencies
SUBDIRS			= man src
EXTRA_DIST		= config doc AUTHORS BUGS ChangeLog COPYING HISTORY \
			  INSTALL README THANKS \
			  acx_prog_cc_optim.m4
RPM_ROOTDIR		= /usr/src/RPM
RPM_SRCDIR		= $(RPM_ROOTDIR)/SOURCES
rpm:	dist
	cp -f $(PACKAGE_NAME)-$(PACKAGE_VERSION).tar.gz $(RPM_SRCDIR)
	rpmbuild -ba --clean $(PACKAGE_NAME).spec $(PACKAGE_NAME)-mp.spec

rpm-opteron:	dist
	cp -f $(PACKAGE_NAME)-$(PACKAGE_VERSION).tar.gz $(RPM_SRCDIR)
	GCCFLAGS="-O3 -g -funroll-loops -fomit-frame-pointer -Wall \
		-march=opteron" rpmbuild -ba --target=x86_64 \
		--clean $(PACKAGE_NAME).spec                                                             

rpm-athlon:	dist
	cp -f $(PACKAGE_NAME)-$(PACKAGE_VERSION).tar.gz $(RPM_SRCDIR)
	GCCFLAGS="-O3 -g -funroll-loops -fomit-frame-pointer -Wall -m32 \
		-march=i686 -msse -mfpmath=sse -mtune=athlon" rpmbuild \
		-ba --target=i686 --clean $(PACKAGE_NAME).spec

debug:
	$(MAKE) CFLAGS="-O3 -funroll-loops -fomit-frame-pointer -Wall -m32 -g"
