# Main Makefile for SExtractor
# Copyright (C) 2002,2003 Emmanuel Bertin.
AUTOMAKE_OPTIONS	= foreign no-dependencies
SUBDIRS			= man src
EXTRA_DIST		= config doc AUTHORS BUGS ChangeLog COPYING HISTORY \
			  INSTALL README THANKS \
			  $(PACKAGE).spec.in $(PACKAGE)-tpx.spec.in \
			  man/sex.1.in \
			  acx_pthread.m4 acx_prog_cc_optim.m4
RPM_ROOTDIR		= /usr/src/RPM
RPM_SRCDIR		= $(RPM_ROOTDIR)/SOURCES
RPM_ARCH		= i586
RPM_RPMDIR		= $(RPM_ROOTDIR)/RPMS/$(RPM_ARCH)
rpm:	dist
	cp -f $(PACKAGE)-$(VERSION).tar.gz $(RPM_SRCDIR)
	rpmbuild -bb --clean $(PACKAGE).spec

rpm-tpx:	dist
	cp -f $(PACKAGE)-$(VERSION).tar.gz $(RPM_SRCDIR)
	rpmbuild -bb --clean $(PACKAGE).spec $(PACKAGE)-tpx.spec

