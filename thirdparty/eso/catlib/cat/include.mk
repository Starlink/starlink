# E.S.O. - VLT project/ESO Archive
#
# "@(#) $Id: include.mk,v 1.4 2001/08/27 10:10:28 abrighto Exp $" 
#
# Makefile.in
# 
# who      when     what
# -------- -------- ------------------
# abrighto 15/04/96 created
#
# ------------------------------------------------------------------------
# 	Make include file for include source directories
# ------------------------------------------------------------------------
#

all:

All:
	cd ../..; $(MAKE)

install: install-dirs
	@set -x; for i in $(SRCS); do \
		test -f "$$i" && $(INSTALL_DATA) $$i $(INCDIR) ;\
	done

install-dirs:
	@for i in $(TOPDIR) `dirname $(INCDIR)` $(INCDIR) ; do \
	    if test ! -d $$i ; then \
		mkdir $$i ; \
	    fi ; \
        done

clean:

distclean: clean
	rm -f Makefile	

depend:

man:

