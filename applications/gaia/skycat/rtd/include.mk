# E.S.O. - VLT project/ESO Archive
#
# "@(#) $Id: include.mk,v 1.3 1997/04/11 10:47:19 abrighto Exp $" 
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
	    test -d $$i || mkdir $$i ; \
        done

clean:

distclean: clean
	rm -f Makefile	

depend:

man:

