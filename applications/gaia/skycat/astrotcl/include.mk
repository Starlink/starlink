# E.S.O. - VLT project/ESO Archive
#
# "@(#) $Id: include.mk,v 1.2 2005/02/02 01:43:04 brighton Exp $" 
#
# Makefile.in
# 
# who      when     what
# -------- -------- ------------------
# abrighto 15/04/96 created
# pbiereic 28/03/02 install optional includes only if VLTROOT is not defined
#
# ------------------------------------------------------------------------
# 	Make include file for include source directories
# ------------------------------------------------------------------------
#

all:

All:
	cd ../..; $(MAKE)

install: install-dirs
	@for i in $(SRCS); do \
		test -f "$$i" && $(INSTALL_DATA) $$i $(INCDIR) ;\
	done
	@if test -z "$(VLTROOT)" && test -n "$(SRCS_OPTL)"; then \
		for i in "" $(SRCS_OPTL); do \
			test -n "$$i" && test -f "$$i" && $(INSTALL_DATA) $$i $(INCDIR) ;\
		done \
	fi

install-dirs:
	@for i in $(TOPDIR) `dirname $(INCDIR)` $(INCDIR) ; do \
	    test -d $$i || mkdir $$i ; \
        done

clean:

distclean: clean
	rm -f Makefile  

depend:

man:
