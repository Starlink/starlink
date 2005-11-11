# E.S.O. - VLT project / ESO Archive
# "@(#) $Id: Makefile,v 1.2 2005/02/02 01:43:02 brighton Exp $"
#
# who             when      what
# --------------  --------  ----------------------------------------
# Allan Brighton  05/12/95  Created
# pbiereic        26/08/99  Adapted for VLT SW OCT99 release
# pbiereic        25/10/02  added USE_CFLAGS.
# pbiereic        17/02/03  added vltMakeLogInstallation (SPR)
#                           
#
# This Makefile is used to configure and make all of the modules in 
# the parent directories (rtd, tclutil, astrotcl)
# It conforms to the VLT standards which expects 
# <module>/src/Makefile to define the targets: "all", "clean",
# "install" and "man".
#
# The default installation root directory is configured in the following order:
#
# 1)		PREFIX
# 2)		if PREFIX is not defined take INTROOT 
# 3)		if PREFIX and INTROOT are not defined take VLTROOT
# 4)            if none of the above are define use PREFIX=/usr/local as default
#
# The installation root directory can be redefined later either by setting
# the environment variable INSTALL_TARGET before running 'make install'
# or by running 'make install PREFIX='
#
# All the above variables must contain absolute path names.
#

# default installation root directory
DEFAULT_PREFIX="/usr/local"

# default configure flags ### --with-nosstream
CONFIGURE_FLAGS=--with-gcc --enable-shared

MAKEFLAGS=
VLT_INSTALL_LOG=

# additional configure flags (set to --with-debug to get a non-optimized compiled version)
ADD_CONFIGURE_FLAGS=

# additional _compiler_ flags (e.g. cflags=-fpermissive) ### -Wno-deprecated
cflags=

# ------- end of definitions --------------

USE_CONFIG=$(CONFIGURE_FLAGS) $(ADD_CONFIGURE_FLAGS)

ifneq (Z,Z$(cflags))
	USE_CFLAGS="cflags=$(cflags)"
endif

# default PREFIX for VLT software
ifndef PREFIX
    ifdef INTROOT
	PREFIX = ${INTROOT}
    endif
    ifndef PREFIX
        ifdef VLTROOT
            PREFIX = ${VLTROOT}
        endif
    endif
endif

ifndef PREFIX
    USE_PREFIX=$(DEFAULT_PREFIX)
else
    USE_PREFIX=$(PREFIX)
    VLT_INSTALL_LOG=vltMakeLogInstallation $(PREFIX)
endif

all: init_config config
	@(cd ..; $(MAKE) all)

# the VLT software archive removes some file attributes
# which need to be re-installed. config.cache must be
# deleted before running configure.
init_config:
	@(cd ..; \
	chmod -R +w * 2> /dev/null; \
	chmod +x configure; \
	rm -f config.cache ; \
	find . -name install.sh -exec chmod +x {} \; )

config:
	(cd ..; \
	if test -f $(GNU_ROOT)/lib/libg++.a ; then \
	    set -x ; ./configure -prefix $(USE_PREFIX) $(USE_CONFIG) ; \
	else \
	    set -x ; ./configure -prefix $(USE_PREFIX) $(USE_CONFIG) --with-vltgnu $(USE_CFLAGS); \
	fi)

clean: 
	@(if test -f ../Makefile ; then \
	  cd ..; $(MAKE) clean distclean; rm -fr `find . -type l`; rm -f Makefile config.cache; \
	fi)
	@echo "clean done..."

install:
	if [ ! -z "$(PREFIX)" ] ; then \
	    INSTALL_TARGET="$(PREFIX)" ; \
	    export INSTALL_TARGET ; \
	fi ; \
	(cd ..; $(MAKE) install) ; \
	if [ ! -z "$(VLT_INSTALL_LOG)" ] ; then \
	    eval $(VLT_INSTALL_LOG) ; \
	fi

cleanINTROOT:
	rm -fr ${INTROOT}/lib/rtd/ ${INTROOT}/lib/tclutil/ ${INTROOT}/lib/astrotcl/
	rm -fr ${INTROOT}/lib/rtd*.sh ${INTROOT}/lib/tclutil*.sh ${INTROOT}/lib/astrotcl*.sh
	rm -fr ${INTROOT}/lib/librtd*.* ${INTROOT}/lib/libtclutil.* ${INTROOT}/lib/libastrotcl.*
	rm -fr ${INTROOT}/bin/rtd ${INTROOT}/bin/rtdServer
	rm -fr ${INTROOT}/bin/rtdimage_wish ${INTROOT}/bin/astrotcl_wish ${INTROOT}/bin/tclutil_wish
	rm -fr ${INTROOT}/bin/tRtd
	rm -fr ${INTROOT}/include/rtd/ ${INTROOT}/include/tclutil/ ${INTROOT}/include/astrotcl/
	rm -fr ${INTROOT}/man/mann/rtd*
	rm -fr ${INTROOT}/man/man1/rtd*
	rm -fr ${INTROOT}/man/man3/rtd* ${INTROOT}/man/man3/Rtd* ${INTROOT}/man/man3/Image*
	rm -fr ${INTROOT}/man/man3/ColorMapInfo* ${INTROOT}/man/man3/ITTInfo*

man:
	-(cd ..; $(MAKE) man)

