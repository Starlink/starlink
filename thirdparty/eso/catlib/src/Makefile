# E.S.O. - VLT project / ESO Archive
# "@(#) $Id: Makefile,v 1.12 1999/12/15 14:41:54 abrighto Exp $"
#
# Makefile 
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
# who             when      what
# --------------  --------  ----------------------------------------
# Allan Brighton  05/12/95  Created
# pbiereic        26/08/99  Adapted for VLT SW OCT99 release

# default installation root directory
DEFAULT_PREFIX="/usr/local"

# default configure flags
CONFIGURE_FLAGS=--with-gcc --enable-shared

# additional configure flags (set to --with-debug to get a non-optimized compiled version)
ADD_CONFIGURE_FLAGS=

# additional _compiler_ flags
cflags=

# ------- end of definitions --------------

USE_CONFIG=$(CONFIGURE_FLAGS) $(ADD_CONFIGURE_FLAGS)

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
endif

all: init_config config
	(cd ..; $(MAKE) all)

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
	@(cd ..; \
	if test -f $(GNU_ROOT)/lib/libg++.a ; then \
	    set -x ; ./configure -prefix $(USE_PREFIX) $(USE_CONFIG) ; \
	else \
	    set -x ; ./configure -prefix $(USE_PREFIX) $(USE_CONFIG) --with-vltgnu ; \
	fi)

clean: 
	-@(cd ..; $(MAKE) clean distclean; sh rmlinks; rm -f Makefile config.cache)

install:
	if [ ! -z "$(PREFIX)" ] ; then \
	    INSTALL_TARGET="$(PREFIX)" ; \
	    export INSTALL_TARGET ; \
	fi ; \
	(cd ..; $(MAKE) install)

man:
	-(cd ..; $(MAKE) man)

