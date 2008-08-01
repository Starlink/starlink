
## Before we can do anything in ../gaia and ../gaia3d we need to run configure
## in them. This needs to be deferred because we have a configure dependency
## on skycat (and related), which I don't want to add to the make
## configure-deps stage (that should be kept as lightweight as possible),
## hence this file and directory to act as a proxy for that dependency (we use
## a sub-directory as that's the easiest way to make sure this is executed
## before entering the gaia, gaia3d & gaiavo directories).

prefix=@prefix@
MANIFESTS=$(prefix)/manifests

## Where to find Starlink libraries and headers.
STARLINK=@STARLINK@

## Configure arguments, usually "--disable-shared", if set.
CONFIGURE_ARGS=@CONFIGURE_ARGS@

## Fake dependency to make things happen.
nodist_bin_SCRIPTS = ../gaia/Makefile ../gaia3d/Makefile \
                     ../gaiavo/Makefile ../gaia/Makefile

../gaia/Makefile: $(MANIFESTS)/skycat ../gaia/Makefile.in
	echo "gaia dependency triggered"
	cd ../gaia && \
	   ./configure --prefix=$(prefix) $(CONFIGURE_ARGS) \
	               --with-starlink=$(STARLINK) \
	               --cache-file=../config.cache

../gaia3d/Makefile: $(MANIFESTS)/skycat ../gaia3d/Makefile.in
	echo "gaia3d dependency triggered"
	cd ../gaia3d && \
	   ./configure --prefix=$(prefix) $(CONFIGURE_ARGS) \
	               --with-starlink=$(STARLINK) \
	               --cache-file=../config.cache

../gaiavo/Makefile: $(MANIFESTS)/skycat ../gaiavo/Makefile.in
	echo "gaiavo dependency triggered"
	cd ../gaiavo && \
	./configure --prefix=$(prefix) $(CONFIGURE_ARGS) \
	            --with-starlink=$(STARLINK) \
	            --cache-file=../config.cache

$(MANIFESTS)/skycat:
	echo "You must build and install Skycat"
	exit 1
