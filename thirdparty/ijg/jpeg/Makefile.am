## Process this file with automake to produce Makefile.in

@SET_MAKE@

# We can't override target `all', but `all-local' is the approved hook.
all-local: $(MAIN_TARGET)

# Build jpeglib
jpeglib:
	cd jpeg; $(MAKE) all

# Don't build jpeglib.  Just create a stamp file.
jpeglib-stamp:
	rm -f $@
	{ date; \
          echo "jpeglib.h found in system -- no need to build our own"; } >$@

# Recursive targets, use local versions so that we get proper cleans too.
# This means we must expand into explicit lines so that automake see them
# (previously these three were a macro, plus target)
clean-local:
	- cd jpeg; $(MAKE) clean

distclean-local:
	- cd jpeg; $(MAKE) distclean

check:
	- cd jpeg; $(MAKE) check

#We need to distribute the source.

EXTRA_DIST = jpeg

CLEANFILES = jpeglib-stamp
