## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = clean distclean check

# We can't override target `all', but `all-local' is the approved hook.
all-local: $(MAIN_TARGET)

# Build jpeglib
jpeglib:
	cd src; $(MAKE) all

# Don't build jpeglib.  Just create a stamp file.
jpeglib-stamp:
	rm -f $@
	{ date; \
          echo "jpeglib.h found in system -- no need to build our own"; } >$@

$(RECURSIVE_TARGETS):
	cd src; $(MAKE) $@
