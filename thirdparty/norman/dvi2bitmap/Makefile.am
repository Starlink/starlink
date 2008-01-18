## Process this file with automake to produce Makefile.in

@SET_MAKE@

MAIN_TARGET = @MAIN_TARGET@
MESA_SYSTEM = @MESA_SYSTEM@

#  We can't override target `all', but `all-local' is the approved hook.
all-local: $(MAIN_TARGET)

#  Build Mesa.
mesa-src:
	cd mesa && rm -f configs/current
	cd mesa && $(MAKE) $(MESA_SYSTEM)

#  Don't build Mesa.  Just create a stamp file.
mesa-stamp:
	rm -f $@
	{ date; \
	  echo "OpenGL found in system -- no need to build our own"; } >$@

#  Recursive targets, use local versions so that we get proper cleans too.
#  This means we must expand into explicit lines so that automake see them
#  (previously these three were a macro, plus target)
clean-local:
	- cd mesa && $(MAKE) clean

distclean-local:
	- cd mesa && $(MAKE) realclean

#  We need to distribute the source.

EXTRA_DIST = mesa

CLEANFILES = mesa-stamp

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove any softlinks and the
##  subversion trace.
dist-hook:
	rm -rf `find $(distdir) -type d -name .svn`
	rm -rf `find $(distdir) -type l`
	cd $(distdir)/mesa && make realclean

