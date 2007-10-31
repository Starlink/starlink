##  Process this file with automake to produce Makefile.in

@SET_MAKE@

##  First build CMake, then use that to configure VTK and finall build it.
##  Note need a softlink so that include files are located using <vtk/>.
all:
	( cd cmake-2.4.7 && $(MAKE) )
	( PATH=`pwd`/cmake-2.4.7/bin:${PATH}; export PATH; \
	  cd VTK && \
	  cmake -D BUILD_SHARED_LIBS:BOOL=ON \
	        -D BUILD_TESTING:BOOL=OFF \
	        -D CMAKE_INSTALL_PREFIX:PATH=$(prefix) \
	        -D VTK_USE_GUISUPPORT:BOOL=ON \
	        -D VTK_WRAP_TCL:BOOL=ON \
	        -D VTK_USE_X:BOOL=ON \
	        -D VTK_USE_CARBON:BOOL=OFF \
	        -D VTK_USE_COCOA:BOOL=OFF . && \
	  $(MAKE) )
	( cd $(prefix)/include && rm -rf vtk && $(LN_S) vtk-5.0 vtk )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd cmake-2.4.7; $(MAKE) distclean )
	( cd VTK; $(MAKE) distclean )

clean-local:
	( cd cmake-2.4.7; $(MAKE) clean )
	( cd VTK; $(MAKE) clean )

EXTRA_DIST =

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the CVS trace and any
##  softlinks.
dist-hook:
	rm -rf `find $(distdir) -type d -name CVS`
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/VTK; make distclean)
	(cd $(distdir)/cmake-2.4.7; make distclean)
