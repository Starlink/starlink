##  Process this file with automake to produce Makefile.in

@SET_MAKE@

##  First build CMake, then use that to configure VTK and finall build it.
##  Note need a softlink so that include files are located using <vtk/>
##  (this will need updating for new major versions of VTK).
##  On OS X we make sure that this is an X11 build, so require all the
##  various options below.
all:
	( cd cmake && $(MAKE) )
	( PATH=`pwd`/cmake/bin:${PATH}; export PATH; \
	  pwd=`pwd`; \
	  TCL_LIBRARY_PATH=$(prefix)/lib; export TCL_LIBRARY_PATH; \
	  cd VTK && \
	  cmake -D BUILD_SHARED_LIBS=ON \
	        -D BUILD_TESTING=OFF \
	        -D CMAKE_INSTALL_PREFIX=$(prefix) \
		-D CMAKE_INCLUDE_PATH=$(prefix)/include \
		-D CMAKE_LIBRARY_PATH=$(prefix)/lib \
	        -D VTK_USE_GUISUPPORT=ON \
	        -D VTK_WRAP_TCL=ON \
	        -D VTK_USE_X=ON \
	        -D VTK_USE_CARBON=OFF \
	        -D VTK_USE_COCOA=OFF . && \
	  $(MAKE) )
	( cd $(prefix)/include && rm -rf vtk && $(LN_S) vtk-5.0 vtk )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd cmake && $(MAKE) distclean )
	( cd VTK && $(MAKE) distclean )

clean-local:
	( cd cmake && $(MAKE) clean )
	( cd VTK && $(MAKE) clean )

EXTRA_DIST = cmake VTK

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove any softlinks. XXX .svn
##  traces?
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/VTK && make distclean)
	(cd $(distdir)/cmake && make distclean)
