## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all clean distclean

$(RECURSIVE_TARGETS):
	for d in @itcl_subdirs@; \
        do \
           ( cd $$d; $(MAKE) $@; ) \
        done

EXTRA_DIST = makefile.bc makefile.vc make.config.in $(subdirs)

dist_pkgdata_DATA = license.terms INCOMPATIBLE CHANGES

##  Need all the source code in the subdirectories for a distribution.
subdirs = config doc itcl itk iwidgets tclconfig tools

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the softlinks.
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/itcl; make distclean)
	(cd $(distdir)/itk; make distclean)
	(cd $(distdir)/iwidgets; make distclean)
