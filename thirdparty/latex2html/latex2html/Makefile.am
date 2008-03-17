## Process this file with automake to produce Makefile.in

@SET_MAKE@
RECURSIVE_TARGETS = all clean distclean

$(RECURSIVE_TARGETS):
	cd latex2html && $(MAKE) $@

##  Need all the source code in the subdirectories for a distribution.
EXTRA_DIST = $(subdirs)

subdirs = latex2html

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories). Also remove the SVN trace and any
##  softlinks.
dist-hook:
	rm -rf `find $(distdir) -type d -name .svn`
	rm -rf `find $(distdir) -type l`
	cd $(distdir)/latex2html && make distclean
