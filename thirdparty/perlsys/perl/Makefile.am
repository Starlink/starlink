## Process this file with automake to produce Makefile.in

@SET_MAKE@

RECURSIVE_TARGETS = all

$(RECURSIVE_TARGETS):
	( cd perl; $(MAKE) $@; )

##  Automake really requires local versions for these targets.
distclean-local:
	( cd perl; $(MAKE) distclean )

clean-local:
	( cd perl; $(MAKE) clean )

##  Check maps to test.
check-local:
	( cd perl; $(MAKE) test )

EXTRA_DIST = perl

##  Special install hook. The "perl" command is softlinked by "starperl" in
##  the main bin/ directory. This avoids having our Perl picked up by
##  thirdparties. Note this is ran by the STAR_SPECIAL_INSTALL_COMMAND
##  (so no manifest handling is required).
install-starperl:
	- $(mkdir_p) $(DESTDIR)$(bindir)
	cd $(DESTDIR)$(bindir) ;\
	rm -f starperldoc ;\
	$(LN_S) ../Perl/bin/perldoc starperldoc ;\
	rm -f starperl ;\
	$(LN_S) ../Perl/bin/perl starperl

##  Before distributing subdirectories we need to clean then up (we get all
##  the files in these directories).
dist-hook:
	rm -rf `find $(distdir) -type l`
	(cd $(distdir)/perl; make distclean)
