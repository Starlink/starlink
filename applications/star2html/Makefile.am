## Process this file with automake to produce Makefile.in

bin_SCRIPTS = star2html xlabel latex2html

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Template data files.
# And we do want to distribute them (not done for data files by default).
dist_data_DATA = sun.tex ssn.tex sgp.tex sg.tex sc.tex sst.tex

# Don't define a l2h_DATA variable here -- we'll install the l2h files
# `by hand' below.
l2hdir=$(DESTDIR)$(datadir)/star2html-l2hsys

# Distribute the l2h directory and all the .in scripts.
EXTRA_DIST = l2h_source.tar star2html.in latex2html.in xlabel.in

# The tarball consists of the contents of the l2h directory, minus any
# CVS directories.
l2h_source.tar:
	rm -Rf l2h_tempdir
	mkdir l2h_tempdir
	cp -R l2h/current l2h/old l2h_tempdir
	cd l2h_tempdir; \
	    for d in `find . -type d -name CVS`; do rm -Rf $$d; done; \
	    tar cf ../l2h_source.tar *

# The l2h directory is required for building the local documentation. 
# When distributed we need to unpack the source tarball to get this back
# (normally part of CVS repository). Do not depend on source tarball as
# we do not want to create l2h if it exists (part of CVS checkout) or
# create the tarball if not doing a "dist".
l2h:
	rm -Rf l2h
	mkdir l2h
	cd l2h; \
	    tar xf ../l2h_source.tar

# Extra install hook.  Unpack the l2h_source.tar tarball into $(l2hdir).
install-data-local: l2h_source.tar
	rm -Rf $(l2hdir)
	$(mkdir_p) $(l2hdir)
	wd=`pwd`; cd $(l2hdir); tar xf $$wd/l2h_source.tar
	$(MANIFEST) && tar tf l2h_source.tar | sed 's,^,MANIFEST:$(l2hdir)/,'

sun199.htx_tar: sun199.tex star2html latex2html l2h
	env CURRENT_L2H=`pwd`/l2h/current PATH=`pwd`/"${PATH}" ./star2html sun199.tex
	tar cf sun199.htx_tar sun199.htx

# The latex2html and star2html files contain references to the installation
# directories, and so must be substituted here at make time, rather
# than by ./configure at configuration time.  See section
# `Installation Directory Variables' in the autoconf manual for
# discussion.
edit=sed \
    -e 's,@PERL\@,$(PERL),g' \
    -e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g' \
    -e 's,@PACKAGE_BUGREPORT\@,$(PACKAGE_BUGREPORT),g' \
    -e 's,@UUDECODE\@,$(UUDECODE),g' \
    -e 's,@UNCOMPRESS\@,$(UNCOMPRESS),g' \
    -e 's,@l2hdir\@,$(l2hdir),g'
latex2html: Makefile $(srcdir)/latex2html.in
	rm -f latex2html latex2html.tmp
	$(edit) \
	    -e 's,@edited_input\@,latex2html. Generated from latex2html.in by Makefile.am,g' \
	    $(srcdir)/latex2html.in >latex2html.tmp
	chmod +x latex2html.tmp
	mv latex2html.tmp latex2html
star2html: Makefile $(srcdir)/star2html.in
	rm -f star2html star2html.tmp
	$(edit) \
	    -e 's,@edited_input\@,star2html. Generated from star2html.in by Makefile.am,g' \
	    $(srcdir)/star2html.in >star2html.tmp
	chmod +x star2html.tmp
	mv star2html.tmp star2html

CLEANFILES = latex2html star2html xlabel

dist_starnews_DATA = star2html.news


