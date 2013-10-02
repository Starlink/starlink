## Process this file with automake to produce Makefile.in

bin_SCRIPTS = star2html xlabel latex2html

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Template data files.
# And we do want to distribute them (not done for data files by default).
dist_data_DATA = sun.tex ssn.tex sgp.tex sg.tex sc.tex sst.tex

# Distribute all the .in scripts.
EXTRA_DIST = star2html.in latex2html.in xlabel.in

sun199.htx_tar: sun199.tex star2html latex2html
	./star2html sun199.tex
	tar cf sun199.htx_tar sun199.htx

# The latex2html and star2html files contain references to the installation
# directories, and so must be substituted here at make time, rather
# than by ./configure at configuration time.  See section
# `Installation Directory Variables' in the autoconf manual for
# discussion.

# Directory with the real latex2html script.
l2hdir=$(DESTDIR)$(datadir)/latex2html/bin/

edit=sed \
    -e 's,@ENV\@,$(ENV),g' \
    -e 's,@PDFTOPS\@,$(PDFTOPS),g' \
    -e 's,@PDF2PS\@,$(PDF2PS),g' \
    -e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g' \
    -e 's,@PACKAGE_BUGREPORT\@,$(PACKAGE_BUGREPORT),g' \
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
