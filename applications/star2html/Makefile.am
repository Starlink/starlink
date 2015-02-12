## Process this file with automake to produce Makefile.in

bin_SCRIPTS = star2html xlabel

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Distribute all the .in scripts.
EXTRA_DIST = star2html.in xlabel.in

sun199.htx_tar: sun199.tex star2html
	./star2html sun199.tex
	tar cf sun199.htx_tar sun199.htx


edit=sed \
    -e 's,@PACKAGE_VERSION\@,$(PACKAGE_VERSION),g'

star2html: Makefile $(srcdir)/star2html.in
	rm -f star2html star2html.tmp
	$(edit) \
	    -e 's,@edited_input\@,star2html. Generated from star2html.in by Makefile.am,g' \
	    $(srcdir)/star2html.in >star2html.tmp
	chmod +x star2html.tmp
	mv star2html.tmp star2html

CLEANFILES = star2html xlabel

dist_starnews_DATA = star2html.news
