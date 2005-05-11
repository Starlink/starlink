## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

dist_bin_SCRIPTS = findcoords
dist_starnews_DATA = findcoords.news

# The findcoords script needs to have installation locations edited into it
# Can not be done during configure since @bindir@ gets converted
# to ${exec_prefix}/bin

edit = sed \
	-e 's,@PERL\@,$(PERL),g' \
	-e 's,@bindir\@,$(bindir),g'

findcoords: findcoords.in
	rm -f findcoords.tmp findcoords
	$(edit) \
	   -e 's,@edited_input\@,findcoords: produced from findcoords.in by Makefile.am,' \
	 $(srcdir)/findcoords.in > findcoords.tmp
	mv findcoords.tmp findcoords
	chmod +x findcoords

EXTRA_DIST = findcoords.in

CLEANFILES = findcoords
