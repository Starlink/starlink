## Process this file with automake to produce Makefile.in

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

dist_bin_SCRIPTS = docfind
dist_starnews_DATA = docfind.news

# The coco script needs to have installation locations edited into it
# Can not be done during configure since @bindir@ gets converted
# to ${exec_prefix}/bin

edit = sed \
	-e 's,@prefix\@,$(prefix),g'

docfind: docfind.in
	rm -f docfind.tmp docfind
	$(edit) \
	   -e 's,@edited_input\@,docfind: produced from docfind.in by Makefile.am,' \
	 $(srcdir)/docfind.in > docfind.tmp
	mv docfind.tmp docfind
	chmod +x docfind

EXTRA_DIST = docfind.in

CLEANFILES = docfind
