## Process this file with automake to produce Makefile.in

bin_SCRIPTS = rv xrv

bin_PROGRAMS = rv.x

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

rv_x_SOURCES = input.f  ktest.f  opr.F  opw.f  rvmain.f  rvml.f  tran.f

rv_x_LDADD = `sla_link`


# The xrv script needs to have installation locations edited into it
# Can not be done during configure since @bindir@ gets converted
# to ${exec_prefix}/bin
# expectk is picked up from the path using STAR_CHECK_PROGS
# Note that this may be a different location to where xrv is installed.
# This really needs to happen at install time

edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@EXPECTK\@,$(EXPECTK),g' \
	-e 's,@starbindir\@,$(bindir),g'

xrv: xrv.in
	rm -f xrv.tmp xrv
	$(edit) \
	 -e 's,@edited_input\@,xrv: produced from xrv.in by Makefile.am,' \
	 $(srcdir)/xrv.in > xrv.tmp
	mv xrv.tmp xrv
	chmod +x xrv

dist_starnews_DATA = rv.news

EXTRA_DIST = rv xrv.in

CLEANFILES = xrv

dist_pkgdata_DATA = read.me



