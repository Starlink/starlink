## Process this file with automake to produce Makefile.in

bin_SCRIPTS = rv xrv

bin_PROGRAMS = rv.x

rv_x_SOURCES = input.f  ktest.f  opr.f  opw.f  rvmain.f  rvml.f  tran.f

rv_x_LDADD = `sla_link`


# The xrv script needs to have installation locations edited into it
# Can not be done during configure since @bindir@ gets converted
# to ${exec_prefix}/bin
# Note that xrv looks for the expectk binary in
# $STARCONF_DEFAULT_STARLINK whereas it expects rv to be
# in $STARCONF_DEFAULT_PREFIX. (ie $STARLINK vs $INSTALL).
# For now, assume everything is in $INSTALL.

# This really needs to happen at install time

edit = sed \
	-e 's,@bindir\@,$(bindir),g' \
	-e 's,@starbindir\@,$(bindir),g'

xrv: xrv.in
	rm -f xrv.tmp xrv
	$(edit) \
	 -e 's,@edited_input\@,xrv: produced from xrv.in by Makefile.am,' \
	 $(srcdir)/xrv.in > xrv.tmp
	mv xrv.tmp xrv
	chmod +x xrv
