## Process this file with automake to produce Makefile.in
AM_FCFLAGS = -I../

noinst_LTLIBRARIES = libchart.la
libchart_la_SOURCES = $(F_ROUTINES) $(FPP_ROUTINES) $(PRIVATE_INCLUDES)

FPP_ROUTINES = fileopen.F

F_ROUTINES = catinf.f convdec.f ellipse.f hdng.f near.f page.f prt26.f \
set.f subbsrch.f titles.f catrec.f conv.f finput.f ident.f nnint.f     \
palp.f putparams.f setup.f sublisto.f today.f catsel.f convpos.f       \
found.f iidate.f nssymb.f plotclose.f quadt.f sort.f subplot.f	       \
trulen.f convra.f getastrom.f leng.f number.f plotgrid.f       \
ramk.f sparin.f subrgs.f ttyhdng.f chartsub.f coord.f getdeflt.f       \
lpo1m.f numb.f pparin.f restore.f spot.f subsrch.f ttyhold.f chkcls.f  \
cross.f getparams.f ltoi.f objplt.f prcol.f rounup.f squ.f tconv.f     \
ttyout.f con_factor.f decmk.f gsout.f magns.f offs.f preces.f sbar.f   \
starp.f telcor.f ttypalp.f const.f delay.f guid13.f merz26.f output.f  \
proj.f select.f store.f tiltx.f val.f

PRIVATE_INCLUDES = SPT_CMN CONVF CATINF_CMN FILENAMES

BUILT_SOURCES = FILENAMES

# Tweak the path to the catalogue files

edit = sed \
	-e 's,@staretcdir\@,$(staretcdir),g'

FILENAMES: FILENAMES.in
	rm -f FILENAMES.tmp FILENAMES
	$(edit) \
	 -e 's,@edited_input\@,FILENAMES: produced from FILENAMES.in by Makefile.am,' \
	 $(srcdir)/FILENAMES.in > FILENAMES.tmp
	mv FILENAMES.tmp FILENAMES

