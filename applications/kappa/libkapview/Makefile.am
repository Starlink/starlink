
noinst_LTLIBRARIES = libkapview.la
noinst_DATA = kapview.hlp

libkapview_la_SOURCES = $(F_ROUTINES)

F_ROUTINES = ardplot.f clinplot.f colcomp.f contour.f cursor.f drawsig.f	      \
drawnorth.f display.f elprof.f gdclear.f gdnames.f gdset.f gdstate.f  \
linplot.f lutable.f lutsave.f lutview.f lutedit.f mlinplot.f paldef.f \
palentry.f palread.f palsave.f piccur.f picdef.f picempty.f	      \
picentire.f picin.f piclabel.f piclist.f picsel.f pictrans.f picvis.f \
scatter.f vecplot.f


# Create help for this library
kapview.hlp: $(F_ROUTINES)
	cat $(F_ROUTINES) > all.f
	-rm -f kapview.hlp
	PATH="$(PATH):$(STARLINK)/bin/sst"; export PATH; \
	@PROHLP@ in=all.f out=kapview.hlp single=no
	-rm all.f

CLEANFILES = kapview.hlp
