
noinst_LTLIBRARIES = libndfpack.la
noinst_DATA = ndfpack.hlp

libndfpack_la_SOURCES = $(F_ROUTINES) $(C_ROUTINES)

F_ROUTINES = axconv.f axlabel.f axunits.f erase.f fitsdin.f fitsexist.f \
fitsexp.f fitsimp.f fitslist.f fitsmod.f fitstext.f fitsval.f \
hiscom.f  hislist.f hisset.f native.f ndfcopy.f ndftrace.f setaxis.f setbad.f \
setbb.f setbound.f setext.f setlabel.f setnorm.f setorigin.f setsky.f   \
settitle.f settype.f setunits.f setvar.f wcsadd.f wcsattrib.f wcscopy.f \
wcsframe.f wcsremove.f wcsshow.f chain.f reshape.f showqual.f setqual.f \
remqual.f qualtobad.f ndfcompress.f provshow.f provadd.f provmod.f \
provrem.f ndfecho.f

C_ROUTINES = ndfcompare.c

# Create help for this library
ndfpack.hlp: $(F_ROUTINES) $(C_ROUTINES)
	-rm -f $@  $@.1 $@.2
	cat $(F_ROUTINES) > all.f
	cat $(C_ROUTINES) > all.c
	PATH="$(PATH):$(STARLINK)/bin/sst"; \
	export PATH ; \
	@PROHLP@ in=all.f out=$@.1 single=no ; \
	@PROHLP@ in=all.c out=$@.2 single=no
	-rm all.f all.c
	cat $@.1 $@.2 > $@

CLEANFILES = ndfpack.hlp

