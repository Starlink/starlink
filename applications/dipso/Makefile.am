## Process this file with automake to produce Makefile.in

bin_PROGRAMS = dipso

dipso_SOURCES = run_dipso.c

# C main so we need FCLIBS
dipso_LDADD = libdipso.la `$(srcdir)/dipso_link` @FCLIBS@
dipso_LDFLAGS =

nodist_bin_SCRIPTS = dipsosetup dipso_link my_dipso dipsosetup.sh

dist_bin_SCRIPTS = dipsotest

lib_LTLIBRARIES = libdipso.la

include_HEADERS = $(PUBLIC_INCLUDES)

libdipso_la_SOURCES = $(F_ROUTINES) $(FPP_ROUTINES) $(C_ROUTINES) $(BLOCK_DATA_FILES)

F_ROUTINES = abload.f abscal.f acheck.f arb.f arith.f arrow.f arysrt.f \
 asterix.f atfit.f atlasrd.f axmat.f badchk.f bbint.f bbody.f calcfx.f \
 cdraw.f cheatsm.f cmatch.f comnds.f convol.f cpair.f ctrlc_ast.f dacfit.f \
 dashit.f dbound.f decimate.f decode.f decrng.f deftyp.f dela.f dered.f \
 detev.f dipso.f dparse.f dtoupp.f elfbps.f elfclr.f elfdep.f \
 elfg.f elfinv.f elfit.f elfkey.f elfkj.f elfparea.f elfpflx.f elfpl.f \
 elfpnt.f elfset.f elfsfun.f elfstor.f elfvar.f elfview.f endmsk.f \
 ermess.f even.f ewmeas.f expand.f expar.f explabel.f fcaller.f fcdel.f \
 fcpop.f fcrest.f fcsave.f fcsl.f filter.f findit.f flfix.f fneb.f fnum.f \
 fouri.f fourier.f fpget.f fpush.f fword.f gammln.f gammode.f gauss.f \
 gcfmode.f get0c.f get0i.f get0l.f get0r.f getcho.f getcom.f getcom2.f \
 getdir.f gtenv.f getinp.f getndf.f getrng.f getstk.f gsermode.f gtslot.f \
 handler.f heapsrt.f help1.f ihmmnt.f ihmrg.f ihrot.f ihrstr.f ihsmth.f \
 ihsval.f ihubv.f incxy.f integrate.f intep.f ipset.f iruser.f \
 isatm.f isbach.f itochr.f iwrite.f jsymbol.f kdcode.f lagint.f lagint4.f \
 laguerre.f lalpha.f lblrun.f lenstr.f locdat.f lwrcor.f models.f modest.f \
 mongowr.f move_real.f msgout.f ndfnam.f nebcont.f nebset.f nextk.f \
 ocdraw.f options.f osp0rd.f osp0wr.f parse_env.f pdgfit.f perigram.f \
 pltarr.f pointsym.f poly.f ppalet.f prpage.f putext.f pwritit.f range.f \
 rdkybd.f rdndf.f rdstr.f read.f reca.f record.f remap.f \
 repfrm.f reporting.f rerep.f resint.f resstk.f restore.f reverx.f \
 rinteg.f rlint.f save.f savstk.f sdocont.f setcolours.f sgscurse.f \
 slen.f smatin.f smooth.f snip.f sofar.f sort1i.f sp0rd.f sp0wr.f sp1rd.f \
 sp2rd.f sp2wr.f spload.f srtbrk.f ssdraw.f sstrip.f star.f strrd.f \
 swcor2.f swpcor.f sysexe.f tau.f toreal.f update.f upush.f va04a.f \
 velsort.f voigt.f vpeak.f vsp0rd.f window.f wrcopy.f write.f write_ndf.f \
 wrndf.f wrprop.f wtstruser.f xcorre.f xdcode.f xhelp.f xsort.f xsprmpt.f \
 yendmsk.f yxn.f zanstra.f croper.f rdcat.f getcat.f gtctcl.f rdctd.f \
 user.f gks_gstat.f

C_ROUTINES = handlev.c rdkbd2.c

FPP_ROUTINES = readvmsrec.F

## Note that we cannot call this variable just BLOCK_DATA, since automake
## interprets this as a `primary' DATA
BLOCK_DATA_FILES = is_blk.f

## Create the historical link to libdipso.a in the bin directory.
install-data-local:
	if test ! -d $(DESTDIR)$(bindir); then \
	   mkdir $(DESTDIR)$(bindir) ;\
	fi
	(cd $(DESTDIR)$(bindir) && \
	rm -f libdipso.a ;\
	@LN_S@ `$(PERL) -MFile::Spec -e "print File::Spec->abs2rel('$(DESTDIR)$(libdir)/libdipso.a')"` .)

PUBLIC_INCLUDES = DECLARE_STKS DECLARE_LBLS DECLARE_PLTS DECLARE_DATA

## Historical include file names. Not implemented (could softlink these?).
OLD_PUBLIC_INCLUDES = declare_stks.f declare_lbls.f declare_plts.f \
 declare_data.f

PRIVATE_INCLUDES = DSB_CLR KARS_COM KUSE_COM

# The `dipsosetup' script needs to have installation locations edited into it
edit = sed \
        -e 's,@bindir\@,$(bindir),g' \
        -e 's,@prefix\@,$(prefix),g' \
        -e 's,@staretcdir\@,$(staretcdir),g'

dipsosetup: dipsosetup.in
	rm -f dipsosetup.tmp dipsosetup
	$(edit) \
	   -e 's,@edited_input\@,dipsosetup: produced from dipsosetup.in by Makefile.am,' \
	   $(srcdir)/dipsosetup.in >dipsosetup.tmp
	mv dipsosetup.tmp dipsosetup

dipsosetup.sh: dipsosetup.sh.in
	rm -f dipsosetup.sh.tmp dipsosetup.sh
	$(edit) \
	   -e 's,@edited_input\@,dipsosetup: produced from dipsosetup.in by Makefile.am,' \
	   $(srcdir)/dipsosetup.sh.in >dipsosetup.sh.tmp
	mv dipsosetup.sh.tmp dipsosetup.sh

## Files that should be copied into bindir.
dipsosupportdir = $(bindir)
dipsosupport_DATA = ATLIST.DAT demo2.cmd LWPLO.ABS user.f \
 ATOMIC.DAT DEMO2.sdf LWRLO.ABS SWPCOR.DAT command.hlp  \
 exercise.cmd SWPLO.ABS demo1.cmd dipso.hlp help.lis testspec2.cmd \
 DEMO1_STK.sdf NEBCONT.DAT updates.lis installtest.in

nodist_dipsosupport_DATA = run_dipso.o

## Data files for etc/dipso.
staretc_DATA = specdat/*.*

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

EXTRA_DIST = specdat $(PRIVATE_INCLUDES) dipsosetup.in my_dipso.in \
             $(dipsosupport_DATA) dipsosetup.sh.in

## News file

dist_starnews_DATA = dipso.news

## Simple installation test.

TESTS = installtest

installtest: installtest.in
	rm -f installtest.tmp installtest
	$(edit) \
	   $(srcdir)/installtest.in > installtest.tmp
	mv installtest.tmp installtest
	chmod 755 installtest

CLEANFILES = installtest
