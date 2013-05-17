## Process this file with automake to produce Makefile.in

SUBDIRS = examples data

LN_S=@LN_S@

# Build the actual libraries

lib_LTLIBRARIES = libncar.la \
                  libncar_conrecqck.la \
		  libncar_conrecsupr.la \
		  libncar_dashline.la \
		  libncar_dashsmth.la \
		  libncar_dashsupr.la \
		  libncar_conras.la \
		  libncar_conpms.la \
		  libncar_user.la \
		  libncar_snx.la

dist_bin_SCRIPTS = ncar_link ncar_link_adam

# This is the standalone version that can be used when SNX is not in use
libncar_la_SOURCES = $(F_ROUTINES) $(NONSNX_F_ROUTINES) $(BLOCKDATA_ROUTINES) $(FPP_ROUTINES)

# This version of the library should be used for SNX since it contains the
# SNX override functions, along with those that access the NCAR common-blocks.
# Do not link this and libncar at the same time
libncar_snx_la_SOURCES = $(F_ROUTINES) $(SNX_F_ROUTINES) $(BLOCKDATA_ROUTINES) $(FPP_ROUTINES)

libncar_snx_la_LIBADD = libncar_user.la

libncar_conrecqck_la_SOURCES = conrecqck.f
libncar_conrecsupr_la_SOURCES = conrecsupr.f
libncar_dashline_la_SOURCES = dashline.f
libncar_dashsmth_la_SOURCES = dashsmth.f
libncar_dashsupr_la_SOURCES = dashsupr.f
libncar_conras_la_SOURCES = conras.f
libncar_conpms_la_SOURCES = conpms.f

# Library of the user replaceable functions, dummy functions. Currently if you
# don't use this you need to supply all the named functions.
libncar_user_la_SOURCES = $(USER_F_ROUTINES)

# Make all library code position independent.
if !NOPIC
libncar_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libncar_snx_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libncar_conrecqck_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libncar_conrecsupr_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libncar_dashline_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libncar_dashsmth_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libncar_dashsupr_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libncar_conras_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libncar_conpms_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libncar_user_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
endif

# Install the data files
staretc_DATA = ezmap.dat pwritx.dat

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

# Source files

# Routines overridden by SNX equivalents.
NONSNX_F_ROUTINES = agpwrt.f agchnl.f

# SNX routines that match above and that access the SNX common blocks
# (moved here as that is not allowed for shareable libraries).
SNX_F_ROUTINES = snx_agchnlz.f snx_agpwritx.f snx_agcs.f snx_agguy.f \
snx_agsav.f snx_agugy.f snx_aggux.f snx_agres.f snx_agugx.f snx_chset.f

# NCAR routines that can be replaced with user versions. Note need
# replacements for all these when -user linking flag is used.
USER_F_ROUTINES = agchax.f agchcu.f agchil.f

F_ROUTINES = agaxis.f agback.f agbnch.f  \
agctcs.f agctko.f agcurv.f agdash.f agdflt.f agdlch.f agdshn.f \
agexax.f agexus.f agezsu.f agfpbn.f agftol.f aggetc.f aggetf.f aggeti.f \
aggetp.f aggtch.f aginit.f agkurv.f aglbls.f agmaxi.f agmini.f agnumb.f \
agppid.f agqurv.f agrpch.f agrstr.f agsave.f agscan.f agsetc.f \
agsetf.f agseti.f agsetp.f agsrch.f agstch.f agstup.f agutol.f anotat.f \
bound.f calcnt.f ccheck.f cfux.f cfuy.f cfvld.f chstr.f clgen.f clset.f \
clsgks.f cmfx.f cmfy.f cmux.f cmuy.f concal.f concld.f concls.f \
concom.f condet.f condrw.f condsd.f conecd.f congen.f conint.f conlcm.f \
conlin.f conloc.f conlod.f conop1.f conop2.f conop3.f conop4.f conot2.f \
conout.f conpdv.f conpmm.f conran.f conraq.f conrec.f \
conreo.f consld.f conssd.f constp.f contlk.f contng.f contor.f \
conxch.f cpfx.f cpfy.f cpux.f cpuy.f ctcell.f cufx.f cufy.f curve.f \
curve3.f curved.f dandr.f dashdb.f dashdc.f dcheck.f displa.f drawi.f \
drawpv.f draws.f drawt.f drcntr.f drline.f drwvec.f e9rin.f encd.f \
entsr.f eprin.f errof.f expand.f ezcntr.f ezhftn.f ezisos.f ezmxy.f \
ezmy.f ezsrfc.f ezvec.f ezxy.f ezy.f fdum.f fence3.f fillin.f fl2int.f \
frame.f frst3.f frstc.f frstd.f frstpt.f frsts.f gbytes.f gethol.f \
getset.f getsi.f getusv.f gray.f grid.f gridal.f gridl.f \
gtdgts.f gtnum.f gtnumb.f gtsign.f hafton.f halfax.f hst1.f hstbkd.f \
hstexp.f hstgrm.f hstlst.f hstmed.f hstopc.f hstopl.f hstopr.f hststr.f \
htable.f i1mach.f i8sav.f idictl.f idiot.f init3d.f initzi.f initzs.f \
initzt.f intzi.f intzs.f intzt.f isosrf.f kfmx.f kfmy.f kfpx.f kfpy.f \
kmpx.f kmpy.f kpmx.f kpmy.f kumx.f kumy.f kupx.f kupy.f kurv1s.f \
kurv2s.f labmod.f lastd.f line.f line3.f line3w.f lined.f \
mapcem.f mapchi.f mapdrw.f mapeos.f mapfst.f mapgrd.f mapgtc.f mapgti.f \
mapgtl.f mapgtr.f mapio.f mapiq.f mapit.f maplbl.f maplmb.f maplot.f \
mappos.f maproj.f maprs.f maprst.f mapsav.f mapset.f mapstc.f mapsti.f \
mapstl.f mapstr.f maptre.f maptrn.f maptrp.f mapusr.f mapvec.f mapvp.f \
minmax.f mkmsk.f mmask.f mxmy.f nerro.f opngks.f perim.f \
perim3.f periml.f plotit.f point.f point3.f points.f psym3.f pwrit.f \
pwrity.f pwrx.f pwrxbd.f pwry.f pwrygt.f pwryso.f pwrz.f pwrzgi.f \
pwrzgs.f pwrzgt.f pwrzi.f pwrzoi.f pwrzos.f pwrzot.f pwrzs.f pwrzt.f \
q8qst4.f r1mach.f reord.f reset.f retsr.f sbytes.f set.f set3.f set3d.f \
seter.f seti.f setr.f setusv.f srface.f srfgk.f stcntr.f stline.f \
supcon.f supmap.f tick3.f tick4.f tick43.f ticks.f tr32.f trn32i.f \
trn32s.f trn32t.f vect3.f vectd.f vector.f velvct.f velvec.f \
wtstr.f xtch.f zerosc.f zlset.f 

BLOCKDATA_ROUTINES = conbd.f conbdn.f dashbd.f hfinit.f gridt.f	       \
isosrb.f mapbd.f pwrybd.f srfabd.f threbd.f uerrbd.f utilbd.f veldat.f

PRIVATE_INCLUDES = ncar.h

#  List of Fortran sources that must be edited before compilation.
FPP_ROUTINES = mapint.F pwritx.F ishift.F

#  Not used in most applications. ishift.c requires that ncar.h
#  is fixed to handle the generic case. This is not yet done.
C_ROUTINES = ishift.c

# build the data files

noinst_PROGRAMS = convrt pcrbin

convrt_SOURCES = $(CONVRT_ROUTINES)
CONVRT_ROUTINES = convrt.f

pcrbin_SOURCES = $(PWRITX_ROUTINES)
pcrbin_LDADD = libncar.la libncar_user.la `gks_link`
PWRITX_ROUTINES = pcrbin.f

ezmap.dat: convrt
	- rm -f fort.*
	$(LN_S) data/ezmapdat.source fort.1
	./convrt
	mv fort.2 ezmap.dat
	- rm -f fort.*

pwritx.dat: pcrbin
	- rm -f fort.*
	$(LN_S) data/pwritx.c1 fort.1
	$(LN_S) data/pwritx.c2 fort.2
	$(LN_S) data/pwritx.d1 fort.3
	$(LN_S) data/pwritx.d2 fort.4
	./pcrbin
	mv -f fort.7 pwritx.dat
	- rm -f fort.*

noinst_INCLUDES = $(PRIVATE_INCLUDES)

CLEANFILES = pwritx.dat ezmap.dat

dist_starnews_DATA = ncar.news

EXTRA_DIST = ncar.h
