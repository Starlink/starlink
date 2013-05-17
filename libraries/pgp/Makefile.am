## Process this file with automake to produce Makefile.in

SUBDIRS = examples

# grfont.dat and rgb.txt are not actually programs but they still need to
# go into bin/

dist_bin_SCRIPTS = pgp_link grfont.dat rgb.txt

lib_LTLIBRARIES = libpgp.la

libpgp_la_SOURCES = $(F_ROUTINES) $(include_MESSAGES)
libpgp_la_LIBADD = `gks_link` `gns_link`

include_MESSAGES = PGP_ERR

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately). Keep this list in alphabetical
#  order so that the subroutine description html comes out in the right 
#  order.

PG_ROUTINES = pgarro.f pgask.f pgband.f pgbbuf.f pgbeg.f pgbin.f       \
pgbox.f pgbox1.f pgcirc.f pgcl.f pgclos.f pgcn01.f pgcnsc.f pgconb.f   \
pgconl.f pgcons.f pgcont.f pgconx.f pgcp.f pgctab.f pgcurs.f pgdraw.f  \
pgebuf.f pgend.f pgenv.f pgeras.f pgerrb.f pgerrx.f pgerry.f pgetxt.f  \
pgfunt.f pgfunx.f pgfuny.f pggray.f pghi2d.f pghis1.f pghist.f	       \
pghtch.f pgiden.f pgimag.f pginit.f pglab.f pglcur.f pgldev.f pglen.f  \
pgline.f pgmove.f pgmtxt.f pgncur.f pgnoto.f pgnpl.f pgnumb.f pgolin.f \
pgopen.f pgpage.f pgpanl.f pgpap.f pgpixl.f pgpnts.f pgpoly.f pgpt.f   \
pgptxt.f pgqah.f pgqcf.f pgqch.f pgqci.f pgqcir.f pgqcol.f pgqcr.f     \
pgqcs.f pgqfs.f pgqhs.f pgqid.f pgqinf.f pgqitf.f pgqls.f pgqlw.f      \
pgqpos.f pgqtbg.f pgqtxt.f pgqvp.f pgqvsz.f pgqwin.f pgrect.f pgrnd.f  \
pgrnge.f pgsah.f pgsave.f pgscf.f pgsch.f pgsci.f pgscir.f pgscr.f     \
pgscrn.f pgsetc.f pgsfs.f pgshls.f pgshs.f pgsitf.f pgslct.f pgsls.f   \
pgslw.f pgstbg.f pgsubp.f pgsvp.f pgswin.f pgtbox.f pgtext.f pgupdt.f  \
pgvect.f pgvsiz.f pgvstd.f pgvw.f pgwedg.f pgwnad.f

PG_OBSOLETE = pgadvance.f pgbegin.f pgcurse.f pglabel.f pgmtext.f     \
pgncurse.f pgpaper.f pgpoint.f pgptext.f pgsize.f pgvport.f pgvsize.f \
pgvstand.f pgwindow.f

GR_ROUTINES = grarea.f grbpic.f grchkt.f grchsz.f grclos.f grclwk.f    \
grcoci.f grcurs.f grdate.f grdot0.f grdtyp.f gretext.f grfa.f grfilt.f \
grflu0.f grgcom.f grgenv.f grglun.f grgra1.f grgray.f grinqcol.f       \
grinqdev.f grinqls.f grinqlw.f grinqtyp.f grldev.f grlen.f grlin0.f    \
grlin1.f grlin2.f grlin3.f grlina.f grmarker.f grmova.f grmsg.f	       \
grobs.f gropen.f gropwk.f grpage.f grpixl.f grprom.f grqcol.f grqdev.f \
grqfnt.f grqpos.f grqrep.f grrect.f grsave.f grscr.f grselect.f	       \
grsetc.f grsetcol.f grsetls.f grsetlw.f grsets.f grsfnt.f grsfs.f      \
grsize.f grterm.f grtext.f grtoup.f grtrn0.f grtter.f grtxy0.f	       \
gruser.f grvct0.f grwarn.f grxhls.f grxrgb.f grqcr.f grqcap.f groptx.f \
grskpb.f grctoi.f grflun.f grtrim.f grpocl.f grqtxt.f grgfil.f	       \
grgra2.f grimg0.f grsyds.f grsymk.f grsyxd.f grsy00.f grinit.f

F_ROUTINES = $(PG_ROUTINES) $(PG_OBSOLETE) $(GR_ROUTINES)

BUILT_SOURCES = PGP_ERR pgplot.html

# Create the HTML page for the PGPLOT documentation.
pgplot.html : $(PG_ROUTINES) makehtml
	$(mkdir_p) $(srcdir)/sun15.htx
	$(srcdir)/makehtml $(PG_ROUTINES) > $(srcdir)/pgplot.html
	cp -p pgplot.html $(srcdir)/sun15.htx/pgplot.html

# Need to build the font database. Do not want to install this file.
# Really should be using libtool

pack: pack.o
	$(FCLINK) pack.o

grfont.dat : pack grfont.txt
	-rm -f grfont.dat
	./pack

# Test the build

TESTS = pgp_test
check_PROGRAMS = pgp_test

pgp_test_SOURCES = pgp_test.f
pgp_test_LDADD = libpgp.la `$(srcdir)/pgp_link`

EXTRA_DIST = pgp_err.msg pgplot.inc grecom.inc grfont.txt pack.f PGP_ERR_ext \
	makehtml pgplot.html

noinst_INCLUDES = pgplot.inc grecom.inc

CLEANFILES = PGP_ERR pack grfont.dat 

dist_starnews_DATA = pgp.news

dist_pkgdata_DATA = copyright.notice
