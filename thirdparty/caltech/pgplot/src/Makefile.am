## Process this file with automake to produce Makefile.in

# grfont.dat and rgb.txt are not actually programs but they still need to
# go into bin/

lib_LTLIBRARIES = libpgplot_base.la

libpgplot_base_la_SOURCES = $(F_ROUTINES)


#  List of Fortran routines required for building the package.  This is
#  just a list of all the Fortran source files (excluding BLOCK DATA
#  routines, which are treated separately). Keep this list in alphabetical
#  order so that the subroutine description html comes out in the right 
#  order.

F_ROUTINES = \
grarea.f grbpic.f grchar.f grchr0.f grchsz.f grclip.f grclos.f	       \
grclpl.f grctoi.f grcurs.f grdat2.f grdot0.f grdot1.f grdtyp.f	       \
grepic.f gresc.f gretxt.f grfa.f grfao.f grgfil.f grgray.f grgtc0.f    \
grimg0.f grimg1.f grimg2.f grimg3.f grinit.f grinqfont.f grinqli.f     \
grinqpen.f gritoc.f grldev.f grlen.f grlin0.f grlin1.f grlin2.f	       \
grlin3.f grlina.f grlinr.f grmark.f grmcur.f grmker.f grmova.f	       \
grmovr.f grmsg.f gropen.f grpage.f grpars.f grpixl.f grpocl.f grprom.f \
grpxpo.f grpxps.f grpxpx.f grpxre.f grqcap.f grqci.f grqcol.f grqcr.f  \
grqdev.f grqdt.f grqfnt.f grqls.f grqlw.f grqpos.f grqtxt.f grqtyp.f   \
grquit.f grrec0.f grrect.f grsci.f grscr.f grscrl.f grsetc.f	       \
grsetfont.f grsetli.f grsetpen.f grsets.f grsfnt.f grsize.f grskpb.f   \
grslct.f grsls.f grslw.f grsyds.f grsymk.f grsyxd.f grterm.f grtext.f  \
grtoup.f grtran.f grtrim.f grtrn0.f grtxy0.f grvct0.f grvect.f	       \
grwarn.f grxhls.f grxrgb.f pgadvance.f pgarro.f pgask.f pgaxis.f       \
pgaxlg.f pgband.f pgbbuf.f pgbeg.f pgbegin.f pgbin.f pgbox1.f pgbox.f  \
pgcirc.f pgcl.f pgclos.f pgcn01.f pgcnsc.f pgconb.f pgconf.f pgconl.f  \
pgcons.f pgcont.f pgconx.f pgcp.f pgctab.f pgcurse.f pgcurs.f pgdraw.f \
pgebuf.f pgend.f pgenv.f pgeras.f pgerr1.f pgerrb.f pgerrx.f pgerry.f  \
pgetxt.f pgfunt.f pgfunx.f pgfuny.f pggray.f pghi2d.f pghis1.f	       \
pghist.f pghtch.f pgiden.f pgimag.f pginit.f pglabel.f pglab.f	       \
pglcur.f pgldev.f pglen.f pgline.f pgmove.f pgmtext.f pgmtxt.f	       \
pgncur.f pgncurse.f pgnoto.f pgnpl.f pgnumb.f pgolin.f pgopen.f	       \
pgpage.f pgpanl.f pgpaper.f pgpap.f pgpixl.f pgpnts.f pgpoint.f	       \
pgpoly.f pgpt1.f pgptext.f pgpt.f pgptxt.f pgqah.f pgqcf.f pgqch.f     \
pgqci.f pgqcir.f pgqclp.f pgqcol.f pgqcr.f pgqcs.f pgqdt.f pgqfs.f     \
pgqhs.f pgqid.f pgqinf.f pgqitf.f pgqls.f pgqlw.f pgqndt.f pgqpos.f    \
pgqtbg.f pgqtxt.f pgqvp.f pgqvsz.f pgqwin.f pgrect.f pgrnd.f pgrnge.f  \
pgsah.f pgsave.f pgscf.f pgsch.f pgsci.f pgscir.f pgsclp.f pgscr.f     \
pgscrl.f pgscrn.f pgsetc.f pgsfs.f pgshls.f pgshs.f pgsitf.f pgsize.f  \
pgslct.f pgsls.f pgslw.f pgstbg.f pgsubp.f pgsvp.f pgswin.f pgtbox.f   \
pgtext.f pgtick.f pgtikl.f pgupdt.f pgvect.f pgvport.f pgvsize.f       \
pgvsiz.f pgvstand.f pgvstd.f pgvw.f pgwedg.f pgwindow.f pgwnad.f


PRIVATE_INCLUDES = grpckg1.inc  pgplot.inc
