#  File to define groups for use by the SDT grp command.
#  ====================================================
#  This is a description file for the "make" utility.

# The names of all the tar files 
TAR_FILES = dipso_source.tar  dipso_sub1.tar

# The contents of the dipso_source.tar file:
DIPSO_SOURCE = ATLIST.DAT ATOMIC.DAT DEMO1.sdf DEMO1_STK.sdf DEMO2.sdf \
LWPLO.ABS LWRLO.ABS NEBCONT.DAT SWP27857L.ULD SWPCOR.DAT SWPLO.ABS \
TEST_STK.sdf check command.hlp demo1.cmd demo2.cmd dipso.hlp dipso_link \
dipsosetup dipsotest exercise.cmd help.lis run_dipso.c testsetup \
testspec2.cmd testspec2.cmd.%1 updates.lis ussp.cmd

# The contents of the dipso_sub1.tar file:
DIPSO_SUB1 = abload.f abscal.f acheck.f arb.f arith.f arrow.f arysrt.f \
asterix.f atfit.f atlasrd.f axmat.f badchk.f bbint.f bbody.f calcfx.f \
cdraw.f cheatsm.f cmatch.f comnds.f convol.f cpair.f croper.f ctrlc_ast.f \
dacfit.f dashit.f dbound.f decimate.f declare_data.f declare_lbls.f \
declare_plts.f declare_stks.f decode.f decrng.f deftyp.f dela.f dered.f \
detev.f dipso.f dparse.f dsb_clr.f dtoupp.f elfbps.f elfclr.f elfdep.f \
elfg.f elfinv.f elfit.f elfkey.f elfkj.f elfparea.f elfpflx.f elfpl.f \
elfpnt.f elfset.f elfsfun.f elfstor.f elfvar.f elfview.f endmsk.f \
ermess.f even.f ewmeas.f expand.f expar.f explabel.f fcaller.f fcdel.f \
fcpop.f fcrest.f fcsave.f fcsl.f filter.f findit.f flfix.f fneb.f fnum.f \
fouri.f fourier.f fpget.f fpush.f fword.f gammln.f gammode.f gauss.f \
gcfmode.f get0c.f get0i.f get0l.f get0r.f getcat.f getcho.f getcom.f \
getcom2.f getdir.f getinp.f getndf.f getrng.f getstk.f gsermode.f \
gtctcl.f gtenv.f gtslot.f handler.f handlev.c heapsrt.f help1.f ihmmnt.f \
ihmrg.f ihrot.f ihrstr.f ihsmth.f ihsval.f ihubv.f incxy.f integrate.f \
intep.f ipset.f iruser.f is_blk.f isatm.f isbach.f itochr.f iwrite.f \
jsymbol.f kars_com kdcode.f kuse_com lagint.f lagint4.f laguerre.f \
lalpha.f lblrun.f lenstr.f locdat.f lwrcor.f models.f modest.f mongowr.f \
move_real.f msgout.f ndfnam.f nebcont.f nebset.f nextk.f ocdraw.f \
options.f osp0rd.f osp0wr.f parse_env.f pdgfit.f perigram.f pltarr.f \
pointsym.f poly.f ppalet.f prpage.f putext.f pwritit.f range.f rdcat.f \
rdctd.f rdkbd2.c rdkybd.f rdndf.f rdstr.f read.f readvmsrec.f reca.f \
record.f remap.f repfrm.f reporting.f rerep.f resint.f resstk.f restore.f \
reverx.f rinteg.f rlint.f save.f savstk.f sdocont.f setcolours.f \
sgscurse.f slen.f smatin.f smooth.f snip.f sofar.f sort1i.f sp0rd.f \
sp0wr.f sp1rd.f sp2rd.f sp2wr.f spload.f srtbrk.f ssdraw.f sstrip.f \
star.f strrd.f swcor2.f swpcor.f sysexe.f tau.f toreal.f update.f upush.f \
user.f va04a.f velsort.f voigt.f vpeak.f vsp0rd.f window.f wrcopy.f \
write.f write_ndf.f wrndf.f wrprop.f wtstruser.f xcorre.f xdcode.f \
xhelp.f xsort.f xsprmpt.f yendmsk.f yxn.f zanstra.f 

# The other files which need to be extracted from the source code
# repository and which end up in kappa.tar.
UNIX_OTHERS = makefile mk sun50.tex dipso.news dipso_specdat.tar

#  All files which need to be extracted from the RCS repository in order
#  to make a UNIX release. 
UNIX_RELEASE =  $(UNIX_OTHERS) $(DIPSO_SOURCE) $(DIPSO_SPECDAT) \
$(DIPSO_SUB1)

#  The contents of kappa.tar.
UNIX_TOTAL = makefile mk sun50.tex sun50.htx_tar dipso.news \
dipso_sub1.tar dipso_specdat.tar

#  Target for use by the grp command.
$(action)

#  Keyword for use by RCS.
# $Id$ 
