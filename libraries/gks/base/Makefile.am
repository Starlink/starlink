##

## The makefile simply has to process the .fpp include files
## See prm for example
## These files should not be installed

noinst_LTLIBRARIES = libgks_base.la

libgks_base_la_SOURCES = $(F_ROUTINES) $(FPP_ROUTINES) $(C_ROUTINES)

## Need main GKS directory for GKS_ERR
INCLUDES = -I..

# These are the old "sources" files and the "fortran" files.
# plus the "machines" files that do not actually need to be
# machines specific.

F_ROUTINES = \
gkbug.f gkcasm.f gkcctg.f gkcctm.f gkcela.f gkceli.f gkcell.f gkcelt.f \
gkcetr.f gkcfag.f gkcirc.f gkclds.f gkclip.f gkclks.f gkcplg.f gkcpmg.f \
gkcrce.f gkcrcn.f gkcrcs.f gkcrcv.f gkcsac.f gkcsai.f gkcsap.f gkcsar.f \
gkcsbi.f gkcsfp.f gkcsfr.f gkcsgc.f gkcsgi.f gkcsgp.f gkcsgr.f gkcsgt.f \
gkcsin.f gkcsit.f gkcslk.f gkcsnx.f gkcsot.f gkcsrd.f gkcsrl.f gkcssg.f \
gkcssl.f gkcsul.f gkcswd.f gkctxg.f gkcwcb.f gkcwsb.f gkcwsi.f gkcwsl.f \
gkdda.f gkdfab.f gkdhex.f gkdplb.f gkdpmb.f gkdrcr.f gkdrde.f gkdrdl.f \
gkdrge.f gkdrli.f gkdrpu.f gkdrq.f gkdrqn.f gkdrrn.f gkdrs.f gkdtxb.f \
gkecks.f gkedst.f gkepbb.f gkepbc.f gkerr.f gkesc.f gkfatz.f gkfhcl.f \
gkfila.f gkfilc.f gkfilh.f gkfill.f gkfilp.f gkfils.f gkfind.f gkgem.f \
gkgtrc.f gkhpal.f gkhpcl.f gkhpda.f gkhpgi.f gkhpgr.f gkhpop.f gkhppi.f \
gkhppr.f gkhpq.f gkhpqs.f gkhpzz.f gkiea.f gkilc.f gkinip.f gkiobg.f \
gkioen.f gkisk.f gkiwsb.f gkiwsi.f gkiwsl.f gkiwso.f gklclp.f gklerr.f \
gklncr.f gkltyp.f gklump.f gklwdt.f gkmca.f gkmcvx.f gkmet.f gkmfa.f \
gkmgdp.f gkmpar.f gkmpl.f gkmpm.f gkmtdv.f gkmtiv.f gkmtml.f gkmtnd.f \
gkmtx.f gkmtxf.f gkmtyp.f gknext.f gknrl.f gkowcb.f gkpcbi.f gkpclp.f \
gkpcp.f gkpcrd.f gkpcsi.f gkpcxc.f gkpcxi.f gkpcxp.f gkpdmv.f gkpidd.f \
gkpikp.f gkpipd.f gkpkc.f gkpki.f gkpkpb.f gkpkr.f gkpkrq.f gkpksi.f \
gkpmap.f gkpntp.f gkppaf.f gkppal.f gkppbb.f gkppbc.f gkppbe.f gkppbp.f \
gkppbu.f gkppfa.f gkppms.f gkpppl.f gkpppm.f gkpppr.f gkppsg.f gkprec.f \
gkprei.f gkprlg.f gkpsad.f gkpscb.f gkpsci.f gkpscl.f gkpsco.f gkpsfh.f \
gkpsji.f gkpsjn.f gkpsni.f gkpsqx.f gkpwal.f gkpwcd.f gkpwcg.f gkpwcn.f \
gkpwcr.f gkpwcx.f gkpwgs.f gkpwhd.f gkpwhf.f gkpwhg.f gkpwhl.f gkpwhm.f \
gkpwhp.f gkpwpd.f gkpwps.f gkpwrd.f gkpwrl.f gkpwrq.f gkpwsb.f gkpwsl.f \
gkpwss.f gkpwtx.f gkpxad.f gkqint.f gkqlni.f gkqpxa.f gkqwdt.f gkqwk.f \
gkqxxd.f gkqxxi.f gkrdct.f gkrdrw.f gkrect.f gkrgn.f gkrotv.f gkrqch.f \
gkrqes.f gkrqip.f gkrqlc.f gkrqpk.f gkrqst.f gkrwks.f gkscan.f gksect.f \
gksgac.f gksgat.f gksgcn.f gksgoc.f gksgpb.f gksgwb.f gksgwk.f gksipm.f \
gkslbb.f gkslbf.f gkslcr.f gksldl.f gkslds.f gkslfn.f gkslge.f gksllk.f \
gkslnb.f gkslnm.f gkslpr.f gkslpu.f gkslqn.f gkslsp.f gksrfa.f gksrpa.f \
gksrpl.f gksrpm.f gksrtx.f gkstal.f gkstcl.f gkstda.f gkstop.f gkstq.f \
gkswkl.f gkswkv.f gkswkw.f gksyn.f gktcvx.f gktdn.f gktdr.f gktilt.f \
gktnd.f gktnw.f gktnwv.f gktold.f gktwd.f gktwdv.f gktwn.f gktwnp.f \
gktwnv.f gktzex.f gktzhp.f gktzjp.f gktzme.f gktzre.f gktzrh.f gktzsl.f \
gktzsw.f gktztp.f gktzvp.f gkupc.f gkupi.f gkupr.f gkupsi.f gkurec.f \
gkurei.f gkvers.f gkwcld.f gkwddl.f gkwkc4.f gkwspb.f gkxbcl.f gkxcge.f \
gkxcpu.f gkxdcs.f gkxdwc.f gkxdwo.f gkxfd.f gkxlin.f gkxof.f gkxon.f \
gkxpal.f gkxpat.f gkxpcc.f gkxpcx.f gkxpxc.f gkxpxo.f gkxqxc.f gkxqxo.f \
gkxss.f gkxtfb.f gkxtfp.f gkxthw.f gkxxf.f gkan1.f gkaton.f gkfobo.f \
gkfoco.f gkiocl.f gkiofi.f lnblnk.f gkna1.f gkntoa.f gkioop.f \
gactm.f gacwk.f gasgwk.f gca.f gclks.f gclrwk.f gclsg.f gclwk.f \
gcrsg.f gcsgwk.f gdawk.f gdsg.f gdsgwk.f geclks.f gerhnd.f gerlog.f \
gesc.f gevtm.f gfa.f ggdp.f ggtitm.f giitm.f ginch.f ginlc.f ginpk.f \
ginsg.f ginsk.f ginst.f ginvl.f gmsg.f gmsgs.f gopks.f gopwk.f gpl.f \
gpm.f gprec.f gqacwk.f gqasf.f gqaswk.f gqcf.f gqchb.f gqchh.f gqchs.f \
gqchsp.f gqchup.f gqchw.f gqchxp.f gqclip.f gqcntn.f gqcr.f gqdch.f \
gqdds.f gqdlc.f gqdpk.f gqdsga.f gqdsk.f gqdsp.f gqdst.f gqdvl.f \
gqdwka.f gqeci.f gqefai.f gqegdp.f gqentn.f gqepai.f gqepli.f gqepmi.f \
gqetxi.f gqewk.f gqfaci.f gqfaf.f gqfai.f gqfais.f gqfar.f gqfasi.f \
gqgdp.f gqlcs.f gqli.f gqln.f gqlvks.f gqlwk.f gqlwsc.f gqmk.f \
gqmksc.f gqmntn.f gqnt.f gqops.f gqopsg.f gqopwk.f gqpa.f gqpaf.f \
gqpar.f gqparf.f gqpcr.f gqpfar.f gqpkid.f gqpks.f gqplci.f gqplf.f \
gqpli.f gqplr.f gqpmci.f gqpmf.f gqpmi.f gqpmr.f gqppar.f gqpplr.f \
gqppmr.f gqptxr.f gqpx.f gqpxa.f gqpxad.f gqsga.f gqsgp.f gqsgus.f \
gqsgwk.f gqsks.f gqsts.f gqtxal.f gqtxci.f gqtxf.f gqtxfp.f gqtxi.f \
gqtxp.f gqtxr.f gqtxx.f gqtxxs.f gqvls.f gqwkc.f gqwkca.f gqwkcl.f \
gqwkdu.f gqwkm.f gqwks.f gqwkt.f grditm.f grensg.f grqch.f grqlc.f \
grqpk.f grqsk.f grqst.f grqvl.f grsgwk.f gsasf.f gschh.f gschm.f \
gschsp.f gschup.f gschxp.f gsclip.f gscr.f gsds.f gsdtec.f gselnt.f \
gsfaci.f gsfai.f gsfais.f gsfar.f gsfasi.f gshlit.f gslcm.f gsln.f \
gslwsc.f gsmk.f gsmksc.f gspa.f gspar.f gsparf.f gspkid.f gspkm.f \
gsplci.f gspli.f gsplr.f gspmci.f gspmi.f gspmr.f gssgp.f gssgt.f \
gsskm.f gsstm.f gstxal.f gstxci.f gstxfp.f gstxi.f gstxp.f gstxr.f \
gsvis.f gsvlm.f gsvp.f gsvpip.f gswkvp.f gswkwn.f gswn.f gtx.f gtxs.f \
gurec.f guwk.f gwitm.f



# These are the routines that were in the machines directory but
# require FPP/CPP processing and configure checks

FPP_ROUTINES = gkdate.F gkdfop.F gkiobi.F gkiobo.F \
	gkioci.F gkioco.F gkiofo.F gklor.F gkland.F \
	gktime.F getfd.F

C_ROUTINES = gktset.c gkgtsp.c gkiosb.c getputc.c

dist_noinst_HEADERS = f77_type.h gkerr.h gkdt.h gkwca.h



