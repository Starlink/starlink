## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = ndf_link ndf_link_adam
cincludedir = $(includedir)/star
dist_noinst_SCRIPTS = make_h

lib_LTLIBRARIES = libndf.la libndf_adam.la

libndf_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(BASIC_C_ROUTINES) \
	$(BUILT_C_ROUTINES) \
	$(STANDALONE_C_ROUTINES) \
	$(PRIVATE_INCLUDES)

libndf_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(BASIC_C_ROUTINES) \
	$(BUILT_C_ROUTINES) \
	$(ADAM_C_ROUTINES) \
	$(PRIVATE_INCLUDES)


# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libndf_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
libndf_adam_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
endif

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = ndf_err.h NDF_ERR

# Include files that should be installed
PUBLIC_F_INCLUDES = NDF_PAR  NDF_FUNC_DEC NDF_FUNC_DEF NDF_FUNC
PUBLIC_C_INCLUDES = ndf.h ndf_cgen.h ndf_types.h
PUBLIC_INCLUDES = $(PUBLIC_F_INCLUDES) $(PUBLIC_C_INCLUDES)

# Include files that are only needed for the build
PRIVATE_INCLUDES = ndf1.h ndf1_cgen.h ndf1_types.h ndf_dlt.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@


#  C files required for both the standalone and ADAM libraries.
BASIC_C_ROUTINES = ndf1A2p.c ndf1Absnt.c ndf1Accok.c ndf1GlobalDecs.c \
ndf1Acprp.c ndf1Acre.c ndf1Acrst.c ndf1Adcre.c ndf1Adext.c \
ndf1Adfrm.c ndf1Adini.c ndf1Admap.c ndf1Adprp.c ndf1Adrst.c \
ndf1Adsbn.c ndf1Adstp.c ndf1Adtyp.c ndf1Adump.c ndf1Affor.c ndf1Amap.c \
ndf1Amsg.c ndf1Anl.c ndf1Annpl.c ndf1Antmp.c ndf1Aprp.c ndf1Arst.c \
ndf1Asbnd.c ndf1Atyp.c ndf1Aump.c ndf1Avcre.c ndf1Avext.c \
ndf1Avfrm.c ndf1Avmap.c ndf1Avprp.c ndf1Avrst.c ndf1Avsbn.c ndf1Avstp.c \
ndf1Avtyp.c ndf1Avump.c ndf1Awcre.c ndf1Awext.c ndf1Awfrm.c \
ndf1Awini.c ndf1Awmap.c ndf1Awprp.c ndf1Awrst.c ndf1Awsbn.c \
ndf1Awstp.c ndf1Awtyp.c ndf1Awump.c ndf1Axlim.c ndf1Bad.c ndf1Bpp.c \
ndf1Cbfrm.c ndf1Ccpy.c ndf1Chacc.c ndf1Chftp.c ndf1Chhum.c \
ndf1Chmod.c ndf1Chtim.c ndf1Chxnm.c ndf1Clfor.c ndf1Cln.c ndf1Cmpac.c \
ndf1Cmpbl.c ndf1Cmpfl.c ndf1Cpync.c ndf1Crfor.c ndf1Crnbn.c ndf1Cut.c \
ndf1Cvcmd.c ndf1Cvfor.c ndf1Cvtok.c ndf1Da.c ndf1Dac.c \
ndf1Dad.c ndf1Dan.c ndf1Danl.c ndf1Dav.c ndf1Daw.c ndf1Dbad.c ndf1Dc.c \
ndf1Dcre.c ndf1Dcrep.c ndf1Dd.c ndf1Del.c ndf1Delob.c ndf1Dh.c ndf1Dimp.c \
ndf1Dlfor.c ndf1Dmap.c ndf1Dmsg.c ndf1Dnfor.c ndf1Docmd.c ndf1Dqanl.c \
ndf1Dq.c ndf1Dump.c ndf1Dvanl.c ndf1Dv.c ndf1Dw.c ndf1Dx.c ndf1Ellip.c \
ndf1Evmsg.c ndf1Expfn.c ndf1Expid.c ndf1Ffs.c ndf1Filac.c ndf1Filex.c \
ndf1Findwords.c ndf1Fmhdt.c ndf1Forxt.c ndf1Fr2px.c ndf1Freewords.c \
ndf1Fsplt.c ndf1Gadex.c ndf1Gawex.c ndf1Gtarg.c ndf1Gtbb.c \
ndf1Gtenv.c ndf1Gtfil.c ndf1Gthdt.c ndf1Gtxtn.c ndf1Hcopy.c ndf1Hcpy.c \
ndf1Hdcre.c ndf1Hderr.c ndf1Hfwrt.c ndf1Hincr.c ndf1Hlerr.c ndf1Hmp0C.c \
ndf1Hnew.c ndf1Hprp.c ndf1Hrst.c ndf1Hscrt.c ndf1Hsrt.c ndf1Htcmp.c \
ndf1Htlen.c ndf1Htop.c ndf1Hunmp.c ndf1Hwdef.c \
ndf1Hwrt.c ndf1Id2ac.c ndf1Imp.c ndf1Impid.c ndf1Imppl.c ndf1Indxp.c \
ndf1Infcb.c ndf1Inifr.c ndf1Init.c ndf1Intcb.c ndf1Isalm.c ndf1Isnam.c \
ndf1Map.c ndf1Mbad.c ndf1Mbndp.c ndf1Mbndt.c ndf1Move.c ndf1Mpanl.c \
ndf1Mpspt.c ndf1Mtyp.c ndf1Mxtyp.c ndf1Ncut.c ndf1Nfind.c ndf1Nplac.c \
ndf1Nsplt.c ndf1Ntfor.c ndf1Nxtsl.c ndf1Opfor.c ndf1P2a.c ndf1Plcre.c \
ndf1Pldcb.c ndf1Plfor.c ndf1Prfor.c ndf1Prp.c ndf1Pscpx.c ndf1Psffl.c \
ndf1Psfmt.c ndf1Pshdt.c ndf1Psndb.c ndf1Psnde.c ndf1Psndf.c ndf1Pstyp.c \
ndf1Pxlst.c ndf1Qbpp.c ndf1Qcre.c ndf1Qfrm.c ndf1Qimp.c \
ndf1Qityp.c ndf1Qma.c ndf1Qmap.c ndf1Qmlog.c ndf1Qrst.c \
ndf1Qsta.c ndf1Qump.c ndf1Rdast.c ndf1Rdtun.c ndf1Rdwcs.c ndf1Rjust.c \
ndf1Rls.c ndf1Rmblk.c ndf1Rst.c ndf1S2v.c ndf1Sctyp.c \
ndf1Simlr.c ndf1Spfor.c ndf1Spldt.c ndf1Ssdup.c ndf1Stats.c \
ndf1StoreSqLimit.c ndf1Strip.c ndf1Substr.c ndf1Tcnam.c ndf1Temp.c \
ndf1Tilde.c ndf1Time.c ndf1Trace.c ndf1True.c ndf1Twrap.c ndf1Typ.c \
ndf1Ump.c ndf1Uname.c ndf1V2s.c ndf1Vaccn.c ndf1Van.c \
ndf1Vbad.c ndf1Vbnd.c ndf1Vccn.c ndf1Vclst.c ndf1Vcpx.c ndf1Vcre.c \
ndf1Vdat.c ndf1Vfrm.c ndf1Vftp.c ndf1Vimp.c ndf1Vmap.c ndf1Vmmd.c \
ndf1Vmod.c ndf1Vrst.c ndf1Vsbd.c ndf1Vsftp.c ndf1Vsta.c ndf1Vstat.c \
ndf1Vstyp.c ndf1Vtyp.c ndf1Vump.c ndf1Vwcs.c ndf1Wclim.c ndf1Wcspm.c \
ndf1Wplim.c ndf1Wrast.c ndf1Wrwcs.c ndf1Wsbnd.c ndf1Wsta.c ndf1Wwrt.c \
ndf1Xcpy.c ndf1Xlst.c ndf1Xtfor.c ndf1Zpsca.c ndf1Zscal.c ndfAcget.c \
ndfAclen.c ndfAcmsg.c ndfAcput.c ndfAcre.c ndfAform.c ndfAmap.c \
ndfAnnul.c ndfAnorm.c ndfArest.c ndfAsnrm.c ndfAstat.c \
ndfAstyp.c ndfAtype.c ndfAunmp.c ndfBad.c ndfBase.c ndfBb.c ndfBegin.c \
ndfBlock.c ndfBound.c ndfCget.c ndfChunk.c ndfClen.c \
ndfClone.c ndfCmplx.c ndfCmsg.c ndfCopy.c ndfCput.c \
ndfDelet.c ndfDim.c ndfEnd.c ndfFind.c ndfForm.c \
ndfFtype.c ndfGtdlt.c ndfGtune.c ndfGtwcs.c \
ndfHappn.c ndfHcopy.c ndfHcre.c ndfHdef.c ndfHecho.c ndfHend.c ndfHfind.c \
ndfHgmod.c ndfHinfo.c ndfHndlr.c ndfHnrec.c ndfHout.c ndfHpurg.c \
ndfHput.c ndfHsdat.c ndfHsmod.c ndfImprt.c ndfIsacc.c ndfIsbas.c \
ndfIsin.c ndfIstmp.c ndfLoc.c ndfMap.c ndfMapql.c ndfMapz.c ndfMbad.c \
ndfMbadn.c ndfMbnd.c ndfMbndn.c ndfMsg.c ndfMtype.c ndfMtypn.c ndfNbloc.c \
ndfNchnk.c ndfNew.c ndfNewp.c ndfNoacc.c ndfOpen.c ndfPlace.c \
ndfPtwcs.c ndfQmf.c ndfReset.c ndfSame.c ndfSbad.c ndfSbb.c \
ndfSbnd.c ndfScopy.c ndfSctyp.c ndfSect.c ndfShift.c ndfSize.c ndfSqmf.c \
ndfSsary.c ndfState.c ndfStype.c ndfTemp.c ndfTrace.c ndfTune.c ndfType.c \
ndfUnmap.c ndfValid.c ndfXdel.c ndfXgt0c.c ndfXiary.c \
ndfXloc.c ndfXname.c ndfXnew.c ndfXnumb.c ndfXpt0c.c ndfXpt0l.c \
ndfXstat.c ndfZdelt.c ndfZscal.c ndf1IsValid.c ndf1Mjd2t.c ndf1Cuserid.c \
fortran_interface.c v1_interface.c ndfXgt0l.c ndf1CheckLocker.c \
ndf1LockDCB.c ndf1UnlockDCB.c ndf1LockACB.c ndf1UnlockACB.c \
ndfLock.c ndfUnlock.c ndf1Locked.c ndfLocked.c ndfReport.c \
ndf1UnlockLoc.c ndf1LockLoc.c ndf1UnlockAry.c ndf1LockAry.c

GEN_C_ROUTINES = ndf1Adex.cgen  ndf1Awex.cgen  ndf1Cvtx.cgen  \
ndf1V2sx.cgen ndfXgt0x.cgen ndf1Adix.cgen ndf1Awix.cgen ndf1Qmax.cgen \
ndfGtszx.cgen ndfXpt0x.cgen ndf1Avex.cgen ndf1Bppx.cgen ndf1S2vx.cgen \
ndfPtszx.cgen ndf1SqLimit.cgen

# The .c files which are built from the above .cgen files.
BUILT_C_ROUTINES = $(GEN_C_ROUTINES:.cgen=.c)

BUILT_SOURCES = $(include_MESSAGES) $(BUILT_C_ROUTINES) ndf1_cgen.h

ndf1_cgen.h: ndf1_cgen.h.source make_h
	./make_h

ndf_cgen.h: ndf_cgen.h.source make_h
	./make_h

#  Additional C files required for the standalone library.
STANDALONE_C_ROUTINES = ndf1Hwenv.c ndf1Getap.c

#  Additional C files required for the ADAM library.
ADAM_C_ROUTINES = ndfAssoc.c ndfCinp.c ndfCreat.c ndfCrep.c \
ndfExist.c ndfProp.c ndf1Getap_a.c ndf1Hwenv_a.c ndfCrepl.c \
ndf1Ptloc.c ndfCancl.c fortran_interface_adam.c v1_interface_adam.c



# Testing

TESTS = ndfCtest ndfCnulltest ndf_ftest ndf_ftest8
check_PROGRAMS = ndfCtest ndfCnulltest ndf_ftest ndf_ftest8
ndfCtest_SOURCES = ndfCtest.c
ndfCnulltest_SOURCES = ndfCnulltest.c
ndf_ftest_SOURCES = ndf_ftest.f
ndf_ftest8_SOURCES = ndf_ftest8.f

# LDADD is the expansion of ndf_link, with -lndf replaced by libndf.la
ndfCtest_LDADD = libndf.la `$(srcdir)/ndf_link`
ndfCnulltest_LDADD = libndf.la `$(srcdir)/ndf_link`
ndf_ftest_LDADD = libndf.la `$(srcdir)/ndf_link`
ndf_ftest8_LDADD = libndf.la `$(srcdir)/ndf_link`

dist_starnews_DATA = ndf.news
dist_pkgdata_DATA = LICENCE

CLEANFILES = NDF_ERR

# Create the .c file which defines type-specific code generated from each
# .cgen file.
$(GEN_C_ROUTINES:.cgen=.c):
	@echo "/*" > $@
	@echo "*  Name:" >> $@
	@echo "*    $@" >> $@
	@echo "" >> $@
	@echo "*  Purpose:" >> $@
	@echo "*    This file expands the generic C code held in $(@:.c=.cgen) to provide" >> $@
	@echo "*    the required type-specific implementations which can be called by" >> $@
	@echo "*    other functions." >> $@
	@echo "" >> $@
	@echo "*  Notes:" >> $@
	@echo "*    - This file is generated automatically at build time (see" >> $@
	@echo "*    Makefile.am)" >> $@
	@echo "*/" >> $@
	@echo "" >> $@
	@echo "#include \"prm_par.h\"" >> $@
	@echo "#include \"cgeneric.h\"" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_DOUBLE_TYPE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_FLOAT_TYPE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_INT_TYPE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_WORD_TYPE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_UWORD_TYPE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_BYTE_TYPE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_UBYTE_TYPE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_INT64_TYPE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@

# Need to distribute the generic files
EXTRA_DIST = $(GEN_C_ROUTINES) ndf_err.msg ndf1_cgen.h

