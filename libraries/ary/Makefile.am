## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = ary_link ary_link_adam
cincludedir = $(includedir)/star
dist_noinst_SCRIPTS = make_h

lib_LTLIBRARIES = libary.la

libary_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(C_ROUTINES) \
	$(PRIVATE_INCLUDES)

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libary_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
endif

include_HEADERS = $(PUBLIC_INCLUDES)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

PRIVATE_INCLUDES = ary1.h ary1_cgen.h ary1_types.h ary_dlt.h

PUBLIC_F_INCLUDES = ARY_PAR
PUBLIC_C_INCLUDES = ary.h ary_types.h
PUBLIC_INCLUDES = $(PUBLIC_F_INCLUDES) $(PUBLIC_C_INCLUDES)
include_MESSAGES = ary_err.h ARY_ERR

C_ROUTINES = $(BUILT_C_ROUTINES) ary1Accok.c ary1Anl.c ary1Annpl.c \
ary1Antmp.c ary1Bad.c ary1Bpp.c ary1Ccpy.c ary1Chacc.c ary1Chbpp.c \
ary1Chcma.c ary1Chmod.c ary1Chscn.c ary1Cln.c ary1Cmtmp.c \
ary1Cpdlt.c ary1Cpscl.c ary1Cpy.c ary1Cpync.c ary1Crnba.c ary1Cut.c \
ary1DCBLock.c ary1Danl.c ary1Dbad.c ary1Dbnd.c ary1Dcpy.c ary1Dcre.c \
ary1Dcrep.c ary1Defr.c ary1Del.c ary1Dfppl.c ary1Dfrm.c ary1Dimp.c \
ary1Dlshp.c ary1Dmod.c ary1Dobj.c ary1Dp2s.c ary1Drst.c ary1Dsbd.c \
ary1Dsbnd.c ary1Dscl.c ary1Dsft.c ary1Dsta.c ary1Dstp.c ary1Dtyp.c \
ary1Dvfy.c ary1Expid.c ary1Extyp.c ary1Ffs.c ary1Get0C.c ary1Gmrb.c \
ary1Gtdlt.c ary1Gtn.c ary1Hunmp.c ary1Id2ac.c ary1Imp.c ary1Impid.c \
ary1Inbnd.c ary1Intyp.c ary1Iobw.c ary1IsValid.c ary1Maps.c ary1Mpsr.c \
ary1Mpsw.c ary1Nel.c ary1NewOr.c ary1Nxtsl.c ary1Pbnd.c ary1Ptn.c \
ary1Rebnd.c ary1Retyp.c ary1Rls.c ary1S2dlt.c ary1Sbd.c ary1Sbnd.c \
ary1Sft.c ary1Stp.c ary1Tcnam.c ary1Temp.c ary1Trace.c ary1Ump.c \
ary1Umps.c ary1Undlt.c ary1Upsr.c ary1Upsw.c ary1Vbad.c ary1Vbnd.c \
ary1Vftp.c ary1Vmmd.c ary1Vscl.c ary1Vtyp.c ary1Vzero.c ary1Xsbnd.c \
aryAnnul.c aryBad.c aryBase.c aryBound.c aryClone.c aryCmplx.c \
aryCopy.c aryDelet.c aryDelta.c aryDim.c aryDupe.c aryFind.c \
aryForm.c aryFtype.c aryGtdlt.c aryImprt.c aryIsacc.c aryIsbas.c \
aryIsmap.c aryIstmp.c aryLoc.c aryLock.c aryLocked.c aryMap.c \
aryMapz.c aryMsg.c aryNdim.c aryNew.c aryNewp.c aryNoacc.c aryOffs.c \
aryPlace.c aryReset.c arySame.c arySbad.c arySbnd.c arySctyp.c \
arySect.c aryShift.c arySize.c arySsect.c aryState.c aryStype.c \
aryTemp.c aryTrace.c aryType.c aryUnlock.c aryUnmap.c aryValid.c \
aryVerfy.c fortran_interface.c

CGENERIC_ROUTINES = ary1Badx.cgen ary1Zerx.cgen ary1Ptnx.cgen \
ary1Pt1x.cgen ary1Cvfx.cgen ary1Gtnx.cgen ary1Gt1x.cgen ary1Cvtx.cgen \
ary1Bppx.cgen aryGtszx.cgen aryPtszx.cgen

# The .c files which are built from the above .cgen files.
BUILT_C_ROUTINES = $(CGENERIC_ROUTINES:.cgen=.c)

BUILT_SOURCES = $(include_MESSAGES) $(BUILT_C_ROUTINES) ary1_cgen.h

ary1_cgen.h: ary1_cgen.h.source make_h
	./make_h

ary_cgen.h: ary_cgen.h.source make_h
	./make_h

# Testing

TESTS = aryTest ary_test64 ary_test32
check_PROGRAMS = aryTest ary_test64 ary_test32
aryTest_SOURCES = aryTest.c
ary_test64_SOURCES = ary_test64.f
ary_test32_SOURCES = ary_test32.f

# LDADD is the expansion of ary_link, with -lary replaced by libary.la
aryTest_LDADD = libary.la `ast_link` `hds_link` `err_link` `chr_link` `prm_link`
ary_test64_LDADD = libary.la `ast_link` `hds_link` `err_link` `chr_link` `prm_link`
ary_test32_LDADD = libary.la `ast_link` `hds_link` `err_link` `chr_link` `prm_link`

dist_starnews_DATA = ary.news
dist_pkgdata_DATA = LICENCE

CLEANFILES = ARY_ERR

# Create the .c file which defines type-specific code generated from each
# .cgen file.
$(CGENERIC_ROUTINES:.cgen=.c):
	echo "/*" > $@
	echo "*  Name:" >> $@
	echo "*    $@" >> $@
	echo "" >> $@
	echo "*  Purpose:" >> $@
	echo "*    This file expands the generic C code held in $(@:.c=.cgen) to provide" >> $@
	echo "*    the required type-specific implementations which can be called by" >> $@
	echo "*    other functions." >> $@
	echo "" >> $@
	echo "*  Notes:" >> $@
	echo "*    - This file is generated automatically at build time (see" >> $@
	echo "*    Makefile.am)" >> $@
	echo "*/" >> $@
	echo "" >> $@
	echo "#include \"prm_par.h\"" >> $@
	echo "#include \"cgeneric.h\"" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_DOUBLE_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_FLOAT_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_INT_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_WORD_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_UWORD_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_BYTE_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_UBYTE_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@
	echo "#define CGEN_CODE_TYPE CGEN_INT64_TYPE" >> $@
	echo "#include \"cgeneric_defs.h\"" >> $@
	echo "#include \"$(@:.c=.cgen)\"" >> $@
	echo "#undef CGEN_CODE_TYPE" >> $@
	echo "" >> $@

# Need to distribute the generic files
EXTRA_DIST = $(CGENERIC_ROUTINES) ary_err.msg ary1_cgen.h

