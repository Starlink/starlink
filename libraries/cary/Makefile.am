## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = ary_link ary_link_adam
cincludedir = $(includedir)/star
dist_noinst_SCRIPTS = make_h

# Note that "classic" has a libary and libary_adam but they
# are identical. Since everyone goes through the _link scripts
# there is no reason to persist with this madness so we have only
# one library here.

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

PUBLIC_F_INCLUDES =
PUBLIC_C_INCLUDES = ary.h ary_types.h
PUBLIC_INCLUDES = $(PUBLIC_F_INCLUDES) $(PUBLIC_C_INCLUDES)
include_MESSAGES = ary_err.h ARY_ERR

#C_ROUTINES = ary1_s2dlt.c ary1_undlt.c
C_ROUTINES = ary1Trace.c aryTrace.c ary1Rls.c ary1Ffs.c ary1Dfrm.c \
ary1Get0C.c ary1Dimp.c ary1Dmod.c ary1Defr.c ary1Intyp.c ary1Dtyp.c \
ary1Dlshp.c ary1Dbnd.c ary1Hunmp.c ary1Danl.c ary1Dsta.c ary1Antmp.c \
ary1Dbad.c ary1Crnba.c ary1Chscn.c ary1DCBLock.c aryLocked.c ary1Tcnam.c \
aryUnlock.c aryLock.c ary1Imp.c ary1Nxtsl.c ary1IsValid.c ary1Expid.c \
ary1Impid.c ary1Id2ac.c aryFind.c aryAnnul.c ary1Anl.c ary1Ump.c \
ary1Umps.c ary1Mpsw.c ary1Cmtmp.c ary1Temp.c ary1Iobw.c ary1Vbad.c \
ary1Vzero.c ary1Upsr.c ary1Upsw.c ary1Sbd.c ary1Dsbd.c ary1Gmrb.c \
ary1Inbnd.c ary1Xsbnd.c ary1Ptn.c ary1Dp2s.c ary1Ccpy.c $(BUILT_C_ROUTINES)

CGENERIC_ROUTINES = ary1Badx.cgen ary1Zerx.cgen ary1Ptnx.cgen \
ary1Pt1x.cgen ary1Cvfx.cgen

# The .c files which are built from the above .cgen files.
BUILT_C_ROUTINES = $(CGENERIC_ROUTINES:.cgen=.c)

BUILT_SOURCES = $(include_MESSAGES) $(BUILT_C_ROUTINES) ary1_cgen.h

ary1_cgen.h: ary1_cgen.h.source make_h
	./make_h

# Testing

TESTS = aryTest
check_PROGRAMS = aryTest

aryTest_SOURCES = aryTest.c

# LDADD is the expansion of ary_link, with -lary replaced by libary.la
aryTest_LDADD = libary.la `hds_link` `err_link` `chr_link` `prm_link`

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

