## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = prm_link prm_link_adam
lib_LTLIBRARIES = libprm.la libprm_a.la
noinst_PROGRAMS = make-prm-par

libprm_la_SOURCES = \
	$(F_ROUTINES) \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(noinst_MESSAGES) \
	$(PRIVATE_INCLUDES) \
	$(BUILT_C_ROUTINES) \
	PRM_PAR

libprm_a_la_SOURCES = \
	$(PLATFORM_C)

# Make all library code position independent. This is handy for creating
# shareable libraries from the static ones (Java JNI libraries).
if !NOPIC
libprm_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
libprm_la_FCFLAGS = $(AM_FCFLAGS) -prefer-pic
libprm_a_la_CFLAGS = $(AM_CFLAGS) -prefer-pic
endif

make_prm_par_SOURCES = make-prm-par.c
make_prm_par_LDADD = -lm

include_HEADERS = $(NORMAL_INCLUDES) $(PLATFORM_INCLUDES)
include_MESSAGES = PRM_ERR
noinst_MESSAGES = prm_err.h

# The common block should not be installed since it will not
# work with shared libraries. The correct API should be used instead
noinst_HEADERS = $(PLATFORM_PRIVATE_INCLUDES)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

F_ROUTINES = $(NONGEN_F_ROUTINES) $(GEN_F_ROUTINES)

NONGEN_F_ROUTINES = val_absb.f val_absd.f val_absi.f val_absk.f \
val_absr.f val_absub.f val_absuw.f val_absw.f val_acosd.f val_acosr.f \
val_acsdd.f val_acsdr.f val_asind.f val_asinr.f val_asndd.f \
val_asndr.f val_at2dd.f val_at2dr.f val_atand.f val_atanr.f val_atn2d.f \
val_atn2r.f val_atndd.f val_atndr.f val_cosd.f val_cosdd.f \
val_cosdr.f val_coshd.f val_coshr.f val_cosr.f \
val_negb.f val_negd.f val_negi.f val_negr.f val_negub.f val_neguw.f \
val_negw.f val_signb.f val_signd.f \
val_signi.f val_signr.f val_signub.f val_signuw.f val_signw.f \
val_sind.f val_sindd.f val_sindr.f val_sinhd.f val_sinhr.f val_sinr.f \
val_tand.f val_tandd.f val_tandr.f \
val_tanhd.f val_tanhr.f val_tanr.f \
vec_absb.f vec_absd.f vec_absi.f vec_absk.f vec_absr.f \
vec_absub.f vec_absuw.f vec_absw.f vec_acosd.f vec_acosr.f vec_acsdd.f \
vec_acsdr.f vec_asind.f vec_asinr.f vec_asndd.f vec_asndr.f \
vec_at2dd.f vec_at2dr.f vec_atand.f vec_atanr.f vec_atn2d.f vec_atn2r.f \
vec_atndd.f vec_atndr.f vec_cosd.f vec_cosdd.f vec_cosdr.f \
vec_coshd.f vec_coshr.f vec_cosr.f \
vec_negb.f \
vec_negd.f vec_negi.f vec_negr.f vec_negub.f vec_neguw.f vec_negw.f \
vec_signb.f vec_signd.f \
vec_signi.f vec_signr.f vec_signub.f vec_signuw.f vec_signw.f \
vec_sind.f vec_sindd.f vec_sindr.f vec_sinhd.f vec_sinhr.f vec_sinr.f \
vec_tand.f vec_tandd.f vec_tandr.f \
vec_tanhd.f vec_tanhr.f vec_tanr.f \
num_clearerr.f num_wasok.f num_geterr.f

GEN_F_ROUTINES = $(PRM_N_GEN:.gn=.f) $(PRM_CVT_ITO_GEN:.gbBdkrwW=.f) \
$(PRM_CVT_KTO_GEN:.gbBdirwW=.f) $(PRM_CVT_DTO_GEN:.gbBikrwW=.f) \
$(PRM_CVT_RTO_GEN:.gbBdikwW=.f) $(PRM_CVT_WTO_GEN:.gbBdikrW=.f) \
$(PRM_CVT_BTO_GEN:.gBdikrwW=.f) $(PRM_CVT_UBTO_GEN:.gbdikrwW=.f) \
$(PRM_CVT_UWTO_GEN:.gbBdikrw=.f)

PRM_N_GEN = \
val_addx.gn val_dimx.gn val_divx.gn val_expx.gn  \
val_idvx.gn val_intx.gn val_lg10x.gn val_logx.gn val_maxx.gn \
val_minx.gn val_modx.gn val_mulx.gn val_nintx.gn val_pwrx.gn \
val_sqrtx.gn val_subx.gn val_xtox.gn \
vec_addx.gn vec_dimx.gn vec_divx.gn \
vec_expx.gn vec_idvx.gn vec_intx.gn vec_lg10x.gn vec_logx.gn \
vec_maxx.gn vec_minx.gn vec_modx.gn vec_mulx.gn vec_nintx.gn \
vec_pwrx.gn vec_sqrtx.gn vec_subx.gn vec_xtox.gn

# Each source type should be converted to all types except
# the source type (which is handled separately)
PRM_CVT_ITO_GEN = \
val_itox.gbBdkrwW vec_itox.gbBdkrwW

PRM_CVT_KTO_GEN = \
val_ktox.gbBdirwW vec_ktox.gbBdirwW

PRM_CVT_DTO_GEN = \
val_dtox.gbBikrwW vec_dtox.gbBikrwW

PRM_CVT_RTO_GEN = \
val_rtox.gbBdikwW vec_rtox.gbBdikwW

PRM_CVT_WTO_GEN = \
val_wtox.gbBdikrW vec_wtox.gbBdikrW

PRM_CVT_BTO_GEN = \
val_btox.gBdikrwW vec_btox.gBdikrwW

PRM_CVT_UBTO_GEN = \
val_ubtox.gbdikrwW vec_ubtox.gbdikrwW

PRM_CVT_UWTO_GEN = \
val_uwtox.gbBdikrw vec_uwtox.gbBdikrw

# Module num1_cvt.c is not in fact necessary in cases where the
# Fortran compiler supports the IZEXT (and related) intrinsic.  We
# could try to make configure.ac spot these cases, and manipulate
# LIBOBJS, but there's no real need, since num1_cvt.c is inside a big
# #if which makes it define nothing if the intrinsic is defined.  Thus
# we can compile it unconditionally.
PLATFORM_C = num_han.c num1_cvt.c

#  Generic C functions.
CGENERIC_ROUTINES = vecBtox.cgen vecDtox.cgen vecItox.cgen \
vecKtox.cgen vecFtox.cgen vecUBtox.cgen vecUWtox.cgen vecWtox.cgen

# The .c files which are built from the above .cgen files.
BUILT_C_ROUTINES = $(CGENERIC_ROUTINES:.cgen=.c)


PLATFORM_SOURCE = num_han.c num_han.cdefault num_han.c_sun num_han.c_dec_osf \
num_han.cmips num_han.c_sun_solaris

#  List of non platform-specific include files.
NORMAL_INCLUDES = \
	NUM_DEC_B NUM_DEC_D NUM_DEC_I \
	NUM_DEC_R NUM_DEC_UB NUM_DEC_UW NUM_DEC_W \
	NUM_DEC_CVT \
	NUM_DEF_B NUM_DEF_I \
	NUM_DEF_UB NUM_DEF_UW NUM_DEF_W \
	NUM_DEC_K NUM_DEF_K \
	NUM_DEF NUM_DEC \
	PRM_PAR prm_par.h prm.h prm_cgen.h



# List of configured include files.  Each of these has a
# corresponding .F file, which is configured by being run through the
# Fortran preprocessor
#
# NUM_CMN         uses the non-standard VOLATILE keyword.
# NUM_DEF_{R,D}   uses non-standard SIND (etc) intrinsics.
# NUM_DEF_CVT     uses non-standard intrinsics when available,
#                 and functions from num1_cvt.c otherwise.
PLATFORM_INCLUDES = NUM_DEF_D NUM_DEF_R NUM_DEF_CVT
PLATFORM_PRIVATE_INCLUDES = NUM_CMN

# Here are the dependencies for these files
#NUM_CMN: NUM_CMN.F
#NUM_DEF_D: NUM_DEF_D.F
#NUM_DEF_R: NUM_DEF_R.F
#NUM_DEF_CVT: NUM_DEF_CVT.F

# These files are created by being run through the Fortran preprocessor.
# Define the rule to do this.
#
# Need to add the .F suffix explicitly, since there are no .F sources
# in this component currently.
SUFFIXES = .F

# Run a .F file through the fpp preprocessor, to produce a file with no
# extension.  Remove fpp and Fortran comments, and blank lines.
# The following deals with case-insensitive filesystems, on which
# foo.f and foo.F would be the same file.  FPP_OUTPUT is
# either "" (in which case the preprocessor writes to foo.f, and
# the filesystem is presumably case-sensitive) or ">$@".
.F:
	rm -f $@
	$(FPP) $(FPPFLAGS) $(DEFAULT_INCLUDES) $(CPPFLAGS) $< $(FPP_OUTPUT)
	test -f $@ || mv $(<:.F=.f) $@
	rm -f $(<:.F=.tmp)
	echo "* Generated from $< using preprocessor $(FPP)" >$(<:.F=.tmp)
	echo "* DO NOT EDIT THIS FILE" >>$(<:.F=.tmp)
	grep '^ .*[^ ]' $@ >>$(<:.F=.tmp)
	rm -f $@
	mv $(<:.F=.tmp) $@



PRIVATE_INCLUDES = PRM_CONST



# PRM_PAR and prm_par.h are generated from information in float.h and limits.h
PRM_PAR prm_par.h: make-prm-par$(EXEEXT)
	./make-prm-par -fPRM_PAR -cprm_par.h

BUILT_SOURCES = $(include_MESSAGES) $(noinst_MESSAGES) PRM_PAR prm_par.h \
	$(PLATFORM_INCLUDES) $(PLATFORM_PRIVATE_INCLUDES) prm_cgen.h

prm_cgen.h: prm_cgen.h.source make_h
	./make_h

# Tests

TESTS = prm_test
check_PROGRAMS = $(TESTS)

prm_test_SOURCES = prm_test.f
prm_test_LDADD = libprm.la libprm_a.la

# Files for distribution that are not picked up elsewhere.
EXTRA_DIST = $(PLATFORM_SOURCE) $(PLATFORM_INCLUDES:=.F) \
	$(PRIVATE_INCLUDES) $(PLATFORM_PRIVATE_INCLUDES:=.F) \
        prm_err.msg

dist_starnews_DATA = prm.news
dist_pkgdata_DATA = LICENCE

CLEANFILES = PRM_ERR

.gn.f:
	$(FGENERIC)  -t n $<

.gbBdirwW.f:
	$(FGENERIC)  -t bBdirwW $<

.gbBdkrwW.f:
	$(FGENERIC)  -t bBdkrwW $<

.gbBikrwW.f:
	$(FGENERIC)  -t bBikrwW $<

.gbBdikwW.f:
	$(FGENERIC)  -t bBdikwW $<

.gbBdikrW.f:
	$(FGENERIC)  -t bBdikrW $<

.gBdikrwW.f:
	$(FGENERIC)  -t BdikrwW $<

.gbdikrwW.f:
	$(FGENERIC)  -t bdikrwW $<

.gbBdikrw.f:
	$(FGENERIC)  -t bBdikrw $<


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
