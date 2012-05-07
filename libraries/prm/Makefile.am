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

NONGEN_F_ROUTINES = val_absb.f val_absd.f val_absi.f \
val_absr.f val_absub.f val_absuw.f val_absw.f val_acosd.f val_acosr.f \
val_acsdd.f val_acsdr.f val_asind.f val_asinr.f val_asndd.f \
val_asndr.f val_at2dd.f val_at2dr.f val_atand.f val_atanr.f val_atn2d.f \
val_atn2r.f val_atndd.f val_atndr.f val_btod.f val_btoi.f \
val_btor.f val_btoub.f val_btouw.f val_btow.f val_cosd.f val_cosdd.f \
val_cosdr.f val_coshd.f val_coshr.f val_cosr.f \
val_dtob.f val_dtoi.f val_dtor.f val_dtoub.f val_dtouw.f \
val_dtow.f val_itob.f val_itod.f \
val_itor.f val_itoub.f val_itouw.f val_itow.f \
val_negb.f val_negd.f val_negi.f val_negr.f val_negub.f val_neguw.f \
val_negw.f val_rtob.f val_rtod.f val_rtoi.f \
val_rtoub.f val_rtouw.f val_rtow.f val_signb.f val_signd.f \
val_signi.f val_signr.f val_signub.f val_signuw.f val_signw.f \
val_sind.f val_sindd.f val_sindr.f val_sinhd.f val_sinhr.f val_sinr.f \
val_tand.f val_tandd.f val_tandr.f \
val_tanhd.f val_tanhr.f val_tanr.f val_ubtob.f val_ubtod.f val_ubtoi.f \
val_ubtor.f val_ubtouw.f val_ubtow.f val_uwtob.f \
val_uwtod.f val_uwtoi.f val_uwtor.f val_uwtoub.f \
val_uwtow.f val_wtob.f val_wtod.f val_wtoi.f val_wtor.f val_wtoub.f \
val_wtouw.f \
vec_absb.f vec_absd.f vec_absi.f vec_absr.f \
vec_absub.f vec_absuw.f vec_absw.f vec_acosd.f vec_acosr.f vec_acsdd.f \
vec_acsdr.f vec_asind.f vec_asinr.f vec_asndd.f vec_asndr.f \
vec_at2dd.f vec_at2dr.f vec_atand.f vec_atanr.f vec_atn2d.f vec_atn2r.f \
vec_atndd.f vec_atndr.f vec_btod.f vec_btoi.f vec_btor.f \
vec_btoub.f vec_btouw.f vec_btow.f vec_cosd.f vec_cosdd.f vec_cosdr.f \
vec_coshd.f vec_coshr.f vec_cosr.f vec_dtob.f \
vec_dtoi.f vec_dtor.f vec_dtoub.f vec_dtouw.f vec_dtow.f \
vec_itob.f vec_itod.f \
vec_itor.f vec_itoub.f vec_itouw.f vec_itow.f vec_negb.f \
vec_negd.f vec_negi.f vec_negr.f vec_negub.f vec_neguw.f vec_negw.f \
vec_rtob.f vec_rtod.f vec_rtoi.f \
vec_rtoub.f vec_rtouw.f vec_rtow.f vec_signb.f vec_signd.f \
vec_signi.f vec_signr.f vec_signub.f vec_signuw.f vec_signw.f \
vec_sind.f vec_sindd.f vec_sindr.f vec_sinhd.f vec_sinhr.f vec_sinr.f \
vec_tand.f vec_tandd.f vec_tandr.f \
vec_tanhd.f vec_tanhr.f vec_tanr.f vec_ubtob.f vec_ubtod.f vec_ubtoi.f \
vec_ubtor.f vec_ubtouw.f vec_ubtow.f vec_uwtob.f \
vec_uwtod.f vec_uwtoi.f vec_uwtor.f vec_uwtoub.f \
vec_uwtow.f vec_wtob.f vec_wtod.f vec_wtoi.f vec_wtor.f vec_wtoub.f \
vec_wtouw.f \
num_clearerr.f num_wasok.f num_geterr.f

GEN_F_ROUTINES = $(PRM_N_GEN:.gn=.f)

PRM_N_GEN = \
val_addx.gn val_dimx.gn val_divx.gn val_expx.gn  \
val_idvx.gn val_intx.gn val_lg10x.gn val_logx.gn val_maxx.gn \
val_minx.gn val_modx.gn val_mulx.gn val_nintx.gn val_pwrx.gn \
val_sqrtx.gn val_subx.gn val_xtox.gn \
vec_addx.gn vec_dimx.gn vec_divx.gn \
vec_expx.gn vec_idvx.gn vec_intx.gn vec_lg10x.gn vec_logx.gn \
vec_maxx.gn vec_minx.gn vec_modx.gn vec_mulx.gn vec_nintx.gn \
vec_pwrx.gn vec_sqrtx.gn vec_subx.gn vec_xtox.gn

# Module num1_cvt.c is not in fact necessary in cases where the
# Fortran compiler supports the IZEXT (and related) intrinsic.  We
# could try to make configure.ac spot these cases, and manipulate
# LIBOBJS, but there's no real need, since num1_cvt.c is inside a big
# #if which makes it define nothing if the intrinsic is defined.  Thus
# we can compile it unconditionally.
PLATFORM_C = num_han.c num1_cvt.c

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
	PRM_PAR prm_par.h



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
	$(PLATFORM_INCLUDES) $(PLATFORM_PRIVATE_INCLUDES)


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

