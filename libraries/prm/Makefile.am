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



F_ROUTINES = val_absb.f val_absd.f val_absi.f \
val_absr.f val_absub.f val_absuw.f val_absw.f val_acosd.f val_acosr.f \
val_acsdd.f val_acsdr.f val_addb.f val_addd.f val_addi.f val_addr.f \
val_addub.f val_adduw.f val_addw.f val_asind.f val_asinr.f val_asndd.f \
val_asndr.f val_at2dd.f val_at2dr.f val_atand.f val_atanr.f val_atn2d.f \
val_atn2r.f val_atndd.f val_atndr.f val_btob.f val_btod.f val_btoi.f \
val_btor.f val_btoub.f val_btouw.f val_btow.f val_cosd.f val_cosdd.f \
val_cosdr.f val_coshd.f val_coshr.f val_cosr.f val_dimb.f val_dimd.f \
val_dimi.f val_dimr.f val_dimub.f val_dimuw.f val_dimw.f val_divb.f \
val_divd.f val_divi.f val_divr.f val_divub.f val_divuw.f val_divw.f \
val_dtob.f val_dtod.f val_dtoi.f val_dtor.f val_dtoub.f val_dtouw.f \
val_dtow.f val_expb.f val_expd.f val_expi.f val_expr.f val_expub.f \
val_expuw.f val_expw.f val_idvb.f val_idvd.f val_idvi.f val_idvr.f \
val_idvub.f val_idvuw.f val_idvw.f val_intb.f val_intd.f val_inti.f \
val_intr.f val_intub.f val_intuw.f val_intw.f val_itob.f val_itod.f \
val_itoi.f val_itor.f val_itoub.f val_itouw.f val_itow.f val_lg10b.f \
val_lg10d.f val_lg10i.f val_lg10r.f val_lg10ub.f val_lg10uw.f \
val_lg10w.f val_logb.f val_logd.f val_logi.f val_logr.f val_logub.f \
val_loguw.f val_logw.f val_maxb.f val_maxd.f val_maxi.f val_maxr.f \
val_maxub.f val_maxuw.f val_maxw.f val_minb.f val_mind.f val_mini.f \
val_minr.f val_minub.f val_minuw.f val_minw.f val_modb.f val_modd.f \
val_modi.f val_modr.f val_modub.f val_moduw.f val_modw.f val_mulb.f \
val_muld.f val_muli.f val_mulr.f val_mulub.f val_muluw.f val_mulw.f \
val_negb.f val_negd.f val_negi.f val_negr.f val_negub.f val_neguw.f \
val_negw.f val_nintb.f val_nintd.f val_ninti.f val_nintr.f val_nintub.f \
val_nintuw.f val_nintw.f val_pwrb.f val_pwrd.f val_pwri.f val_pwrr.f \
val_pwrub.f val_pwruw.f val_pwrw.f val_rtob.f val_rtod.f val_rtoi.f \
val_rtor.f val_rtoub.f val_rtouw.f val_rtow.f val_signb.f val_signd.f \
val_signi.f val_signr.f val_signub.f val_signuw.f val_signw.f \
val_sind.f val_sindd.f val_sindr.f val_sinhd.f val_sinhr.f val_sinr.f \
val_sqrtb.f val_sqrtd.f val_sqrti.f val_sqrtr.f val_sqrtub.f \
val_sqrtuw.f val_sqrtw.f val_subb.f val_subd.f val_subi.f val_subr.f \
val_subub.f val_subuw.f val_subw.f val_tand.f val_tandd.f val_tandr.f \
val_tanhd.f val_tanhr.f val_tanr.f val_ubtob.f val_ubtod.f val_ubtoi.f \
val_ubtor.f val_ubtoub.f val_ubtouw.f val_ubtow.f val_uwtob.f \
val_uwtod.f val_uwtoi.f val_uwtor.f val_uwtoub.f val_uwtouw.f \
val_uwtow.f val_wtob.f val_wtod.f val_wtoi.f val_wtor.f val_wtoub.f \
val_wtouw.f val_wtow.f vec_absb.f vec_absd.f vec_absi.f vec_absr.f \
vec_absub.f vec_absuw.f vec_absw.f vec_acosd.f vec_acosr.f vec_acsdd.f \
vec_acsdr.f vec_addb.f vec_addd.f vec_addi.f vec_addr.f vec_addub.f \
vec_adduw.f vec_addw.f vec_asind.f vec_asinr.f vec_asndd.f vec_asndr.f \
vec_at2dd.f vec_at2dr.f vec_atand.f vec_atanr.f vec_atn2d.f vec_atn2r.f \
vec_atndd.f vec_atndr.f vec_btob.f vec_btod.f vec_btoi.f vec_btor.f \
vec_btoub.f vec_btouw.f vec_btow.f vec_cosd.f vec_cosdd.f vec_cosdr.f \
vec_coshd.f vec_coshr.f vec_cosr.f vec_dimb.f vec_dimd.f vec_dimi.f \
vec_dimr.f vec_dimub.f vec_dimuw.f vec_dimw.f vec_divb.f vec_divd.f \
vec_divi.f vec_divr.f vec_divub.f vec_divuw.f vec_divw.f vec_dtob.f \
vec_dtod.f vec_dtoi.f vec_dtor.f vec_dtoub.f vec_dtouw.f vec_dtow.f \
vec_expb.f vec_expd.f vec_expi.f vec_expr.f vec_expub.f vec_expuw.f \
vec_expw.f vec_idvb.f vec_idvd.f vec_idvi.f vec_idvr.f vec_idvub.f \
vec_idvuw.f vec_idvw.f vec_intb.f vec_intd.f vec_inti.f vec_intr.f \
vec_intub.f vec_intuw.f vec_intw.f vec_itob.f vec_itod.f vec_itoi.f \
vec_itor.f vec_itoub.f vec_itouw.f vec_itow.f vec_lg10b.f vec_lg10d.f \
vec_lg10i.f vec_lg10r.f vec_lg10ub.f vec_lg10uw.f vec_lg10w.f \
vec_logb.f vec_logd.f vec_logi.f vec_logr.f vec_logub.f vec_loguw.f \
vec_logw.f vec_maxb.f vec_maxd.f vec_maxi.f vec_maxr.f vec_maxub.f \
vec_maxuw.f vec_maxw.f vec_minb.f vec_mind.f vec_mini.f vec_minr.f \
vec_minub.f vec_minuw.f vec_minw.f vec_modb.f vec_modd.f vec_modi.f \
vec_modr.f vec_modub.f vec_moduw.f vec_modw.f vec_mulb.f vec_muld.f \
vec_muli.f vec_mulr.f vec_mulub.f vec_muluw.f vec_mulw.f vec_negb.f \
vec_negd.f vec_negi.f vec_negr.f vec_negub.f vec_neguw.f vec_negw.f \
vec_nintb.f vec_nintd.f vec_ninti.f vec_nintr.f vec_nintub.f \
vec_nintuw.f vec_nintw.f vec_pwrb.f vec_pwrd.f vec_pwri.f vec_pwrr.f \
vec_pwrub.f vec_pwruw.f vec_pwrw.f vec_rtob.f vec_rtod.f vec_rtoi.f \
vec_rtor.f vec_rtoub.f vec_rtouw.f vec_rtow.f vec_signb.f vec_signd.f \
vec_signi.f vec_signr.f vec_signub.f vec_signuw.f vec_signw.f \
vec_sind.f vec_sindd.f vec_sindr.f vec_sinhd.f vec_sinhr.f vec_sinr.f \
vec_sqrtb.f vec_sqrtd.f vec_sqrti.f vec_sqrtr.f vec_sqrtub.f \
vec_sqrtuw.f vec_sqrtw.f vec_subb.f vec_subd.f vec_subi.f vec_subr.f \
vec_subub.f vec_subuw.f vec_subw.f vec_tand.f vec_tandd.f vec_tandr.f \
vec_tanhd.f vec_tanhr.f vec_tanr.f vec_ubtob.f vec_ubtod.f vec_ubtoi.f \
vec_ubtor.f vec_ubtoub.f vec_ubtouw.f vec_ubtow.f vec_uwtob.f \
vec_uwtod.f vec_uwtoi.f vec_uwtor.f vec_uwtoub.f vec_uwtouw.f \
vec_uwtow.f vec_wtob.f vec_wtod.f vec_wtoi.f vec_wtor.f vec_wtoub.f \
vec_wtouw.f vec_wtow.f \
num_clearerr.f num_wasok.f num_geterr.f

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


