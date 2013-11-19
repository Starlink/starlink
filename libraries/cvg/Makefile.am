## Process this file with automake to produce Makefile.in

cincludedir = $(includedir)/star

lib_LTLIBRARIES = libcvg.la libcvg_adam.la
dist_bin_SCRIPTS = cvg_link cvg_link_adam

libcvg_la_SOURCES = \
        $(PUBLIC_INCLUDES) \
        $(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) \
        $(GEN_F_ROUTINES)

libcvg_adam_la_SOURCES = \
        $(PUBLIC_INCLUDES) \
        $(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) \
        $(ADAM_C_ROUTINES) \
        $(ADAM_F_ROUTINES) \
        $(GEN_F_ROUTINES)

include_HEADERS = $(PUBLIC_F_INCLUDES)
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

#  Define test programs.
TESTS = test1
check_PROGRAMS = test1
test1_SOURCES = cvg_test.c
test1_LDADD = libcvg.la `$(srcdir)/cvg_link` @FCLIBS@

PUBLIC_F_INCLUDES = CVG_PAR
PUBLIC_C_INCLUDES = cvg.h
PUBLIC_INCLUDES = $(PUBLIC_F_INCLUDES) $(PUBLIC_C_INCLUDES)

#  List of C functions required by both ADAM and standalone
#  libraries.
BASIC_C_ROUTINES = cvg.c

#  List of Fortran routines required by both ADAM and standalone
#  libraries.
BASIC_F_ROUTINES = cvg_pcadc.f cvg_fioer.f cvg_bt2ft.f cvg_fc2hd.f \
cvg_ft2bt.f cvg_hd2fc.f cvg_new.f cvg_close.f cvg_showheader.f \
cvg_wprov.f

#  Additional Fortran routines required for the ADAM library.
ADAM_F_ROUTINES = cvg_scadc.f cvg_creat.f

#  Additional C routines required for the ADAM library.
ADAM_C_ROUTINES =  cvg_adam.c

#  All generic Fortran source files.
GEN_F_ROUTINES = $(CVG_N_GEN:.gn=.f)

# All numeric types
CVG_N_GEN =  cvg_retr.gn


dist_starnews_DATA = cvg.news
dist_pkgdata_DATA = LICENCE

# Need to make sure that a maintainer clean really removes the
# output of generic
MAINTAINERCLEANFILES = $(GEN_F_ROUTINES)


.gn.f:
	$(FGENERIC) $(<)


