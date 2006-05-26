## Process this file with automake to produce Makefile.in

cincludedir = $(includedir)/star

lib_LTLIBRARIES = libatl.la 
dist_bin_SCRIPTS = atl_link 

libatl_la_SOURCES = \
        $(PUBLIC_INCLUDES) \
        $(BASIC_C_ROUTINES) \
        $(BASIC_F_ROUTINES) 

include_HEADERS = $(PUBLIC_F_INCLUDES)
cinclude_HEADERS = $(PUBLIC_C_INCLUDES)

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

#  Define test programs.
TESTS = test1 
check_PROGRAMS = test1
test1_SOURCES = atl_test.f
test1_LDADD = libatl.la `$(srcdir)/atl_link`

PUBLIC_F_INCLUDES = ATL_PAR 
PUBLIC_C_INCLUDES = alt.h
PUBLIC_INCLUDES = $(PUBLIC_F_INCLUDES) $(PUBLIC_C_INCLUDES)

#  List of C functions required by both ADAM and standalone 
# libraries.
BASIC_C_ROUTINES = atl.c

#  List of Fortran routines required by both ADAM and standalone 
# libraries.
BASIC_F_ROUTINES = atl_axtrm.f

dist_starnews_DATA = atl.news
dist_pkgdata_DATA = LICENCE

