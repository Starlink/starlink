## Process this file with automake to produce Makefile.in

## In automake terminology, this is the `bin' prefix to the `SCRIPTS' primary.
dist_bin_SCRIPTS = agp_link agp_link_adam

# Need to use include files from parent
AM_FCFLAGS = -I..

## List libraries which are to be built, and installed in `lib'.  Note
## that the file extension is .la, not .a or .so.  This is the
## extension for a `libtool library' -- the actual libraries will be
## installed using whatever names are appropriate for the platform.
## The primary LTLIBRARIES requests that we build both sharable
## and static libraries.

# These libraries will be linked into the main library in the parent
noinst_LTLIBRARIES = libagp_base.la

# AGP library containing the shared files
libagp_base_la_SOURCES = $(AGP_F_ROUTINES)

# Dynamicall generated
AGP_F_DYN = agp1_init.f
DYN_INCLUDES = AGP_CONST AGP_COM
AGP_DYNBLT = $(AGP_F_DYN) $(DYN_INCLUDES)

AGP_F_NONDYN = agp_nview.f agp_sview.f agp_activ.f \
              agi1_defba.f agi1_gwnam.f agp1_envgt.f \
              agp1_fndty.f agp1_pgbeg.f agp_curag.f \
              agp_gdlst.f agp1_trans.f agp_aspec.f \
              agp1_chkdv.f agi1_endgns.f

AGP_F_ROUTINES = $(AGP_F_NONDYN) $(AGP_F_DYN)

BUILT_ROUTINES = $(AGP_DYNBLT)

AGP_DYNSRC = make_agp_files pgnames.txt

$(AGP_DYNBLT):  $(AGP_DYNSRC)
	./make_agp_files

# Force dependencies on dynamic files since autoconf can not work
# this out itself
agp1_chkdv.lo: AGP_CONST AGP_COM
agp1_fndty.lo: AGP_CONST AGP_COM
agp1_pgbeg.lo: AGP_CONST AGP_COM
agp1_trans.lo: AGP_CONST AGP_COM
agp_aspec.lo: AGP_CONST AGP_COM
agp_curag.lo: AGP_CONST AGP_COM
agp1_gdlst.lo: AGP_CONST AGP_COM


EXTRA_DIST = make_agp_files pgnames.txt


CLEANFILES = agp1_init.f AGP_COM AGP_CONST

