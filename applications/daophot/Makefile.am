## Process this file with automake to produce Makefile.in

##  List of Fortran routines required for building the package.

F_SUB1 = addstar.f allstsubs.f find.f \
fotometry.f fudge.f group.f iosubs.f mathsubs.f nstar.f pckpsf.f \
peak.f psf.f sort.f substar.f

F_SUB2 = aif_antmp.f aif_temp.f dao_alloc.f dao_askfile.f dao_attach.f \
dao_byebye.f dao_case.f dao_clfile.f dao_clpic.f dao_coppic.f dao_deall.f \
dao_delpic.f dao_fabort.f dao_infile.f dao_list.f \
dao_oops.f dao_outfil.f dao_ovrwrt.f dao_rdaray.f dao_wraray.f

##  List of external include files.  These are files which are required
##  in order to build the package but form part of other, externally
##  installed packages.

EXTERNAL_INCLUDES = SAE_PAR DAT_ERR DAT_PAR GKS_PAR PRM_PAR

##  List of private include files.  These are include files
##  which form part of the package and are required in order to build
##  it, but which are not required by users of it.

PRIVATE_INCLUDES = ndf_cmn

##  List of ancilliary data files. 

DATA_FILES = allstar.opt daophot.opt photo.opt

##  Startup script.  This is the file that must be executed (sourced)
##  by a user of this package in order to define appropriate aliases
##  and environment variables.  It is listed separately from the public 
##  scripts as it is edited by the installation procedure.

STARTUP_SCRIPT = daophot.csh 

##  List of object library files to be built.

OBJECT_LIBRARIES = libdaophot.a libdaosubs.a


bindatadir = $(bindir)

bindata_DATA = $(DATA_FILES)
noinst_HEADERS = $(PRIVATE_INCLUDES)
bin_SCRIPTS = $(STARTUP_SCRIPT)
bin_PROGRAMS = allstar daophot
bin_MONOLITHS = daocurs daogrey daoplot
noinst_LIBRARIES = $(OBJECT_LIBRARIES)

libdaophot_a_SOURCES = $(F_SUB1)
libdaosubs_a_SOURCES = $(F_SUB2)

allstar_SOURCES = $(OBJECT_LIBRARIES) allstar.f
allstar_LDADD = $(LDADD) `daophot_link`

daophot_SOURCES = $(OBJECT_LIBRARIES) daophot.f
daophot_LDADD = $(LDADD) `daophot_link`

daocurs_SOURCES = $(OBJECT_LIBRARIES) daocurs.f
daocurs_LDADD = $(LDADD) `daophot_link`

daogrey_SOURCES = $(OBJECT_LIBRARIES) daogrey.f
daogrey_LDADD = $(LDADD) `daogrey_link`

daoplot_SOURCES = $(OBJECT_LIBRARIES) daoplot.f
daoplot_LDADD = $(LDADD) `daoplot_link`



## List libraries which are to be built, and installed in `lib'.  Note
## that the file extension is .la, not .a or .so.  This is the
## extension for a `libtool library' -- the actual libraries will be
## installed using whatever names are appropriate for the platform.
## The primary LTLIBRARIES requests that we build both sharable
## and static libraries.
lib_LTLIBRARIES = libcnf.la

## Declare the sources for the libcnf.la library.  Note that the
## prefix which corresponds to the libcnf.la library has non-letters
## canonicalised to underscores.
libcnf_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(BLOCK_DATA_FILES) \
	$(C_ROUTINES) \
	CNF_PAR

## List headers which are to be installed -- these are built first if necessary
include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = CNF_PAR f77.h cnf.h

C_ROUTINES = cnfCopyf.c cnfCreat.c cnfCref.c cnfCrefa.c cnfCreib.c \
	etc.c

## Note that we cannot call this variable just BLOCK_DATA, since automake
## interprets this as a `primary' DATA
BLOCK_DATA_FILES = blocks.f

## We can add any other Makefile rules we require, and automake will
## copy them through to the output.

# f77.h is a duplicate of cnf.h, maintained as such because old software
# (actually including some of the code in this library -- that should
# probably be fixed!) uses it under its old name
f77.h: cnf.h
	$(LN_S) cnf.h f77.h

## The file f77.h is input by some of the C_ROUTINES but, because it's
## generated, it doesn't exist in a fresh checkout. We can't rely on
## the generated dependency checking to sort this out, because that
## isn't generated until after the library is built the first time.
## Declaring the file as a `built source' simply ensures that the file
## is generated at the very beginning of the build.  However the
## targets listed here _won't_ be built unless they are listed in some
## other dependency above (we've done it in PUBLIC_INCLUDES,
## which is a dependency of libcnf_la_SOURCES).
BUILT_SOURCES = f77.h
