## Process this file with automake to produce Makefile.in

dist_bin_SCRIPTS = idi_link idi_link_adam

lib_LTLIBRARIES = libidi.la libidi_adam.la

libidi_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(include_MESSAGES) \
	$(PRIVATE_INCLUDES) \
	$(F_ROUTINES) \
	$(C_ROUTINES)

libidi_adam_la_SOURCES = $(ADAM_F_ROUTINES) \
	$(PRIVATE_INCLUDES)

libidi_adam_la_LIBADD = libidi.la

include_HEADERS = $(PUBLIC_INCLUDES)
include_MESSAGES = IDI_ERR idi_err.h

# This etc file is a binary file, so should not go into share
staretc_DATA = xworks.dat xworks.dct

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

AM_CFLAGS = $(X_CFLAGS)

PUBLIC_INCLUDES = IDI_PAR idi.h

PRIVATE_INCLUDES = idistruct_e.h kwm.h vdm.h idi_did \
idi_compar device.dep iii.h x11defs.h idifuncs.h


F_ROUTINES = idi.f ikndummy.f

ADAM_F_ROUTINES = idi_adam.f

C_ROUTINES = idi_gbl.c idiic.c idiutil.c iig.c iim.c kwm.c idienv.c \
idilocal.c iic.c iii.c iir.c vdm.c idiextra.c idiother.c iid.c iil.c \
iiz.c	vdmdd.c

BUILT_SOURCES = IDI_ERR idi_err.h

# We do not want to install dctprep

noinst_PROGRAMS = dctprep
dctprep_SOURCES = dctprep.c
dctprep_LDADD = idienv.lo kwm.lo $(X_LIBS) $(X_EXTRA_LIBS) $(X_PRE_LIBS) -lX11 @FCLIBS@

xworks.dat: xworks.dct dctprep
	echo y | IDI_DIR=./ IDI_XDT=xworks ./dctprep
	echo


# Testing

# idi_test.f is an ADAM task

TESTS = gnsrun_idi idi_test
check_PROGRAMS = gnsrun_idi
check_MONOLITHS = idi_test

gnsrun_idi_SOURCES = gnsrun_idi.f
# LDADD is the expansion of ref_link, with -lref replaced by libref.la
gnsrun_idi_LDADD = libidi.la `gwm_link` `gns_link` `cnf_link` $(X_LIBS) $(X_EXTRA_LIBS) $(X_PRE_LIBS) -lX11

# idi_test requires ndf and lpthread?

# no variable idi_test_TASKS -- idi_test is an ATASK
idi_test_SOURCES = idi_test.f
idi_test_LDADD = libidi_adam.la `ndf_link_adam` `gwm_link_adam` \
`gns_link_adam` `cnf_link_adam` $(X_LIBS) $(X_EXTRA_LIBS) $(X_PRE_LIBS) -lX11

dist_starnews_DATA = idi.news
dist_pkgdata_DATA = LICENCE

EXTRA_DIST = xworks.dct

CLEANFILES = xworks.dat idi_test.ifc idi_err.h IDI_ERR

