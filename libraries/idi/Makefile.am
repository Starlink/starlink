## Process this file with automake to produce Makefile.in

bin_SCRIPTS = idi_link idi_link_adam

lib_LTLIBRARIES = libidi.la libidi_adam.la

libidi_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(PRIVATE_INCLUDES) \
	$(F_ROUTINES) \
	$(C_ROUTINES)

libidi_adam_la_SOURCES = $(ADAM_F_ROUTINES) \
	$(PRIVATE_INCLUDES)


include_HEADERS = $(PUBLIC_INCLUDES)


PUBLIC_INCLUDES = \
	IDI_ERR IDI_PAR idi_err.h idi.h

PRIVATE_INCLUDES = idistruct_e.h kwm.h vdm.h idi_did \
idi_compar device.dep iii.h x11defs.h idifuncs.h


F_ROUTINES = idi.f ikndummy.f

ADAM_F_ROUTINES = idi_adam.f

C_ROUTINES = idiic.c idiutil.c iig.c iim.c kwm.c idienv.c idilocal.c \
iic.c iii.c iir.c vdm.c idiextra.c idiother.c iid.c iil.c iiz.c	     \
vdmdd.c

BUILT_SOURCES = IDI_ERR idi_err.h

# We do not want to install dctprep

noinst_PROGRAMS = dctprep
dctprep_SOURCES = dctprep.c
dctprep_LDADD = libidi.la `gwm_link` `gns_link` `cnf_link` $(X_LIBS) $(X_EXTRA_LIBS) $(X_PRE_LIBS) -lX11 @FCLIBS@

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

idi_test_TASKS =
idi_test_SOURCES = idi_test.f
idi_test_LDADD = $(LDADD) `$(srcdir)/idi_link_adam` `ndf_link_adam` -lpthread

