## Process this file with automake to produce Makefile.in


CGENERIC_ROUTINES = cupidgcfindmax.cgen cupidrms.cgen \
cupidgcsetinit.cgen cupidgcupdatearrays.cgen cupidgcfit.cgen \
cupidgcprofwidth.cgen cupidgcdump.cgen cupidsumclumps.cgen \
cupidcfscan.cgen cupidcfclump.cgen \
cupidrcheckface.cgen cupidrcopyline.cgen cupidrinitedges.cgen \
cupidfwmain.cgen cupidndfclump.cgen cupidfwjoin.cgen \
cupidfwpixelsets.cgen \
cupidfindback1.cgen cupidfindback2.cgen cupidfindback3.cgen \
cupidfindback4.cgen cupidfindback10.cgen

# The .c files which are built from the above .cgen files.
BUILT_C_ROUTINES = $(CGENERIC_ROUTINES:.cgen=.c)

BUILT_SOURCES = $(BUILT_C_ROUTINES)

# All the .c files which are put into the libcupidsub library. This
# includes the type-specific files which are created from the above
# generic .cgen files.
C_ROUTINES = cupidclumpfind.c cupidgaussclumps.c cupidconfigD.c \
cupidconfigrms.c cupidellipsedesc.c cupidellipsedescnew.c cupidpolygondesc.c \
cupidconfigI.c cupidgclistclump.c cupidgcmodel.c cupidgcchisq.c \
cupidgccalcf.c cupidgccalcg.c cupidranval.c cupiddumpd.c cupiddumpf.c \
cupidcferode.c cupidstoreconfig.c cupidstoreclumps.c cupiddatcopy.c \
cupidcfdeleteps.c cupidcffreeps.c cupidcfxfer.c cupidcfaddpixel.c \
cupidcflevels.c cupiddumpi.c cupidclumpdesc.c cupidcfnebs.c cupidedges.c \
cupidcfidl.c cupidcfmakeps.c cupidreinhold.c cupidredges.c cupidrca.c \
cupidrfillline.c cupidrfillclumps.c cupidrca2.c cupidfellwalker.c \
cupidgcndfclump.c cupiddefminpix.c cupidretrieveconfig.c \
cupidboxiterator.c cupidnextit.c cupidclumpinfo1.c cupidfindback0.c \
$(BUILT_C_ROUTINES)

noinst_LTLIBRARIES = libcupidsub.la

libcupidsub_la_SOURCES = $(C_ROUTINES) $(CGENERIC_ROUTINES)

## Ensure we use the 8-byte NDF interface
AM_CFLAGS = -I.. -std=c99 -DNDF_I8

$(C_ROUTINES:.c=.o): ../cupid.h

# Create the .c file which defines type-specific code generated from each
# .cgen file.
$(CGENERIC_ROUTINES:.cgen=.c):
	@echo "/*" > $@
	@echo "*  Name:" >> $@
	@echo "*    $@" >> $@
	@echo "" >> $@
	@echo "*  Purpose:" >> $@
	@echo "*    This file expands the generic C code held in $(@:.c=.cgen) to provide" >> $@
	@echo "*    the required type-specific implementations which can be called by" >> $@
	@echo "*    other functions." >> $@
	@echo "" >> $@
	@echo "*  Notes:" >> $@
	@echo "*    - This file is generated automatically at build time (see" >> $@
	@echo "*    cupidsub/Makefile.am)" >> $@
	@echo "*/" >> $@
	@echo "" >> $@
	@echo "#include \"prm_par.h\"" >> $@
	@echo "#include \"cgeneric.h\"" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_DOUBLE_TYPE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@
	@echo "#define CGEN_CODE_TYPE CGEN_FLOAT_TYPE" >> $@
	@echo "#include \"cgeneric_defs.h\"" >> $@
	@echo "#include \"$(@:.c=.cgen)\"" >> $@
	@echo "#undef CGEN_CODE_TYPE" >> $@
	@echo "" >> $@

# Need to distribute the generic files
EXTRA_DIST = $(CGENERIC_ROUTINES)



