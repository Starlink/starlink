## Process this file with automake to produce Makefile.in

## List the scripts which are to be installed in the `bin' directory.
bin_SCRIPTS = img_link_adam
dist_bin_SCRIPTS = img_link_adam

## List libraries which are to be built, and installed in `lib'.
lib_LTLIBRARIES = libimg_adam.la

## Declare the sources for the library.
libimg_adam_la_SOURCES = \
 $(include_HEADERS) \
 $(include_MESSAGES) \
 $(PRIVATE_INCLUDES) \
 $(BLOCK_DATA_FILES) \
 $(C_ROUTINES) \
 $(F_ROUTINES) \
 $(G_ROUTINES:.gen=.f) \
 $(GNP_ROUTINES:.gnp=.f) \
 $(GCP_ROUTINES:.gcp=.f)

##  Distribute the original sources, in case these are needed for further
##  development. 
EXTRA_DIST = $(G_ROUTINES) $(GNP_ROUTINES) $(GCP_ROUTINES)

## List headers which are to be installed -- these are built first if necessary
include_HEADERS = img.h
include_MESSAGES = IMG_ERR img_err.h

stardocs_DATA = @STAR_LATEX_DOCUMENTATION@

PRIVATE_INCLUDES = IMG_CONST IMG_PCB IMG_ECB img1.h \
 imgIn1Gen.h imgIn2Gen.h imgIn3Gen.h imgInGen.h imgMod1Gen.h \
 imgMod2Gen.h imgMod3Gen.h imgModGen.h imgNew1Gen.h imgNew2Gen.h \
 imgNew3Gen.h imgNewGen.h imgOutGen.h imgTmp1Gen.h imgTmp2Gen.h \
 imgTmp3Gen.h imgTmpGen.h

F_ROUTINES = hdr_copy.f hdr_delet.f hdr_in.f hdr_mod.f hdr_name.f \
 hdr_numb.f hdr_out.f \
 img1_assoc.f img1_callo.f img1_cfree.f img1_ckey.f \
 img1_cnslt.f img1_cpy.f img1_creal.f img1_dcel.f img1_dkey.f \
 img1_dlex.f img1_dlft.f img1_exex.f img1_exft.f img1_exini.f \
 img1_fobj.f img1_frext.f img1_frslt.f img1_ftscp.f img1_gkeyc.f \
 img1_gtndf.f img1_gtslt.f img1_ncel.f img1_nex.f img1_nft.f \
 img1_nkey.f img1_nmex.f img1_nmft.f img1_nwndf.f img1_ok.f img1_ploc.f \
 img1_prndf.f img1_pshdb.f img1_pshde.f img1_pshdf.f img1_rkey.f \
 img1_tpndf.f img1_trace.f img1_vpar.f img1_wcel.f img_cancl.f \
 img_check.f img_delet.f img_free.f img_in.f img_in1.f img_in2.f \
 img_in3.f img_indf.f img_mod.f img_mod1.f img_mod2.f img_mod3.f \
 img_new.f img_new1.f img_new2.f img_new3.f img_out.f img_tmp.f \
 img_tmp1.f img_tmp2.f img_tmp3.f img_name.f img1_repft.f \
 img1_frtra.f img1_repex.f

C_ROUTINES = StringArray.c hdrCopy.c hdrDelet.c hdrIn.c hdrInC.c hdrInD.c \
 hdrInI.c hdrInL.c hdrInF.c hdrMod.c hdrName.c hdrNumb.c hdrOut.c \
 hdrOutC.c hdrOutD.c hdrOutI.c hdrOutL.c hdrOutF.c imgCancl.c \
 imgCheck.c imgDelet.c imgFree.c imgIn.c imgIn1.c imgIn2.c imgIn3.c \
 imgIndf.c imgMod.c imgMod1.c imgMod2.c imgMod3.c imgNew.c imgNew1.c \
 imgNew2.c imgNew3.c imgOut.c imgTmp.c imgTmp1.c imgTmp2.c imgTmp3.c \
 imgName.c

##  Lists of the various generic Fortran source codes. Note that these are
##  distinguished by their file types and consequently require suffix rules to
##  make sure that they compile correctly. These files are only required when
##  working on the CVS checkout or when creating a distribution, but are
##  distributed.

G_ROUTINES = img_in1x.gen img_in2x.gen img_in3x.gen img_inx.gen \
 img_new1x.gen img_new2x.gen img_new3x.gen img_newx.gen \
 img_outx.gen img_tmp1x.gen img_tmp2x.gen img_tmp3x.gen \
 img_tmpx.gen img_mod1x.gen img_mod2x.gen img_mod3x.gen img_modx.gen

img_in1x.o: img_in1x.f 
img_in2x.o: img_in2x.f 
img_in3x.o: img_in3x.f 
img_inx.o: img_inx.f
img_new1x.o: img_new1x.f
img_new2x.o:img_new2x.f
img_new3x.o:img_new3x.f
img_newx.o: img_newx.f
img_outx.o: img_outx.f
img_tmp1x.o: img_tmp1x.f
img_tmp2x.o: img_tmp2x.f
img_tmp3x.o: img_tmp3x.f
img_tmpx.o: img_tmpx.f
img_mod1x.o: img_mod1x.f 
img_mod2x.o: img_mod2x.f
img_mod3x.o: img_mod3x.f
img_modx.o: img_modx.f

GNP_ROUTINES = img1_gkeyx.gnp

img1_gkeyx.o: img1_gkeyx.f

GCP_ROUTINES = hdr_inx.gcp hdr_outx.gcp img1_rdexx.gcp img1_rdftx.gcp \
 img1_wkeyx.gcp img1_wrexx.gcp img1_wrftx.gcp

hdr_inx.o: hdr_inx.f
hdr_outx.o: hdr_outx.f
img1_rdexx.o: img1_rdexx.f
img1_rdftx.o: img1_rdftx.f
img1_wkeyx.o: img1_wkeyx.f
img1_wrexx.o: img1_wrexx.f
img1_wrftx.o: img1_wrftx.f

## Note that we cannot call this variable just BLOCK_DATA, since automake
## interprets this as a `primary' DATA
BLOCK_DATA_FILES = img1_init.f

## Example data files, not compiled.
EXAMPLES = add.f add.c add.ifl hdrlist.f hdrlist.c hdrlist.ifl hdrread.f \
 hdrread.c hdrread.ifl flat.f flat.c flat.ifl mean.f mean.c mean.ifl \
 shadow.f shadow.c shadow.ifl hdrwrite.f hdrwrite.c hdrwrite.ifl proc.c \
 proc.ifl

## but will be distributed.
EXTRA_DIST += $(EXAMPLES)

## We can add any other Makefile rules we require, and automake will
## copy them through to the output.

##  Generic
FGENERIC=@FGENERIC@

#  Run generic to get numeric types.
GFLAGS = n
.gen.f:
	$(FGENERIC) -t $(GFLAGS) $<

#  Run generic to get the complete set of parameter types (rlidc)
GCPFLAGS = rlidc
.gcp.f:
	$(LN_S) $< $(<:.gcp=.gen)
	$(FGENERIC) -t $(GCPFLAGS) $(<:.gcp=.gen)
	$(RM) $(<:.gcp=.gen)

#  Run generic to get numeric parameter rlid types.
GNPFLAGS = rlid
.gnp.f:
	$(LN_S) $< $(<:.gnp=.gen)
	$(FGENERIC) -t $(GNPFLAGS) $(<:.gnp=.gen)
	$(RM) $(<:.gnp=.gen)

##  The Fortran intermediary files should be cleaned away sometime.
##  Since they are distributed make this maintainer-clean.
MAINTAINERCLEANFILES = $(G_ROUTINES:.gen=.f) $(GNP_ROUTINES:.gnp=.f) \
 $(GCP_ROUTINES:.gcp=.f)

#  Installation check.
TESTS = img_test
check_MONOLITHS = img_test
img_test_SOURCES = img_test.f libimg_adam.la
# no variable img_test_TASKS -- img_test is an ATASK
img_test_LDADD = $(LDADD) `$(srcdir)/img_link_adam`

dist_starnews_DATA = img.news

CLEANFILES = img_test img_test.ifc

