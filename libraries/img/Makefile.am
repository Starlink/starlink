## Process this file with automake to produce Makefile.in

## List the scripts which are to be installed in the `bin' directory.
bin_SCRIPTS = img_link_adam

## List libraries which are to be built, and installed in `lib'.
lib_LTLIBRARIES = libimg_adam.la

## Declare the sources for the library.
libimg_adam_la_SOURCES = \
	$(PUBLIC_INCLUDES) \
	$(BLOCK_DATA_FILES) \
	$(C_ROUTINES) \
	$(F_ROUTINES)

## List headers which are to be installed -- these are built first if necessary
include_HEADERS = $(PUBLIC_INCLUDES)

PUBLIC_INCLUDES = IMG_ERR img_err.h img.h

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

##  Generic source code expanded names... (no longer needed?).
GF_ROUTINES = \
 img_in1xB.f img_in1xW.f img_in1xb.f img_in1xd.f img_in1xi.f img_in1xr.f \
 img_in1xw.f img_in2xB.f img_in2xW.f img_in2xb.f img_in2xd.f img_in2xi.f \
 img_in2xr.f img_in2xw.f img_in3xB.f img_in3xW.f img_in3xb.f img_in3xd.f \
 img_in3xi.f img_in3xr.f img_in3xw.f img_inxB.f img_inxW.f img_inxb.f \
 img_inxd.f img_inxi.f img_inxr.f img_inxw.f img_mod1xB.f img_mod1xW.f \
 img_mod1xb.f img_mod1xd.f img_mod1xi.f img_mod1xr.f img_mod1xw.f \
 img_mod2xB.f img_mod2xW.f img_mod2xb.f img_mod2xd.f img_mod2xi.f \
 img_mod2xr.f img_mod2xw.f img_mod3xB.f img_mod3xW.f img_mod3xb.f \
 img_mod3xd.f img_mod3xi.f img_mod3xr.f img_mod3xw.f img_modxB.f img_modxW.f \
 img_modxb.f img_modxd.f img_modxi.f img_modxr.f img_modxw.f img_new1xB.f \
 img_new1xW.f img_new1xb.f img_new1xd.f img_new1xi.f img_new1xr.f \
 img_new1xw.f img_new2xB.f img_new2xW.f img_new2xb.f img_new2xd.f \
 img_new2xi.f img_new2xr.f img_new2xw.f img_new3xB.f img_new3xW.f \
 img_new3xb.f img_new3xd.f img_new3xi.f img_new3xr.f img_new3xw.f img_newxB.f \
 img_newxW.f img_newxb.f img_newxd.f img_newxi.f img_newxr.f img_newxw.f \
 img_outxB.f img_outxW.f img_outxb.f img_outxd.f img_outxi.f img_outxr.f \
 img_outxw.f img_tmp1xB.f img_tmp1xW.f img_tmp1xb.f img_tmp1xd.f img_tmp1xi.f \
 img_tmp1xr.f img_tmp1xw.f img_tmp2xB.f img_tmp2xW.f img_tmp2xb.f \
 img_tmp2xd.f img_tmp2xi.f img_tmp2xr.f img_tmp2xw.f img_tmp3xB.f \
 img_tmp3xW.f img_tmp3xb.f img_tmp3xd.f img_tmp3xi.f img_tmp3xr.f \
 img_tmp3xw.f img_tmpxB.f img_tmpxW.f img_tmpxb.f img_tmpxd.f img_tmpxi.f \
 img_tmpxr.f img_tmpxw.f img1_gkeyxd.f img1_gkeyxr.f img1_gkeyxl.f \
 img1_gkeyxi.f hdr_inxc.f hdr_inxd.f hdr_inxi.f hdr_inxl.f hdr_inxr.f \
 hdr_outxc.f hdr_outxd.f hdr_outxi.f hdr_outxl.f hdr_outxr.f \
 img1_rdexxc.f img1_rdexxd.f img1_rdexxi.f img1_rdexxl.f \
 img1_rdexxr.f img1_rdftxc.f img1_rdftxd.f img1_rdftxi.f \
 img1_rdftxl.f img1_rdftxr.f img1_wkeyxc.f img1_wkeyxd.f \
 img1_wkeyxi.f img1_wkeyxl.f img1_wkeyxr.f img1_wrexxc.f \
 img1_wrexxd.f img1_wrexxi.f img1_wrexxl.f img1_wrexxr.f \
 img1_wrftxc.f img1_wrftxd.f img1_wrftxi.f img1_wrftxl.f \
 img1_wrftxr.f

##  Lists of the various generic Fortran source codes. Note that these are
##  distinguished by their file types and consequently require suffix rules to
##  make sure that they compile correctly. These names are only used when
##  creating a release from an archive.

G_ROUTINES = img_in1x.gen img_in2x.gen img_in3x.gen img_inx.gen \
             img_new1x.gen img_new2x.gen img_new3x.gen img_newx.gen \
             img_outx.gen img_tmp1x.gen img_tmp2x.gen img_tmp3x.gen \
             img_tmpx.gen img_mod1x.gen img_mod2x.gen img_mod3x.gen \
             img_modx.gen

GSD_ROUTINES = img1_gkeyx.gsd

GSC_ROUTINES = hdr_inx.gsc hdr_outx.gsc img1_rdexx.gsc img1_rdftx.gsc \
               img1_wkeyx.gsc img1_wrexx.gsc img1_wrftx.gsc

## Note that we cannot call this variable just BLOCK_DATA, since automake
## interprets this as a `primary' DATA
BLOCK_DATA_FILES = img1_init.f

## Example data files, not compiled.
EXAMPLES = add.f add.c add.ifl hdrlist.f hdrlist.c hdrlist.ifl \
           hdrread.f hdrread.c hdrread.ifl flat.f flat.c flat.ifl \
           mean.f mean.c mean.ifl shadow.f shadow.c shadow.ifl \
           hdrwrite.f hdrwrite.c hdrwrite.ifl proc.c proc.ifl


## We can add any other Makefile rules we require, and automake will
## copy them through to the output.
