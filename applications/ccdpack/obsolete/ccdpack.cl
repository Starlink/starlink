# IRAF package initialisation script for the  ADAM ccdpack package
# Created automatically from ccdpack.ifd using ifd2iraf
# 20Jun97

package ccdpack

task nagmakemos = "ccdpackdir$ccdpack_nag.e"
task makebias = "ccdpackdir$ccdpack_red.e"
task debias = "ccdpackdir$ccdpack_red.e"
task makecal = "ccdpackdir$ccdpack_red.e"
task calcor = "ccdpackdir$ccdpack_red.e"
task makeflat = "ccdpackdir$ccdpack_red.e"
task flatcor = "ccdpackdir$ccdpack_red.e"
task findcent = "ccdpackdir$ccdpack_reg.e"
task idicurs = "ccdpackdir$ccdpack_reg.e"
task findobj = "ccdpackdir$ccdpack_reg.e"
task register = "ccdpackdir$ccdpack_reg.e"
task tranlist = "ccdpackdir$ccdpack_reg.e"
task plotlist = "ccdpackdir$ccdpack_reg.e"
task findoff = "ccdpackdir$ccdpack_reg.e"
task pairndf = "ccdpackdir$ccdpack_reg.e"
task ccdedit = "ccdpackdir$ccdpack_reg.e"
task makemos = "ccdpackdir$ccdpack_reg.e"
task tranndf = "ccdpackdir$ccdpack_reg.e"
task ccdsetup = "ccdpackdir$ccdpack_res.e"
task ccdclear = "ccdpackdir$ccdpack_res.e"
task ccdnote = "ccdpackdir$ccdpack_res.e"
task ccdshow = "ccdpackdir$ccdpack_res.e"
task ccdndfac = "ccdpackdir$ccdpack_res.e"
task ccdgenerate = "ccdpackdir$ccdpack_res.e"
task picinfo = "ccdpackdir$ccdpack_res.e"
task import = "ccdpackdir$ccdpack_res.e"
task present = "ccdpackdir$ccdpack_res.e"
task schedule = "ccdpackdir$ccdpack_res.e"
task ccdimp = "ccdpackdir$ccdpack_res.e"
task ccdexp = "ccdpackdir$ccdpack_res.e"
task reduce = "ccdpackdir$ccdpack_scr.e"
task ccdalign = "ccdpackdir$ccdpack_scr.e"
task $xreduce = "ccdpackdir$ccdpack_scr.e"

#  IRAF specific tasks
task use_globals = "ccdpackdir$use_globals.cl"

#  IRAF environment variables
set CCDPACK_GLOBALS="yes"

#   Setup conversion of header information for when using foreign data
#   access.
s1 = "NONE"
show | match ("NDF_XTN=", stop=no ) | scan (s1)
if ( s1 == "NONE") { 
   set NDF_XTN="CCDPACK"
} else {
   set NDF_XTN=(envget("NDF_XTN")//",CCDPACK")
}
set NDF_IMP_CCDPACK="$CCDPACK_DIR/ccdimp.csh ^ndf"
set NDF_EXP_CCDPACK="$CCDPACK_DIR/ccdexp.csh ^ndf"

clbye()
