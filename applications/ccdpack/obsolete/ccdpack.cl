# IRAF package initialisation script for the  ADAM ccdpack package
# Created automatically from ccdpack.ifd using ifd2iraf
# 20Jun97

package ccdpack

# Get the Starlink setup.
cl < "starlink$irafstar/zzsetenv.def"

task nagmakemos = "ccdpack$ccdpack_nag.e"
task makebias = "ccdpack$ccdpack_red.e"
task debias = "ccdpack$ccdpack_red.e"
task makecal = "ccdpack$ccdpack_red.e"
task calcor = "ccdpack$ccdpack_red.e"
task makeflat = "ccdpack$ccdpack_red.e"
task flatcor = "ccdpack$ccdpack_red.e"
task findcent = "ccdpack$ccdpack_reg.e"
task idicurs = "ccdpack$ccdpack_reg.e"
task findobj = "ccdpack$ccdpack_reg.e"
task register = "ccdpack$ccdpack_reg.e"
task tranlist = "ccdpack$ccdpack_reg.e"
task plotlist = "ccdpack$ccdpack_reg.e"
task findoff = "ccdpack$ccdpack_reg.e"
task pairndf = "ccdpack$ccdpack_reg.e"
task ccdedit = "ccdpack$ccdpack_reg.e"
task makemos = "ccdpack$ccdpack_reg.e"
task tranndf = "ccdpack$ccdpack_reg.e"
task ccdsetup = "ccdpack$ccdpack_res.e"
task ccdclear = "ccdpack$ccdpack_res.e"
task ccdnote = "ccdpack$ccdpack_res.e"
task ccdshow = "ccdpack$ccdpack_res.e"
task ccdndfac = "ccdpack$ccdpack_res.e"
task ccdgenerate = "ccdpack$ccdpack_res.e"
task picinfo = "ccdpack$ccdpack_res.e"
task import = "ccdpack$ccdpack_res.e"
task present = "ccdpack$ccdpack_res.e"
task schedule = "ccdpack$ccdpack_res.e"
task ccdimp = "ccdpack$ccdpack_res.e"
task ccdexp = "ccdpack$ccdpack_res.e"
task reduce = "ccdpack$ccdpack_scr.e"
task ccdalign = "ccdpack$ccdpack_scr.e"
task $xreduce = "ccdpack$ccdpack_scr.e"

#  IRAF specific tasks
task use_globals = "ccdpack$use_globals.cl"

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

#
#   Show that the CCDPACK commands are now available.
#
print ""
print "   CCDPACK commands are now available -- (Version PKG_VERS)"
print ""

#  For IRAF data we really do need to keep hold of the bad pixels
#  so make sure of this, unless the NDF_TO_IRAF_PARS variable
#  is already set. In this case we assume that the user knows what
#  they are doing.
s1 = "NONE"
show | match ("NDF_TO_IRAF_PARS=", stop=no ) | scan (s1)
if ( s1 == "NONE") { 
   set NDF_TO_IRAF_PARS="FILLBAD=!"
} else {
}

clbye()
