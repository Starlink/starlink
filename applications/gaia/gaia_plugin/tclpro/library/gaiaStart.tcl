# E.S.O. - VLT project
# $Id: gaiaStart.tcl,v 1.1 1999/03/17 20:49:16 abrighto Exp $ 
#
# skyCatStart - startup script for tclpro wrapped version of skycat
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 29 Oct 98   created

set auto_path [list tclutil/library astrotcl/library rtdimg/library tclcat/library interp/library gaia/library tclX8.0.3 blt2.4]

# start the application
gaia::Gaia::startGaia

