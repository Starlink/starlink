# E.S.O. - VLT project
# $Id: skyCatStart.tcl,v 1.1 1998/10/30 18:20:52 abrighto Exp $ 
#
# skyCatStart - startup script for tclpro wrapped version of skycat
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 29 Oct 98   created

set auto_path [list tclutil/library astrotcl/library rtdimg/library tclcat/library interp/library tclX8.0.3 blt2.4]

# start the application
skycat::SkyCat::startSkyCat

