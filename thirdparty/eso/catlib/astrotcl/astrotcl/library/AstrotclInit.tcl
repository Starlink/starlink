# E.S.O. - VLT project
#
# "@(#) $Id:
#
# AstrotclInit.tcl
#
# script which is executed by AstrotclImage.C to initialize tcl
#
# who           when       what
# --------     ---------   ----------------------------------------------
# pbiereic     24/08/99    created

if {[info exists astrotcl_library] && ![cequal $astrotcl_library {}]} {
    if ![lcontain $auto_path $astrotcl_library] {
        lappend auto_path $astrotcl_library
    }
}
