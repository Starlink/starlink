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

if {[info exists astrotcl_library] && "$astrotcl_library" != ""} {
    if ![regexp $astrotcl_library $auto_path] {
        lappend auto_path $astrotcl_library
    }
}
