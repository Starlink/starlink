# E.S.O. - VLT project
#
# "@(#) $Id:
#
# CatInit.tcl
#
# script which is executed by TclAstroCat.C to initialize tcl
#
# who           when       what
# --------     ---------   ----------------------------------------------
# pbiereic     24/08/99    created

if {[info exists cat_library] && ![cequal $cat_library {}]} {
    if ![lcontain $auto_path $cat_library] {
        lappend auto_path $cat_library
    }
}

# see expr(n) for a description of this global Tcl variable
set tcl_precision 17

# set up the namespaces used by the itcl/itk classes
if { $tcl_version >= 8 } {
    namespace eval cat {namespace export *}
    namespace import -force cat::*
} else {
    namespace ::cat {}
    import add cat
}
