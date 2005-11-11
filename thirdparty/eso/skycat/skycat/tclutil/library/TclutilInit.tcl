# E.S.O. - VLT project
#
# "@(#) $Id:
#
# TclutilInit.tcl
#
# script which is executed by TclutilImage.C to initialize tcl
#
# who           when       what
# --------     ---------   ----------------------------------------------
# pbiereic     24/08/99    created

if {[info exists tclutil_library] && "$tclutil_library" != ""} {
    if ![regexp $tclutil_library $auto_path] {
        lappend auto_path $tclutil_library
    }
}

# see expr(n) for a description of this global Tcl variable
set tcl_precision 17

# set up the namespaces used by the itcl/itk classes
if { $tcl_version >= 8 } {
    namespace import -force blt::*
    namespace import -force itcl::*
    namespace import -force itk::*
    namespace eval util {namespace export *}
    namespace import -force util::*
} else {
    import add blt
    import add itcl
    import add itk
    namespace ::util {}
    import add util
}
