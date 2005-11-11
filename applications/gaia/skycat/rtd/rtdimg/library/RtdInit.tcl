# E.S.O. - VLT project
#
# "@(#) $Id: RtdInit.tcl,v 1.2 2005/02/02 01:43:03 brighton Exp $"
#
# RtdInit.tcl
#
# script which is executed by RtdImage.C to initialize tcl
#
# who           when       what
# --------     ---------   ----------------------------------------------
# pbiereic     24/08/99    created

if {[info exists rtd_library] && "$rtd_library" != ""} {
    if { ![regexp $rtd_library $auto_path] } {
        lappend auto_path $rtd_library
    }
}

# set library path for the packages tclutil and astrotcl
# which Rtd depends on

if { ! [info exists env(TCLUTIL_LIBRARY)] } {
    set env(TCLUTIL_LIBRARY) "[ file dirname $rtd_library ]/tclutil"
}
if { ! [info exists env(ASTOTCL_LIBRARY)] } {
    set env(ASTOTCL_LIBRARY) "[ file dirname $rtd_library ]/astrotcl"
}

# see expr(n) for a description of this global Tcl variable
set tcl_precision 17

# set up the namespaces used by the itcl/itk classes
if { $tcl_version >= 8 } {
    namespace eval rtd {namespace export *}
    namespace import -force rtd::*
} else {
    namespace ::rtd {}
    import add rtd
}
