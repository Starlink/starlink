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

package require img::xpm

if {![lcontain $auto_path $cat_library]} {
    lappend auto_path $cat_library
}

namespace eval cat {namespace export *}
namespace import -force cat::*
