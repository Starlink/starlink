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
# abrighto     21/12/05    updated for new version

# see expr(n) for a description of this global Tcl variable
set tcl_precision 17

package require Itcl
package require Itk
package require Tclx

if {![lcontain $auto_path $tclutil_library]} {
    lappend auto_path $tclutil_library
}

# set up the namespaces used by the itcl/itk classes
namespace import -force blt::*
namespace import -force itcl::*
namespace import -force itk::*
namespace eval util {namespace export *}
namespace import -force util::*

