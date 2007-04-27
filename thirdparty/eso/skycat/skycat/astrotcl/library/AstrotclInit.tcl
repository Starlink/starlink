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
# abrighto     21/12/05    updated for new version

#  We depend on.
package require Tclutil
package require Tclx

if {![lcontain $auto_path $astrotcl_library]} {
    lappend auto_path $astrotcl_library
}

namespace eval astrotcl {namespace export *}
namespace import -force astrotcl::*
