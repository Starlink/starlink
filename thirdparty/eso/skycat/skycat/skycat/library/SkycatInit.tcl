# E.S.O. - VLT project
#
# "@(#) $Id: SkycatInit.tcl,v 1.3 2006/01/25 10:55:38 abrighto Exp $"
#
# SkycatInit.tcl
#
# script which is executed by Skycat.C to initialize the package
#
# who           when       what
# --------     ---------   ----------------------------------------------
# abrighto     02/01/06    created

package require img::xpm

if {![lcontain $auto_path $skycat_library]} {
    lappend auto_path $skycat_library
}

namespace eval skycat {namespace export *}
namespace import -force skycat::*
