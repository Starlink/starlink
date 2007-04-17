# E.S.O. - VLT project
#
# "@(#) $Id: RtdInit.tcl,v 1.2 2006/01/25 10:55:38 abrighto Exp $"
#
# RtdInit.tcl
#
# script which is executed by RtdImage.C to initialize tcl
#
# who           when       what
# --------     ---------   ----------------------------------------------
# pbiereic     24/08/99    created

if {![lcontain $auto_path $rtd_library]} {
    lappend auto_path $rtd_library
}

namespace eval rtd {namespace export *}
namespace import -force rtd::*
