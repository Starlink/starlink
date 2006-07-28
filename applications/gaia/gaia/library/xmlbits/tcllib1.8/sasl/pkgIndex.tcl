# pkgIndex.tcl                                                -*- tcl -*-
# Copyright (C) 2005 Pat Thoyts <patthoyts@users.sourceforge.net>
# $Id$
if {![package vsatisfies [package provide Tcl] 8.2]} {
    # PRAGMA: returnok
    return
}
package ifneeded SASL 1.0.0 [list source [file join $dir sasl.tcl]]
package ifneeded SASL::NTLM 1.0.0 [list source [file join $dir ntlm.tcl]]