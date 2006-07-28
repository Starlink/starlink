# TclDOM package index - hand crafted
#
# $Id$

package ifneeded dom::c          3.0 [list load   [file join $dir UNSPECIFIED]]
package ifneeded dom::tcl        3.0 [list source [file join $dir dom.tcl]]
package ifneeded dommap          1.0       [list source [file join $dir dommap.tcl]]
package ifneeded xmlswitch       1.0       [list source [file join $dir xmlswitch.tcl]]

# Examples - will not necessarily be installed
package ifneeded cgi2dom         1.1       [list source [file join $dir cgi2dom.tcl]]
package ifneeded domtree         3.0 [list source [file join $dir domtree.tcl]]
package ifneeded domtree::treectrl 3.0 [list source [file join $dir domtree-treectrl.tcl]]
package ifneeded domtext         3.0 [list source [file join $dir domtext.tcl]]

## Provided by separate package.
##package ifneeded dom::libxml2    3.0 [list load [file join $dir .. UNSPECIFIED] Tcldomxml]

namespace eval ::dom {}

# Requesting the generic dom package loads the C package 
# if available, otherwise falls back to the generic Tcl package.
# The application can tell which it got by examining the
# list of packages loaded (and looking for dom::c, dom::libxml2 or dom::tcl).

package ifneeded dom 3.0 {
    if {[catch {package require dom::libxml2 3.0}]} {
	if {[catch {package require dom::c 3.0}]} {
	    package require dom::tcl 3.0
	}
    }
    package provide dom 3.0
}
