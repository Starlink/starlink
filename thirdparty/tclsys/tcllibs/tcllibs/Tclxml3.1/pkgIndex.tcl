# Tcl package index file - handcrafted
#
# $Id$

package ifneeded xml::c       3.1 [list load   [file join $dir libTclxml3.1.so]]
package ifneeded xml::tcl     3.1 [list source [file join $dir xml__tcl.tcl]]
package ifneeded sgmlparser   1.0       [list source [file join $dir sgmlparser.tcl]]
package ifneeded xpath        1.0       [list source [file join $dir xpath.tcl]]
package ifneeded xmldep       1.0       [list source [file join $dir xmldep.tcl]]

# The C parsers are provided through their own packages and indices,
# and thus do not have to be listed here. This index may require them
# in certain places, but does not provide them. This is part of the
# work refactoring the build system of TclXML to create clean
# packages, and not require a jumble (jungle?) of things in one Makefile.
#
#package ifneeded xml::expat  3.1 [list load   [file join $dir @expat_TCL_LIB_FILE@]]
#package ifneeded xml::xerces 2.0       [list load   [file join $dir @xerces_TCL_LIB_FILE@]]
#package ifneeded xml::libxml2 3.1 [list load   [file join $dir @TclXML_libxml2_LIB_FILE@]]

namespace eval ::xml {}

# Requesting a specific package means we want it to be the default parser class.
# This is achieved by loading it last.

# expat and libxml2 packages must have xml::c package loaded
package ifneeded expat 3.1 {
    package require xml::c 3.1
    package require xmldefs
    package require xml::tclparser 3.1
    catch {package require xml::libxml2 3.1}
    package require xml::expat     3.1
    package provide expat          3.1
}
package ifneeded libxml2 3.1 {
    package require xml::c 3.1
    package require xmldefs
    package require xml::tclparser 3.1
    catch {package require xml::expat 3.1}
    package require xml::libxml2   3.1
    package provide libxml2        3.1
}

# tclparser works with either xml::c or xml::tcl
package ifneeded tclparser 3.1 {
    if {[catch {package require xml::c 3.1}]} {
	# No point in trying to load expat or libxml2
	package require xml::tcl       3.1
	package require xmldefs
	package require xml::tclparser 3.1
    } else {
	package require xmldefs
	catch {package require xml::expat   3.1}
	catch {package require xml::libxml2 3.1}
	package require xml::tclparser
    }
    package provide tclparser 3.1
}

# use tcl only (mainly for testing)
package ifneeded puretclparser 3.1 {
    package require xml::tcl       3.1
    package require xmldefs
    package require xml::tclparser 3.1
    package provide puretclparser  3.1
}                                        

# Requesting the generic package leaves the choice of default parser automatic

package ifneeded xml 3.1 {
    if {[catch {package require xml::c 3.1}]} {
	package require xml::tcl       3.1
	package require xmldefs
	# Only choice is tclparser
	package require xml::tclparser 3.1
    } else {
	package require xmldefs
	package require xml::tclparser    3.1
	# libxml2 is favoured since it provides more features
	catch {package require xml::expat 3.1}
	catch {package require xml::libxml2 3.1}
    }
    package provide xml 3.1
}

if {[info tclversion] <= 8.0} {
    package ifneeded sgml           1.9       [list source [file join $dir sgml-8.0.tcl]]
    package ifneeded xmldefs        3.1 [list source [file join $dir xml-8.0.tcl]]
    package ifneeded xml::tclparser 3.1 [list source [file join $dir tclparser-8.0.tcl]]
} else {
    package ifneeded sgml           1.9       [list source [file join $dir sgml-8.1.tcl]]
    package ifneeded xmldefs        3.1 [list source [file join $dir xml-8.1.tcl]]
    package ifneeded xml::tclparser 3.1 [list source [file join $dir tclparser-8.1.tcl]]
}


