# -*- tcl -*- $Id$

package provide page::config::peg 0.1

proc page_cdefinition {} {
    return {
	--reset
	--append
	--reader    peg
	--transform reachable
	--transform realizable
	--writer    me
    }
}
