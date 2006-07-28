# imagemap.tcl
# based on imagemap.c, version 1.8
# Brent Welch (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::imagemap 1.0

# Doc_application/x-imagemap --
#
# this is called by DocHandle to process .map files
#
# Arguments:
#	path	The file name of the .map file.
#	suffix	The URL suffix
#	sock	The socket connection.
#
# Results:
#	None
#
# Side Effects:
#	Redirect to the URL indicated by the map.

proc Doc_application/x-imagemap {path suffix sock} {
    upvar #0 Httpd$sock data
    if {![info exists data(query)]} {
	Httpd_ReturnData $sock text/plain "[parray Httpd$sock]"
	return
    }
    set url [Map_Lookup $path?$data(query)]
    Count maphits
    Httpd_Redirect $url $sock
}

proc MapPointInRect {X Y coordArray} {
	upvar $coordArray coords
        return [expr ($X >= $coords(0,X) && $X <= $coords(1,X)) && \
        ($Y >= $coords(0,Y) && $Y <= $coords(1,Y))]
}

proc MapPointInCircle {X Y coordArray} {
	upvar $coordArray coords

        set radius1 [expr (($coords(0,Y) - $coords(1,Y)) * ($coords(0,Y) - \
        $coords(1,Y))) + (($coords(0,X) - $coords(1,X)) * ($coords(0,X) - \
        $coords(1,X)))]
        set radius2 [expr (($coords(0,Y) - $Y) * ($coords(0,Y) - $Y)) + \
        (($coords(0,X) - $X) * ($coords(0,X) - $X))]
        return [expr $radius2 <= $radius1]
}

proc MapPointInPoly {tx ty pgonArray} {
	upvar $pgonArray pgon
	global MAXVERTS

	set numverts [expr [llength [array names pgon]] / 2]
	set lastvert [expr $numverts - 1]
        set crossings 0

        set y pgon($lastvert,Y)

        set p pgon(0,Y)
        if {($y >= $ty) != ($p >= $ty)} {
		set xflag0 [expr $pgon($lastvert,X) >= $tx]
                if {$xflag0 == ($pgon(0,X) >= $tx)} {
                        if $xflag0 {
                                incr crossings
			}
                } else {
                        incr crossings [expr {$pgon($lastvert,X) - ($y - $ty) *
			    ($pgon(0,X) - $pgon($lastvert,X)) /
			    ($p - $y) >= $tx}]
                }
        }

        for {set i 0} {$i < $numverts} {incr i} {
		set y $pgon($i,Y)
                if {$y >= $ty} {
			while {$i < $numverts && $pgon($i,Y) >= $ty} {
				incr i
			}
                } else {
			while {$i < $numverts && $pgon($i,Y) < $ty} {
				incr i
			}
		}
		if {$i >= $numverts} {
			break
		}
		set lasti [expr $i-1]
		set xflag0 [expr $pgon($lasti,X) >= $tx]
		if {$xflag0 == ($pgon($i,X) >= $tx)} {
			if $xflag0 {
				incr crossings
			}
		} else {
			incr crossings \
			    [expr ($pgon($lasti,X) - \
			    ($pgon($lasti,Y) - $ty) * \
			    ($pgon($i,X) - $pgon($lasti,X)) / \
			    ($pgon($i,Y) - $pgon($lasti,Y))) >= $tx]
		}
        }
        set inside_flag [expr $crossings & 0x01]
        return $inside_flag
}

proc MapTest {} {
    array set pgon {
	0,X 0.0   0,Y 0.0
	1,X 100.0 1,Y 0.0
	2,X 100.0 2,Y 100.0
	3,X 0.0   3,Y 100.0
    }
    foreach {X Y} {50. 50. 1. 1. 0. 0. 200. 200.} {
	puts "($X,$Y) In Poly [pointinpoly $X $Y pgon]"
    }
}
