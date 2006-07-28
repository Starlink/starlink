# maptcl.tcl
# Pure Tcl imagemap handling.
#
# 	read in an image map file, and generate the appropriate canvas
# 	# sample image map
#	circle 	/sun-on-net/internet.sol/access.html  664,311 686,311
#	default	http://www.hal.com/
#	default 	/sunsoft/index.html
#	point /sunworldonline/common/swol-subscribe.html 45,134
#	poly / 182,11 184,88 301,77 365,86 393,57 393,10 
#	rect	/sunsoft/index.html 1,1 119,34
#
# Brent Welch (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::ismaptcl 1.0
package require httpd::imagemap 1.0

# translate an x/y coordinate into a url

proc Map_Lookup {request} {
    global ImageMaps
    if {[regexp {([^?]+)\?([0-9]+),([0-9]+)} $request {} map x y]} {
	if [catch {file mtime $map} mtime] {
	    return ""
	}
	if {![info exists ImageMaps($map)] ||
		($mtime > $ImageMaps(mtime,$map))} {
	    set ImageMaps($map) [MapRead $map]
	    set ImageMaps(mtime,$map) $mtime
	} 
	return [MapHit $ImageMaps($map) $x $y]
    } else {
	return ""
    }
}

# MapRead - this parses a map data file and converts it to
# a Tcl array that contains state about each region.
# This parser is another famous regsub-subst combo by Steve Uhler

proc MapRead {file} {
    if {[catch {open $file} fd]} {
	Log "" "file open" $fd
	return {}
    }
    regsub -all  {\.}  $file _ cookie
    set data [read $fd]
    close $fd
    regsub -all "(^|\n+)#\[^\n]*" $data {} data
    regsub -all {([][$\\])} $data {\\\1} data
    set types circle|default|point|poly|rect
    append exp {[ 	]*(} $types {)[ 	]+([^	}
    append exp \n { ]+)[ 	]*([^} \n\r {]*)} \[\n\r]+
    regsub -nocase -all $exp $data "\[MapInsert [list $cookie] \\1 {\\2} {\\3}]" cmd
    upvar #0 $cookie map
    catch {unset map}
    subst $cmd
    return $cookie
}

# helper proc for generating the data structure

proc MapInsert {cookie type href coords} {
    upvar #0 $cookie map
    if ![info exists map] {
	set map(N) 0
    } else {
	incr map(N)
    }
    regsub -all , $coords { } coords
    set c {}
    set i 0
    foreach {X Y} $coords {
	lappend c $i,X $X $i,Y $Y
	incr i
    }
    set map($map(N),type) $type
    set map($map(N),coords) $c
    set map($map(N),href) $href
    if {$type == "default"} {
	set map(default) $href
    }
}

# MapHit looks up coordinates in a map

proc MapHit {cookie x y} {
    upvar #0 $cookie map
    set sawpoint 0
    for {set i 0} {$i < $map(N)} {incr i} {
	array set pgon $map($i,coords)
	switch $map($i,type) {
	    poly {
		if [MapPointInPoly $x $y pgon] {
		    return $map($i,href)
		}
	    }
	    circle {
		if [MapPointInCircle $x $y pgon] {
		    return $map($i,href)
		}
	    }
	    rect {
		if [MapPointInRect $x $y pgon] {
		    return $map($i,href)
		}
	    }
	    point {
		set dist [expr ($x - $pgon(0,X)) * ($x - $pgon(0,X)) + \
				($y - $pgon(0,Y)) * ($y - $pgon(0,Y))]
		if {!$sawpoint || ($dist < $mindist)} {
		    set mindist $dist
		    set default $map($i,href)
		}
	    }
	}
    }
    if [info exists default] {
	return $default
    }
    if [info exists map(default)] {
	return $map(default)
    }
    return {}
}

