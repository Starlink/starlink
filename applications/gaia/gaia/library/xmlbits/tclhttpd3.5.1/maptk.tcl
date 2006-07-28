# maptk.tcl
# simple sample image map resolver
# uses Netscape map files
#
# This version uses a Tk canvas to do hit detection.
#
# Stephen Uhler (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::ismaptk 1.0

# 	read in an image map file, and generate the appropriate canvas
# 	# sample image map
#	circle 	/sun-on-net/internet.sol/access.html  664,311 686,311
#	default	http://www.hal.com/
#	default 	/sunsoft/index.html
#	point /sunworldonline/common/swol-subscribe.html 45,134
#	poly / 182,11 184,88 301,77 365,86 393,57 393,10 
#	rect	/sunsoft/index.html 1,1 119,34

# translate an x/y coordinate into a url

proc Map_Lookup {request} {
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

# Resolve hits by building a canvas (which is never mapped),
# with the map objects.  Use the file name as the canvas tag.

proc MapRead {file} {
    if {[catch {open $file} fd]} {
	Log "" "file open" $fd
	return {}
    }
    regsub -all  {\.}  $file _ cookie
    set data [read $fd]
    close $fd
    catch {destroy .image$cookie}
    set can [canvas .image$cookie]
    regsub -all "(^|\n+)#\[^\n]*" $data {} data
    regsub -all {([][$\\])} $data {\\\1} data
    set types circle|default|point|poly|rect
    append exp {[ 	]*(} $types {)[ 	]+([^	}
    append exp \n { ]+)[ 	]*([^} \n\r {]*)} \[\n\r]+
    regsub -nocase -all $exp $data "\[MapInsert [list $can] \\1 {\\2} {\\3}]" cmd
    $can lower default
    set tags [$can itemconfigure default -tags]
    $can itemconfigure default -tags [lindex $tags 0]
    subst $cmd
    return $can
}

# helper proc for generating the canvas

proc MapInsert {can type href coords} {
    regsub -all , $coords { } coords
    if [catch {
    switch -- $type {
	default {
	    upvar #0 $can default
	    set default $href
#	    $can create rectangle  0 0 1000 1000 -fill white -tags [list $href default]
	}
	circle {
	    # The coords are the center and one point on the edge.
	    # Tk ovals are defined by two corner points.
	    foreach {x1 y1 x2 y2} $coords {break}
	    set r [expr int(sqrt(($y2 - $y1) * ($y2 - $y1) + \
				    ($x2 - $x1) * ($x2 - $x1)))]
	    set x1 [expr $x1 - $r]
	    set x2 [expr $x1 + 2*$r]
	    set y1 [expr $y1 - $r]
	    set y2 [expr $y1 + 2 * $r]
	    $can create oval $x1 $y1 $x2 $y2 -fill black -tags $href
	}
	rect {
	    eval {$can create rectangle} $coords -fill black -tags {$href}
	}
	poly {
	    eval {$can create polygon} $coords -fill black -tags {$href}
	}
	point {
	    eval {$can create oval} $coords $coords -width 2 -fill black \
		    -tags {$href}
	}
    }} err] {
	Log $can $href $err
    }
}

proc MapHit {map x y} {
    if {[catch {
	$map gettags [lindex [$map find overlapping $x $y $x $y] 0]
    } result]} {
	set result {}
    }
    if {[string length $result] == 0} {
	upvar #0 $map default
	catch {set result $default}
    }
    return $result
}
