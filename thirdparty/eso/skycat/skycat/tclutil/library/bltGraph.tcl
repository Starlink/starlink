# bltGraph.tcl - utility procs for BLT graphs
# allan: taken from the BLT library dir: 
# Merged here to work with 2.1 and 2.4 versions. 

proc Blt_ActiveLegend { graph } {
    $graph legend bind all <Enter> [list blt::ActivateLegend $graph ]
    $graph legend bind all <Leave> [list blt::DeactivateLegend $graph]
    $graph legend bind all <ButtonPress-1> [list blt::HighlightLegend $graph]
}

proc Blt_Crosshairs { graph } {
    blt::Crosshairs $graph 
}

proc Blt_ZoomStack { graph } {
    blt::ZoomStack $graph
}

proc Blt_PrintKey { graph } {
    blt::PrintKey $graph
}

proc Blt_ClosestPoint { graph } {
    blt::ClosestPoint $graph
}

#
# The following procedures that reside in the "blt" namespace are
# supposed to be private.
#

proc blt::ActivateLegend { graph } {
    set elem [$graph legend get current]
    $graph legend activate $elem
}
proc blt::DeactivateLegend { graph } {
    set elem [$graph legend get current]
    $graph legend deactivate $elem
}

proc blt::HighlightLegend { graph } {
    set elem [$graph legend get current]
    set relief [$graph element cget $elem -labelrelief]
    if { $relief == "flat" } {
	$graph element configure $elem -labelrelief raised
	$graph element activate $elem
    } else {
	$graph element configure $elem -labelrelief flat
	$graph element deactivate $elem
    }
}

proc blt::Crosshairs { graph } {
    $graph crosshairs on
    bind bltCrosshairs <Any-Motion>   {
	%W crosshairs configure -position @%x,%y 
    }
    $graph crosshairs configure -color red
    blt::AddBindTag $graph bltCrosshairs  
}

proc blt::ZoomStack { graph } {
    global zoomInfo
    
    set zoomInfo($graph,A,x) {}
    set zoomInfo($graph,A,y) {}
    set zoomInfo($graph,B,x) {}
    set zoomInfo($graph,B,y) {}
    set zoomInfo($graph,stack) {}
    set zoomInfo($graph,corner) A
    
    bind $graph <1> { 
	blt::SetZoomPoint %W %x %y 
    }
    bind $graph <ButtonPress-3> {
	if { [%W inside %x %y] } {
	    blt::ResetZoom %W 
	}
    }
}

proc blt::PrintKey { graph } {
    bind bltPrintGraph <Shift-ButtonRelease-3>  { Blt_PostScriptDialog %W }
    blt::AddBindTag $graph bltPrintGraph 
}

proc blt::ClosestPoint { graph } {
    bind bltClosestPoint <Control-ButtonPress-1>  {
	blt::FindElement %W %x %y
	break
    }
    blt::AddBindTag $graph bltClosestPoint 
}

proc blt::AddBindTag { graph name } {
    set oldtags [bindtags $graph]
    if { [lsearch $oldtags $name] < 0 } {
	bindtags $graph [concat $name $oldtags]
    }
}

proc blt::FindElement { graph x y } {
    if ![$graph element closest $x $y info -interpolate yes] {
	beep
	return
    }
    # --------------------------------------------------------------
    # find(name)		- element Id
    # find(index)		- index of closest point
    # find(x) find(y)		- coordinates of closest point
    #				  or closest point on line segment.
    # find(dist)		- distance from sample coordinate
    # --------------------------------------------------------------
    foreach i [array names info] {
        puts stderr "info($i) = $info($i)"
    }
    set markerName "bltClosest_$info(name)"
    catch { $graph marker delete $markerName }
    $graph marker create text -coords { $info(x) $info(y) } \
	-name $markerName \
	-text "$info(name): $info(dist)\nindex $info(index)" \
	-font *lucida*-r-*-10-* \
	-anchor center -justify left \
	-yoffset 0 -bg {} 

    set coords [$graph invtransform $x $y]
    set nx [lindex $coords 0]
    set ny [lindex $coords 1]

    $graph marker create line -coords "$nx $ny $info(x) $info(y)" \
	-name line.$markerName 

    blt::FlashPoint $graph $info(name) $info(index) 10
    blt::FlashPoint $graph $info(name) [expr $info(index) + 1] 10
}

proc blt::FlashPoint { graph name index count } {
    if { $count & 1 } {
        $graph element deactivate $name 
    } else {
        $graph element activate $name $index
    }
    incr count -1
    if { $count > 0 } {
	after 200 blt::FlashPoint $graph $name $index $count
	update
    } else {
	eval $graph marker delete [$graph marker names "bltClosest_*"]
    }
}

proc blt::GetCoords { graph x y index } {

    #
    # We're using the default axes, instead of transforming through
    # the specific axes, because it handles inverted axes automatically
    #
    #puts stderr "$x,$y ==>" nonewline

    set coords [$graph invtransform $x $y]
    set nx [lindex $coords 0]
    set ny [lindex $coords 1]

    set x $nx
    set y $ny

    scan [$graph xaxis limits] "%s %s" xmin xmax
    scan [$graph yaxis limits] "%s %s" ymin ymax

     set padx [expr ($xmax - $xmin) * 0.00]
     set pady [expr ($ymax - $ymin) * 0.00]
     if { $x > $xmax } { 
 	set x [expr $xmax + $padx]
     } elseif { $x < $xmin } { 
 	set x [expr $xmin - $padx]
     }
     if { $y > $ymax } { 
 	set y [expr $ymax + $pady]
     } elseif { $y < $ymin } { 
 	set y [expr $ymin - $pady]
     }

    global zoomInfo
    set zoomInfo($graph,$index,x) $x
    set zoomInfo($graph,$index,y) $y
}

proc blt::MarkPoint { graph index } {
    global zoomInfo
    set x $zoomInfo($graph,$index,x)
    set y $zoomInfo($graph,$index,y)

    set marker "zoomText_$index"
    set text [format "x=%.4g\ny=%.4g" $x $y] 

    if [$graph marker exists $marker] {
     	$graph marker configure $marker -coords { $x $y } -text $text 
    } else {
    	$graph marker create text -coords { $x $y } -name $marker \
   	    -font *lucida*-r-*-10-* \
	    -text $text -anchor center -bg {} -justify left
    }
}

proc blt::DestroyZoomTitle { graph } {
    global zoomInfo

    if { $zoomInfo($graph,corner) == "A" } {
	catch { $graph marker delete "zoomTitle" }
    }
}

proc blt::PopZoom { graph } {
    global zoomInfo

    set zoomStack $zoomInfo($graph,stack)
    if { [llength $zoomStack] > 0 } {
	set cmd [lindex $zoomStack 0]
	set zoomInfo($graph,stack) [lrange $zoomStack 1 end]
	eval $cmd
	blt::ZoomTitleLast $graph
	busy hold $graph
	update
	after 2000 "blt::DestroyZoomTitle $graph"
	busy release $graph
    } else {
	catch { $graph marker delete "zoomTitle" }
    }
}

# Push the old axis limits on the stack and set the new ones

proc blt::PushZoom { graph } {
    eval $graph marker delete [$graph marker names "zoom*"]

    global zoomInfo
    set x1 $zoomInfo($graph,A,x)
    set y1 $zoomInfo($graph,A,y)
    set x2 $zoomInfo($graph,B,x)
    set y2 $zoomInfo($graph,B,y)

    if { ($x1 == $x2) && ($y1 == $y2) } { 
	# No delta, revert to start
	return
    }

    set cmd [format {
	%s xaxis configure -min "%s" -max "%s"
	%s yaxis configure -min "%s" -max "%s"
    } $graph [$graph xaxis cget -min] [$graph xaxis cget -max] \
		 $graph [$graph yaxis cget -min] [$graph yaxis cget -max] ]

    if { $x1 > $x2 } { 
	$graph xaxis configure -min $x2 -max $x1 
    } elseif { $x1 < $x2 } {
	$graph xaxis configure -min $x1 -max $x2
    } 
    if { $y1 > $y2 } { 
	$graph yaxis configure -min $y2 -max $y1
    } elseif { $y1 < $y2 } {
	$graph yaxis configure -min $y1 -max $y2
    } 
    set zoomInfo($graph,stack) [linsert $zoomInfo($graph,stack) 0 $cmd]

    busy hold $graph
    update
    catch {busy release $graph}
}

proc blt::ResetZoom { graph } {
    global zoomInfo

    eval $graph marker delete [$graph marker names "zoom*"]
    if { $zoomInfo($graph,corner) == "A" } {
	# Reset the whole axis
	blt::PopZoom $graph
    } else {
	set zoomInfo($graph,corner) A
	bind $graph <Motion> { }
    }
}

option add *zoomTitle.font	  -*-helvetica-bold-o-*-*-18-*-*-*-*-*-*-* 
option add *zoomTitle.shadow	  black
option add *zoomTitle.foreground  yellow
option add *zoomTitle.coords	  "-Inf Inf"

proc blt::ZoomTitleNext { graph } {
    global zoomInfo
    set level [expr [llength $zoomInfo($graph,stack)] + 1]
    if { [$graph cget -invertxy] } {
	set coords "-Inf -Inf"
    } else {
	set coords "-Inf Inf"
    }
    $graph marker create text -name "zoomTitle" -text "Zoom #$level" \
	-coords $coords -bindtags "" -anchor nw
}

proc blt::ZoomTitleLast { graph } {
    global zoomInfo
    set level [llength $zoomInfo($graph,stack)]
    if { $level > 0 } {
     	$graph marker create text -name "zoomTitle" -text "Zoom #$level" -anchor nw
    }
}

proc blt::SetZoomPoint { graph x y } {
    global zoomInfo
    blt::GetCoords $graph $x $y $zoomInfo($graph,corner)
    if { $zoomInfo($graph,corner) == "A" } {
	if { ![$graph inside $x $y] } {
	    return
	}

	# First corner selected, start watching motion events

	#blt::MarkPoint $graph A
	blt::ZoomTitleNext $graph 
	bind $graph <Any-Motion> { 
	    blt::GetCoords %W %x %y B
    	    #blt::MarkPoint $graph B
	    blt::Box %W
	}
	set zoomInfo($graph,corner) B
    } else {
	bind $graph <Any-Motion> { }
	blt::PushZoom $graph 
	set zoomInfo($graph,corner) A
    }
}

option add *zoomOutline.dashes		"dash"
option add *zoomTitle.anchor		nw
option add *zoomOutline.lineWidth	2
option add *zoomOutline.xor		yes

proc blt::Box { graph } {
    global zoomInfo

    if { $zoomInfo($graph,A,x) > $zoomInfo($graph,B,x) } { 
	set x1 $zoomInfo($graph,B,x)
	set x2 $zoomInfo($graph,A,x)
	set y1 $zoomInfo($graph,B,y)
	set y2 $zoomInfo($graph,A,y)
    } else {
	set x1 $zoomInfo($graph,A,x)
	set x2 $zoomInfo($graph,B,x)
	set y1 $zoomInfo($graph,A,y)
	set y2 $zoomInfo($graph,B,y)
    }
    set coords { $x1 $y1 $x2 $y1 $x2 $y2 $x1 $y2 $x1 $y1 }
    if { [$graph marker exists "zoomOutline"] } {
	$graph marker configure "zoomOutline" -coords $coords 
    } else {
	$graph marker create line -coords $coords -name "zoomOutline" \
	    -mapx [$graph xaxis use] -mapy [$graph yaxis use] 
    }
}

proc Blt_PostScriptDialog { graph } {
    set top $graph.top
    toplevel $top

    foreach var { center landscape maxpect preview decorations padx 
	pady paperwidth paperheight width height colormode } {
	global $graph.$var
	set $graph.$var [$graph postscript cget -$var]
    }
    set row 1
    set col 0
    label $top.title -text "PostScript Options"
    table $top $top.title -cspan 7
    foreach bool { center landscape maxpect preview decorations } {
	set w $top.$bool-label
	label $w -text "-$bool" -font *courier*-r-*12* 
	table $top $row,$col $w -anchor e -pady { 2 0 } -padx { 0 4 }
	set w $top.$bool-yes
	global $graph.$bool
	radiobutton $w -text "yes" -variable $graph.$bool -value 1
	table $top $row,$col+1 $w -anchor w
	set w $top.$bool-no
	radiobutton $w -text "no" -variable $graph.$bool -value 0
	table $top $row,$col+2 $w -anchor w
	incr row
    }
    label $top.modes -text "-colormode" -font *courier*-r-*12* 
    table $top $row,0 $top.modes -anchor e  -pady { 2 0 } -padx { 0 4 }
    set col 1
    foreach m { color greyscale } {
	set w $top.$m
	radiobutton $w -text $m -variable $graph.colormode -value $m
	table $top $row,$col $w -anchor w
	incr col
    }
    set row 1
    frame $top.sep -width 2 -bd 1 -relief sunken
    table $top $row,3 $top.sep -fill y -rspan 6
    set col 4
    foreach value { padx pady paperwidth paperheight width height } {
	set w $top.$value-label
	label $w -text "-$value" -font *courier*-r-*12* 
	table $top $row,$col $w -anchor e  -pady { 2 0 } -padx { 0 4 }
	set w $top.$value-entry
	global $graph.$value
	entry $w -textvariable $graph.$value -width 8
	table $top $row,$col+1 $w -cspan 2 -anchor w -padx 8
	incr row
    }
    table configure $top c3 -width .125i
    button $top.cancel -text "Cancel" -command "destroy $top"
    table $top $row,0 $top.cancel  -width 1i -pady 2 -cspan 3
    button $top.reset -text "Reset" -command "destroy $top"
    #table $top $row,1 $top.reset  -width 1i
    button $top.print -text "Print" -command "blt::ResetPostScript $graph"
    table $top $row,4 $top.print  -width 1i -pady 2 -cspan 2
}

proc blt::ResetPostScript { graph } {
    foreach var { center landscape maxpect preview decorations padx 
	pady paperwidth paperheight width height colormode } {
	global $graph.$var
	set old [$graph postscript cget -$var]
	if { [catch {$graph postscript configure -$var [set $graph.$var]}] != 0 } {
	    $graph postscript configure -$var $old
	    set $graph.$var $old
	}
    }
    $graph postscript output "out.ps"
    puts stdout "wrote file \"out.ps\"."
    flush stdout
}


proc Blt_ActiveLegend { graph } {
    global bltActiveEntry

    set bltActiveEntry($graph) "-1"
    bind bltActiveLegend <Motion>  {
	BltActivateLegend %W %x %y
    }    
    BltAddBindTag $graph bltActiveLegend 
}

proc Blt_Crosshairs { graph } {
    bind bltCrosshairs <Any-Motion>   {
	%W crosshairs configure -position @%x,%y 
    }
    $graph crosshairs configure -color red
    bind $graph <Enter> [format {
	BltAddBindTag %s bltCrosshairs  
	%s crosshairs on
    } $graph $graph]
    bind $graph <Leave> [format {
	BltRemoveBindTag %s bltCrosshairs  
	%s crosshairs off
    } $graph $graph]
}

proc Blt_ZoomStack { graph } {
    global bltZoom

    set bltZoom($graph,A,x) {}
    set bltZoom($graph,A,y) {}
    set bltZoom($graph,B,x) {}
    set bltZoom($graph,B,y) {}
    set bltZoom($graph,stack) {}
    set bltZoom($graph,corner) A

    bind $graph <1> { 
	BltSetZoomPoint %W %x %y 
    }
    bind $graph <ButtonPress-3> {
	BltResetZoom %W 
    }
}

proc Blt_PrintKey { graph } {
    bind bltPrintGraph <Shift-ButtonRelease-3>  {
	%W postscript output "out.ps"  -landscape 1 -maxpect 1 -decorations 0
	puts stdout "wrote file \"out.ps\"."
	flush stdout
	break
    }
    BltAddBindTag $graph bltPrintGraph 
}

proc Blt_ClosestPoint { graph } {
    bind bltClosestPoint <Control-ButtonPress-1>  {
	BltFindElement %W %x %y
	break
    }
    BltAddBindTag $graph bltClosestPoint 
}

proc BltAddBindTag { graph name } {
    set oldtags [bindtags $graph]
    if { [lsearch $oldtags $name] < 0 } {
	bindtags $graph [concat $name $oldtags]
    }
}

proc BltRemoveBindTag { graph name } {
    set tagList {}
    foreach tag [bindtags $graph] {
	if { $tag != $name } {
	    lappend tagList $tag
	}
    }
    bindtags $graph $tagList
}

proc BltActivateLegend { graph x y } {
    global bltActiveEntry

    set old $bltActiveEntry($graph)
    set new [$graph legend get @$x,$y]
    if { $old != $new } {
	if { $old != "-1" } {
	    $graph legend deactivate $old
	    $graph element deactivate $old
	}
	if { $new != "" } {
	    $graph legend activate $new
	    $graph element activate $new
	}
    }
    set bltActiveEntry($graph) $new
}


proc BltFindElement { graph x y } {
    if ![$graph element closest $x $y info -interpolate 1] {
	bell
	return
    }
    # --------------------------------------------------------------
    # find(name)		- element Id
    # find(index)		- index of closest point
    # find(x) find(y)		- coordinates of closest point
    #				  or closest point on line segment.
    # find(dist)		- distance from sample coordinate
    # --------------------------------------------------------------
    set markerName "bltClosest_$info(name)"
    $graph marker delete $markerName
    $graph marker create text -coords { $info(x) $info(y) } \
	-name $markerName \
	-text "$info(name): $info(dist)\nindex $info(index)" \
	-font *lucida*-r-*-10-* \
	-anchor center -justify left \
	-yoffset 0 -bg {}
    BltFlashPoint $graph $info(name) $info(index) 10
}

proc BltFlashPoint { graph name index count } {
    if { $count & 1 } {
        $graph element deactivate $name 
    } else {
        $graph element activate $name $index
    }
    incr count -1
    if { $count > 0 } {
	after 200 [list catch [list BltFlashPoint $graph $name $index $count]]
	update
    } else {
       eval $graph marker delete [$graph marker names ""bltClosest_*"]
    }
}

proc BltGetCoords { graph x y index } {

    set coords [$graph invtransform $x $y]
    set x [lindex $coords 0]
    set y [lindex $coords 1]

    scan [$graph xaxis limits] "%s %s" xmin xmax
    scan [$graph yaxis limits] "%s %s" ymin ymax

    if { $x > $xmax } { 
	set x $xmax 
    } elseif { $x < $xmin } { 
	set x $xmin 
    }

    if { $y > $ymax } { 
	set y $ymax 
    } elseif { $y < $ymin } { 
	set y $ymin 
    }
    global bltZoom
    set bltZoom($graph,$index,x) $x
    set bltZoom($graph,$index,y) $y
}

proc BltMarkPoint { graph index } {
    global bltZoom
    set x $bltZoom($graph,$index,x)
    set y $bltZoom($graph,$index,y)

    set marker "bltZoom_text_$index"
    set text [format "x=%.4g\ny=%.4g" $x $y] 

    if [$graph marker exists $marker] {
     	$graph marker configure $marker -coords { $x $y } -text $text 
    } else {
    	$graph marker create text -coords { $x $y } -name $marker \
   	    -font *lucida*-r-*-10-* \
	    -text $text -anchor center -bg {} -justify left
    }
}

proc BltPopZoom { graph } {
    global bltZoom

    set zoomStack $bltZoom($graph,stack)
    if { [llength $zoomStack] > 0 } {
	set cmd [lindex $zoomStack 0]
	set bltZoom($graph,stack) [lrange $zoomStack 1 end]
	eval $cmd
	BltZoomTitleLast $graph
	blt::busy hold $graph
	update
	set cmd [format {
	    if { $bltZoom(%s,corner) == "A" } {
		%s marker delete "bltZoom_title"
	    }
	} $graph $graph ]
	after 2000 [list catch $cmd]
	blt::busy release $graph
    } else {
       catch {$graph marker delete "bltZoom_title"}
    }
}

# Push the old axis limits on the stack and set the new ones

proc BltPushZoom { graph } {
    eval $graph marker delete [$graph marker names "bltZoom_*"]

    global bltZoom
    set x1 $bltZoom($graph,A,x)
    set y1 $bltZoom($graph,A,y)
    set x2 $bltZoom($graph,B,x)
    set y2 $bltZoom($graph,B,y)

    if { ($x1 == $x2) && ($y1 == $y2) } { 
	# No delta, revert to start
	return
    }

    set cmd [format {
	%s xaxis configure -min "%s" -max "%s"
	%s yaxis configure -min "%s" -max "%s"
    } $graph [$graph xaxis cget -min] [$graph xaxis cget -max] \
		 $graph [$graph yaxis cget -min] [$graph yaxis cget -max] ]

    if { $x1 > $x2 } { 
	$graph xaxis configure -min $x2 -max $x1 
    } elseif { $x1 < $x2 } {
	$graph xaxis configure -min $x1 -max $x2
    } 
    if { $y1 > $y2 } { 
	$graph yaxis configure -min $y2 -max $y1
    } elseif { $y1 < $y2 } {
	$graph yaxis configure -min $y1 -max $y2
    } 
    set bltZoom($graph,stack) [linsert $bltZoom($graph,stack) 0 $cmd]

    blt::busy hold $graph
    update
    catch {blt::busy release $graph}
}

proc BltResetZoom { graph } {
    global bltZoom

    eval $graph marker delete [$graph marker names "bltZoom_*"]
    if { $bltZoom($graph,corner) == "A" } {
	# Reset the whole axis
	BltPopZoom $graph
    } else {
	set bltZoom($graph,corner) A
	bind $graph <Motion> { }
    }
}

proc BltZoomTitleNext { graph } {
    global bltZoom

    set level [expr [llength $bltZoom($graph,stack)] + 1]
    set title "Zoom #$level"
    $graph marker create text -name "bltZoom_title" -text $title \
	    -coords {-Inf Inf} -anchor nw -bg {} 
}

proc BltZoomTitleLast { graph } {
    global bltZoom

    set level [llength $bltZoom($graph,stack)]
    if { $level > 0 } {
	set title "Zoom #$level"
     	$graph marker create text -name "bltZoom_title" -text $title \
		-coords {-Inf Inf} -anchor nw -bg {} 
    }
}

proc BltSetZoomPoint { graph x y } {
    global bltZoom

    BltGetCoords $graph $x $y $bltZoom($graph,corner)
    if { $bltZoom($graph,corner) == "A" } {
	# First corner selected, start watching motion events

	#BltMarkPoint $graph A
	BltZoomTitleNext $graph 
	bind $graph <Any-Motion> { 
	    BltGetCoords %W %x %y B
    	    #BltMarkPoint $graph B
	    BltBox %W
	}
	set bltZoom($graph,corner) B
    } else {
	bind $graph <Any-Motion> { }
	BltPushZoom $graph 
	set bltZoom($graph,corner) A
    }
}

proc BltBox { graph } {
    global bltZoom

    if { $bltZoom($graph,A,x) > $bltZoom($graph,B,x) } { 
	set x1 $bltZoom($graph,B,x)
	set x2 $bltZoom($graph,A,x) 
	set y1 $bltZoom($graph,B,y)
	set y2 $bltZoom($graph,A,y) 
    } else {
	set x1 $bltZoom($graph,A,x)
	set x2 $bltZoom($graph,B,x) 
	set y1 $bltZoom($graph,A,y)
	set y2 $bltZoom($graph,B,y) 
    }
    set coords {
	$x1 $y1 $x1 $y2 $x1 $y1 $x2 $y1 $x2 $y1 $x2 $y2 $x1 $y2 $x2 $y2 
    }
    if [$graph marker exists "bltZoom_outline"] {
	$graph marker configure "bltZoom_outline" -coords $coords
    } else {
	$graph marker create line -coords $coords -name "bltZoom_outline" \
	    -dashes { 4 2 }
    }
    $graph marker before "bltZoom_outline"
} 

# blt-2.4 version is above, 2.1 below
if {[info exists blt_version] && $blt_version >= 2.4} {
    return
}

proc Blt_ActiveLegend { graph } {
    global bltActiveEntry

    set bltActiveEntry($graph) "-1"
    bind bltActiveLegend <Motion>  {
	BltActivateLegend %W %x %y
    }    
    BltAddBindTag $graph bltActiveLegend 
}

proc Blt_Crosshairs { graph } {
    bind bltCrosshairs <Any-Motion>   {
	%W crosshairs configure -position @%x,%y 
    }
    $graph crosshairs configure -color red
    bind $graph <Enter> [format {
	BltAddBindTag %s bltCrosshairs  
	%s crosshairs on
    } $graph $graph]
    bind $graph <Leave> [format {
	BltRemoveBindTag %s bltCrosshairs  
	%s crosshairs off
    } $graph $graph]
}

proc Blt_ZoomStack { graph } {
    global bltZoom

    set bltZoom($graph,A,x) {}
    set bltZoom($graph,A,y) {}
    set bltZoom($graph,B,x) {}
    set bltZoom($graph,B,y) {}
    set bltZoom($graph,stack) {}
    set bltZoom($graph,corner) A

    bind $graph <1> { 
	BltSetZoomPoint %W %x %y 
    }
    bind $graph <ButtonPress-3> {
	BltResetZoom %W 
    }
}

proc Blt_PrintKey { graph } {
    bind bltPrintGraph <Shift-ButtonRelease-3>  {
	%W postscript output "out.ps"  -landscape 1 -maxpect 1 -decorations 0
	puts stdout "wrote file \"out.ps\"."
	flush stdout
	break
    }
    BltAddBindTag $graph bltPrintGraph 
}

proc Blt_ClosestPoint { graph } {
    bind bltClosestPoint <Control-ButtonPress-1>  {
	BltFindElement %W %x %y
	break
    }
    BltAddBindTag $graph bltClosestPoint 
}

proc BltAddBindTag { graph name } {
    set oldtags [bindtags $graph]
    if { [lsearch $oldtags $name] < 0 } {
	bindtags $graph [concat $name $oldtags]
    }
}

proc BltRemoveBindTag { graph name } {
    set tagList {}
    foreach tag [bindtags $graph] {
	if { $tag != $name } {
	    lappend tagList $tag
	}
    }
    bindtags $graph $tagList
}

proc BltActivateLegend { graph x y } {
    global bltActiveEntry

    set old $bltActiveEntry($graph)
    set new [$graph legend get @$x,$y]
    if { $old != $new } {
	if { $old != "-1" } {
	    $graph legend deactivate $old
	    $graph element deactivate $old
	}
	if { $new != "" } {
	    $graph legend activate $new
	    $graph element activate $new
	}
    }
    set bltActiveEntry($graph) $new
}


proc BltFindElement { graph x y } {
    if ![$graph element closest $x $y info -interpolate 1] {
	bell
	return
    }
    # --------------------------------------------------------------
    # find(name)		- element Id
    # find(index)		- index of closest point
    # find(x) find(y)		- coordinates of closest point
    #				  or closest point on line segment.
    # find(dist)		- distance from sample coordinate
    # --------------------------------------------------------------
    set markerName "bltClosest_$info(name)"
    $graph marker delete $markerName
    $graph marker create text -coords { $info(x) $info(y) } \
	-name $markerName \
	-text "$info(name): $info(dist)\nindex $info(index)" \
	-font *lucida*-r-*-10-* \
	-anchor center -justify left \
	-yoffset 0 -bg {}
    BltFlashPoint $graph $info(name) $info(index) 10
}

proc BltFlashPoint { graph name index count } {
    if { $count & 1 } {
        $graph element deactivate $name 
    } else {
        $graph element activate $name $index
    }
    incr count -1
    if { $count > 0 } {
	after 200 [list catch [list BltFlashPoint $graph $name $index $count]]
	update
    } else {
        eval $graph marker delete [$graph marker names "bltClosest_*"]
    }
}

proc BltGetCoords { graph x y index } {

    set coords [$graph invtransform $x $y]
    set x [lindex $coords 0]
    set y [lindex $coords 1]

    scan [$graph xaxis limits] "%s %s" xmin xmax
    scan [$graph yaxis limits] "%s %s" ymin ymax

    if { $x > $xmax } { 
	set x $xmax 
    } elseif { $x < $xmin } { 
	set x $xmin 
    }

    if { $y > $ymax } { 
	set y $ymax 
    } elseif { $y < $ymin } { 
	set y $ymin 
    }
    global bltZoom
    set bltZoom($graph,$index,x) $x
    set bltZoom($graph,$index,y) $y
}

proc BltMarkPoint { graph index } {
    global bltZoom
    set x $bltZoom($graph,$index,x)
    set y $bltZoom($graph,$index,y)

    set marker "bltZoom_text_$index"
    set text [format "x=%.4g\ny=%.4g" $x $y] 

    if [$graph marker exists $marker] {
     	$graph marker configure $marker -coords { $x $y } -text $text 
    } else {
    	$graph marker create text -coords { $x $y } -name $marker \
   	    -font *lucida*-r-*-10-* \
	    -text $text -anchor center -bg {} -justify left
    }
}

proc BltPopZoom { graph } {
    global bltZoom

    set zoomStack $bltZoom($graph,stack)
    if { [llength $zoomStack] > 0 } {
	set cmd [lindex $zoomStack 0]
	set bltZoom($graph,stack) [lrange $zoomStack 1 end]
	eval $cmd
	BltZoomTitleLast $graph
	blt::busy hold $graph
	update
	set cmd [format {
	    if { $bltZoom(%s,corner) == "A" } {
		%s marker delete "bltZoom_title"
	    }
	} $graph $graph ]
	after 2000 [list catch $cmd]
	blt::busy release $graph
    } else {
	$graph marker delete "bltZoom_title"
    }
}

# Push the old axis limits on the stack and set the new ones

proc BltPushZoom { graph } {
    eval $graph marker delete [$graph marker names "bltZoom_*"]

    global bltZoom
    set x1 $bltZoom($graph,A,x)
    set y1 $bltZoom($graph,A,y)
    set x2 $bltZoom($graph,B,x)
    set y2 $bltZoom($graph,B,y)

    if { ($x1 == $x2) && ($y1 == $y2) } { 
	# No delta, revert to start
	return
    }

    set cmd [format {
	%s xaxis configure -min "%s" -max "%s"
	%s yaxis configure -min "%s" -max "%s"
    } $graph [$graph xaxis cget -min] [$graph xaxis cget -max] \
		 $graph [$graph yaxis cget -min] [$graph yaxis cget -max] ]

    if { $x1 > $x2 } { 
	$graph xaxis configure -min $x2 -max $x1 
    } elseif { $x1 < $x2 } {
	$graph xaxis configure -min $x1 -max $x2
    } 
    if { $y1 > $y2 } { 
	$graph yaxis configure -min $y2 -max $y1
    } elseif { $y1 < $y2 } {
	$graph yaxis configure -min $y1 -max $y2
    } 
    set bltZoom($graph,stack) [linsert $bltZoom($graph,stack) 0 $cmd]

    blt::busy hold $graph
    update
    catch {blt::busy release $graph}
}

proc BltResetZoom { graph } {
    global bltZoom

    eval $graph marker delete [$graph marker names "bltZoom_*"]
    if { $bltZoom($graph,corner) == "A" } {
	# Reset the whole axis
	BltPopZoom $graph
    } else {
	set bltZoom($graph,corner) A
	bind $graph <Motion> { }
    }
}

proc BltZoomTitleNext { graph } {
    global bltZoom

    set level [expr [llength $bltZoom($graph,stack)] + 1]
    set title "Zoom #$level"
    $graph marker create text -name "bltZoom_title" -text $title \
	    -coords {-Inf Inf} -anchor nw -bg {} 
}

proc BltZoomTitleLast { graph } {
    global bltZoom

    set level [llength $bltZoom($graph,stack)]
    if { $level > 0 } {
	set title "Zoom #$level"
     	$graph marker create text -name "bltZoom_title" -text $title \
		-coords {-Inf Inf} -anchor nw -bg {} 
    }
}

proc BltSetZoomPoint { graph x y } {
    global bltZoom

    BltGetCoords $graph $x $y $bltZoom($graph,corner)
    if { $bltZoom($graph,corner) == "A" } {
	# First corner selected, start watching motion events

	#BltMarkPoint $graph A
	BltZoomTitleNext $graph 
	bind $graph <Any-Motion> { 
	    BltGetCoords %W %x %y B
    	    #BltMarkPoint $graph B
	    BltBox %W
	}
	set bltZoom($graph,corner) B
    } else {
	bind $graph <Any-Motion> { }
	BltPushZoom $graph 
	set bltZoom($graph,corner) A
    }
}

proc BltBox { graph } {
    global bltZoom

    if { $bltZoom($graph,A,x) > $bltZoom($graph,B,x) } { 
	set x1 $bltZoom($graph,B,x)
	set x2 $bltZoom($graph,A,x) 
	set y1 $bltZoom($graph,B,y)
	set y2 $bltZoom($graph,A,y) 
    } else {
	set x1 $bltZoom($graph,A,x)
	set x2 $bltZoom($graph,B,x) 
	set y1 $bltZoom($graph,A,y)
	set y2 $bltZoom($graph,B,y) 
    }
    set coords {
	$x1 $y1 $x1 $y2 $x1 $y1 $x2 $y1 $x2 $y1 $x2 $y2 $x1 $y2 $x2 $y2 
    }
    if [$graph marker exists "bltZoom_outline"] {
	$graph marker configure "bltZoom_outline" -coords $coords
    } else {
	$graph marker create line -coords $coords -name "bltZoom_outline" \
	    -dashes { 4 2 }
    }
    $graph marker before "bltZoom_outline"
}

