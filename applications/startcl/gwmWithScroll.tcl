# gwmWithScroll.tcl
#
# This file defines the procedure gwm_gwmWithScroll which creates a gwm
# widget with scrollbars that can be used to scroll the window around if
# it is too large to be all visible.

# gwm_gwmWithScroll:
#
# Arguments:
#
# w -           The name for the frame widget.
# args -        Options to be used when creating the gwm widget.
#
# Returned:
#               The name of the gwm widget.
#

proc gwm_gwmWithScroll {w args} {

# Create a frame for the gwm window.
    frame $w -relief sunken -border 2

# Create the gwm widget.
    set gwm [concat "gwm $w.gwm" $args]
    if {[catch $gwm]!=0} {
	destroy $w
	frame $w -relief sunken -border 2 -colormap new
	eval $gwm
        puts [winfo toplevel $w]
	wm colormapwindows [winfo toplevel $w] "$w [winfo toplevel $w]"
        puts [wm colormapwindows [winfo toplevel $w]]
    }

# Get the gwmname resource and re-create the widget with that name
     set gwmname gwm
#    set gwmname [$w.gwm cget -gwmname]
#    destroy $w.gwm
#    set gwm [concat "gwm $w.$gwmname" $args]
#    eval $gwm

# "olv" shows whether or not the gwm widget has an overlay plane.
    set overlay [$w.$gwmname cget -overlay]

# Create a frame to hold the vertical scroll bar(s).
    pack [frame $w.f] -side right -fill y

# Pack a scroll bar into the frame, find its width and unpack it again.
    pack [scrollbar $w.yscroll -relief ridge -bd 3 \
	-command "gwm_yScroll $w.yscroll $w.gwm -yoffset"] \
	-side right -in $w.f
    set sw [$w.yscroll cget -width]
    incr sw 6
    pack forget $w.yscroll

# Put a label widget at the bottom of the frame to fill the corner so that
# the scroll bars don't overlap.
    if $overlay {incr sw $sw}
    pack [frame $w.dummy -width $sw -height $sw -bd 1 -relief raised] \
	    -in $w.f -side bottom

# Re-pack the vertical scroll bar and add one for the overlay plane (if there
# is one).
    pack $w.yscroll -side right -fill y -in $w.f
    if $overlay {
        pack [scrollbar $w.yovscroll -relief ridge -bd 3 \
            -command "gwm_yScroll $w.yovscroll $w.$gwmname -yovoffset"] \
            -in $w.f -side right -fill y
    }

# Create and pack the horizontal scroll bar(s).
    pack [scrollbar $w.xscroll -orient horizontal -relief ridge -bd 3 \
	-command "gwm_xScroll $w.xscroll $w.$gwmname -xoffset"] \
	-side bottom -fill x -in $w
    if $overlay {
        pack [scrollbar $w.xovscroll -orient horizontal -relief ridge -bd 3 \
            -command "gwm_xScroll $w.xovscroll $w.$gwmname -xovoffset"] \
	    -side bottom -fill x -in $w
    }

# Set up bindings to "configure" events on the gwm widget so that the
# positions of the scroll bars are updated if the window is resized.

    if $overlay {
	bind $w.$gwmname <Configure> " \
	    gwm_xScroll $w.xscroll $w.$gwmname -xoffset
	    gwm_yScroll $w.yscroll $w.$gwmname -yoffset
	    gwm_xScroll $w.xovscroll $w.$gwmname -xovoffset
	    gwm_yScroll $w.yovscroll $w.$gwmname -yovoffset"
    } {
	bind $w.$gwmname <Configure> " \
	    gwm_xScroll $w.xscroll $w.$gwmname -xoffset
	    gwm_yScroll $w.yscroll $w.$gwmname -yoffset"
    }

# Pack the gwm widget into the frame.
    pack $w.$gwmname -in $w

# Return the name of the gwm widget.
    return $w.$gwmname
}

# gwm_xScroll:
#
# Arguments:
#
# wscroll -     The name of the scroll widget.
# wgwm -        The name of the gwm widget.
# scrollopt -   The gwm widget option to update
# val -         The scrollbar position
#
proc gwm_xScroll {scrollw gwmw scrollopt {val none}} {

# Configure the gwm widget

    if ![string compare $val none] {
	set val [expr - [$gwmw cget $scrollopt]]
    }
    $gwmw configure $scrollopt [expr -$val]

# Get the sizes of the gwm widget and its window.
    set gwmsize [expr [$gwmw cget -width]]
    set winsize [winfo width $gwmw]

# Set the new scroll bar positions.
    $scrollw set [expr $gwmsize] $winsize [expr $val] \
	[expr $val+$winsize]
}

# gwm_yScroll:
#
# Arguments:
#
# wscroll -     The name of the scroll widget.
# wgwm -        The name of the gwm widget.
# scrollopt -   The gwm widget option to update
# val -         The scrollbar position
#
proc gwm_yScroll {scrollw gwmw scrollopt {val none}} {

# Configure the gwm widget
    if ![string compare $val none] {
	set val [expr - [$gwmw cget $scrollopt]]
    }
    $gwmw configure $scrollopt [expr -$val]

# Get the sizes of the gwm widget and its window.
    set gwmsize [expr [$gwmw cget -height]]
    set winsize [winfo height $gwmw]

# Set the new scroll bar positions.
    $scrollw set [expr $gwmsize] $winsize [expr $val] \
	[expr $val+$winsize]
}
