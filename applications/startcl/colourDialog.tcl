# colourDialog.tcl
#
# This file defines the procedure gwm_colourDialog, which creates a
# dialog box for manipulating the colours of a gwm widget.

# gwm_colourDialog:
#
# This procedure creates a modal dialog box with controls for setting the
# colours of a gwm widget. The control that activated the dialog is disabled
# until the dialog box is deleted to prevent an attempt to create a second
# copy.
#
# Arguments:
#
# w -           Window to use for dialog top-level.
# gwm -         The gwm wdiget to be manuipulated.
# c - 	        The control used to pop up the dialog box.
#

proc gwm_colourDialog {w gwm c} {
    global gwm_priv

# Create the top level window.
    toplevel $w -class Dialog -bd 10
    wm title $w "Gwm Colours"
    wm iconname $w Dialog
    wm transient $w .

# Bind a command to re-enable the control that popped us up to the destroy
# event for the top level window.
    bind $w <Destroy> "catch {$c configure -state normal}"

# Initialise the elements of the global array gwm_priv that are used to
# save the state of the dialog box if they don't exist yet.
    if [catch {set gwm_priv($w,pixel_value)}] {set gwm_priv($w,pixel_value) 1}
    if [catch {set gwm_priv($w,colour_select)}] {
	set gwm_priv($w,colour_select) bg
    }

# Create and pack 3 frames arranged vertically
    pack [frame $w.top] [frame $w.mid] [frame $w.bot] -fill x

# Create and pack a frame into the right hand side of the top frame to
# hold the radio buttons for selecting the type of colour (background,
# foreground, overlay or "other").
    pack [frame $w.top.r] [frame $w.top.pad -width 10] -side left -expand y \
	-fill y

# Create the radio buttons and pack them into the frame.
    radiobutton $w.top.r.bg -text Background \
	-variable gwm_priv($w,colour_select) -value bg -relief flat \
	-command "gwm_selectPixelValue $w $gwm"
    radiobutton $w.top.r.fg -text Foreground \
	-variable gwm_priv($w,colour_select) -value fg -relief flat \
	-command "gwm_selectPixelValue $w $gwm"
    radiobutton $w.top.r.ov -text Overlay -variable gwm_priv($w,colour_select) \
	-value ovl -relief flat -command "gwm_selectPixelValue $w $gwm"
    radiobutton $w.top.r.oth -text Other -variable gwm_priv($w,colour_select) \
	-value oth -relief flat -command "gwm_selectPixelValue $w $gwm"
    radiobutton $w.top.r.ch -text Crosshair \
	-variable gwm_priv($w,colour_select) -value ch -relief flat \
	-command "gwm_selectPixelValue $w $gwm"

    pack $w.top.r.bg $w.top.r.fg $w.top.r.oth $w.top.r.ov $w.top.r.ch \
	-anchor w -pady 2 -expand y

# Create a frame for the r g b scales and pack it into the left hand side
# of the top frame.
    pack [frame $w.top.s] -side left -expand y

# Create the r g b scales and pack them into the frame.
    scale $w.top.s.red -fg red -label Red -orient horizontal \
        -sliderlength 50 -length 200 \
	-to 1.0 -command "gwm_setColourScales $w $gwm" -resolution 0.001
    scale $w.top.s.green -fg green -label Green -orient horizontal \
        -sliderlength 50 -length 200 \
	-to 1.0 -command "gwm_setColourScales $w $gwm" -resolution 0.001
    scale $w.top.s.blue -fg blue -label Blue -orient horizontal \
        -sliderlength 50 -length 200 \
	-to 1.0 -command "gwm_setColourScales $w $gwm" -resolution 0.001
    pack $w.top.s.red $w.top.s.green $w.top.s.blue -anchor w -pady 2

# Create the scale for setting the pixel value and pack it into the middle
# frame. The scale maximum is set to the number of colours in the gwm
# widget minus 1.
    set maxcol [$gwm cget -colours]
    incr maxcol -1
    scale $w.mid.pixel -label "Pixel value" -showvalue yes -orient horizontal \
	-to $maxcol -from 0 -command "gwm_selectPixelValue $w $gwm" \
	-state disabled -length 200
    pack $w.mid.pixel -side left -fill x

# Create a label widget to display the selected colour and pack it into
# the middle frame along side the scale.
    label $w.mid.sample -height 5 -width 10 -bd 2 -relief raised
    pack $w.mid.sample -side top -pady 10 -expand y

# Create the OK button with a default border and pack it into the bottom
# frame.
    button $w.bot.ok -text OK -command "destroy $w"

    frame $w.bot.default -relief sunken -bd 1
    raise $w.bot.ok $w.bot.default
    pack $w.bot.default -side left -expand 1 -padx 3m -pady 2m
    pack $w.bot.ok -in $w.bot.default -padx 1m -pady 1m -ipadx 1m

# Add a Help button
    button $w.bot.help -text Help -command "displayHelp colour"
    pack $w.bot.help -side left -expand 1 -padx 3m -pady 2m

# If the gwm widget doesn't have an overlay plane then disable the overlay
# radio button.
    if {[$gwm cget -overlay]==0} {
	$w.top.r.ov configure -state disabled
    }

# If the colour table is fixed then disable all buttons except the crosshair
# and select the crosshair button.
    set visual [winfo visual $gwm]
    if { $visual == "truecolor" || $visual == "staticcolor" || \
	$visual == "staticgray" } {
	$w.top.r.bg configure -state disabled
	$w.top.r.fg configure -state disabled
	$w.top.r.ov configure -state disabled
	$w.top.r.oth configure -state disabled
	set gwm_priv($w,colour_select) ch
    }


# Select the colour type radiobutton that was last selected.
    switch $gwm_priv($w,colour_select) {
	bg {set ctentry 0}
	fg {set ctentry 1}
	ovl {set ctentry -1}
	ch {set ctentry -2}
	oth {set ctentry $gwm_priv($w,pixel_value)}
    }

# If "other" is selected then enable the pixel scale.
    if ![string compare $gwm_priv($w,colour_select) oth] {
	$w.mid.pixel configure -state normal
    }

# Set the pixel value scale to the last value.
    $w.mid.pixel set $gwm_priv($w,pixel_value)

# Set the r g b scales to the currently selected colour.
    if ($ctentry==-2) {
	set colour [$gwm cget -crosscolour]
    } {
	set colour [$gwm get colour $ctentry]
    }
    set rgb [winfo rgb $gwm $colour]
    $w.top.s.red set [expr [lindex $rgb 0]/65535.0]
    $w.top.s.green set [expr [lindex $rgb 1]/65535.0]
    $w.top.s.blue set [expr [lindex $rgb 2]/65535.0]

# Set the colour of the sample label.
    $w.mid.sample configure -bg $colour

# Disable the control that popped us up
    $c configure -state disabled

# Withdraw the window, then update all the geometry information
# so we know how big it wants to be, then center the window in
# parent and de-iconify it.
    wm withdraw $w
    update idletasks
    set parent [winfo parent $w]
    set x [expr [winfo width $parent]/2 - [winfo reqwidth $w]/2 \
	+ [winfo x $parent]]
    set y [expr [winfo height $parent]/2 - [winfo reqheight $w]/2 \
	+ [winfo y $parent]]
    wm geom $w +$x+$y
    wm resizable $w 0 0
    wm deiconify $w
}

# gwm_setColourScales:
#
# This procedure is called whenever one of the r g b scales is moved.
# It sets the appropriate colour in the gwm widget and adjusts the r g b
# scales.
#
# Arguments:
#
# w -           The dialog box window.
# gwm -         The gwm window being manipulated.
# val -		The scale value.
#
proc gwm_setColourScales {w gwm val} {
    global gwm_priv

# Set the colour table entry that is being changed (-1 means the overlay
# colour, -2 means the crosshair).
    switch $gwm_priv($w,colour_select) {
	bg {set ctentry 0}
	fg {set ctentry 1}
	ovl {set ctentry -1}
	ch {set ctentry -2}
	oth {set ctentry $gwm_priv($w,pixel_value)}
    }

# If the display is monochrome then set all the scales to the value of
# the one that moved.
    set visual [winfo visual $gwm]
    if { $visual == "grayscale" || $visual == "staticgray" } {
	$w.top.s.red set $val
	$w.top.s.green set $val
	$w.top.s.blue set $val
    }

# Fetch the r g b scale settings.
    set red [$w.top.s.red get]
    set green [$w.top.s.green get]
    set blue [$w.top.s.blue get]

# Format the r g b values into a X colour specification string.
    set colour [format "#%4.4x%4.4x%4.4x" [expr int($red*65535)] \
	[expr int($green*65535)] [expr int($blue*65535)]]

# Set the gwm widget colour table
    if {$ctentry == -2} {
	$gwm configure -crosscolour $colour
    } {
	$gwm set colour $ctentry $colour
    }

# Set the colour of the sample label widget.
    $w.mid.sample configure -bg $colour

# If either the background or foreground was changed then set the
# appropriate gwm widget resource as well.
    if ![string compare $gwm_priv($w,colour_select) bg] {
	$gwm configure -bg $colour
    }
    if ![string compare $gwm_priv($w,colour_select) fg] {
	$gwm configure -fg $colour
    }
}

# gwm_setPixelValue:
#
# This procedure is called whenever the pixel scale is moved or one of the
# colour type radio buttons is selected.
#
# Arguments:
#
# w -           The dialog box window.
# gwm -         The gwm window being manuipulated.
#
proc gwm_selectPixelValue {w gwm {val ""}} {
    global gwm_priv

# If this procedure has been called because the pixel scale has moved then
# val will have an integer value. Save it in the global array.
    if {$val!=""} {set gwm_priv($w,pixel_value) $val}

# Select the colour table entry to display in the sample label.
    switch $gwm_priv($w,colour_select) {
	bg {set ctentry 0}
	fg {set ctentry 1}
	ovl {set ctentry -1}
	ch {set ctentry -2}
	oth {set ctentry $gwm_priv($w,pixel_value)}
    }

# Get the colour from the gwm widget.
    if ($ctentry==-2) {
	set colour [$gwm cget -crosscolour]
    } {
	set colour [$gwm get colour $ctentry]
    }
    set rgb [winfo rgb $gwm $colour]

# Set the r g b scales. This will cause the scale move callback to be called
# which will update the sample label.
    $w.top.s.red set [expr [lindex $rgb 0]/65535.0]
    $w.top.s.green set [expr [lindex $rgb 1]/65535.0]
    $w.top.s.blue set [expr [lindex $rgb 2]/65535.0]

# Enable or disable the pixel scale according to the colour type now
# selected.
    if [string compare $gwm_priv($w,colour_select) oth] {
	$w.mid.pixel configure -state disabled
    } {
	$w.mid.pixel configure -state normal
    }
}
