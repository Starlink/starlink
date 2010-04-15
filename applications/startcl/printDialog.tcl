# printDialog.tcl
#
# This file defines the procedure gwm_printDialog, which creates a modal
# dialog box for printing the contents of a gwm widget.

# gwm_printDialog:
#
# This procedure creates a modal dialog box with controls printing a
# gwm widget. The control that activated the dialog is disabled
# until the printing operation is complete to prevent attempts to start
# a second print before the previous one has finished.
#
# Arguments:
#
# w -           Window to use for dialog top-level.
# gwm -         The gwm window to be printed.
# c -           The name of the control used to pop up the dialog box.
#
proc gwm_printDialog {w gwm c} {
    global gwm_priv

    if ![winfo exists $w] {

# Create the top level window.
	toplevel $w -class Dialog -bd 10
	wm title $w "Gwm Print"
	wm iconname $w Print
	wm transient $w .


# Initialise the elements of the global array gwm_priv that are used to
# save the state of the dialog box if they don't exist yet.
	if [catch {set gwm_priv($w,pDprint_format)}] {
	    set gwm_priv($w,pDprint_format) "B/W PostScript"
	}
	if [catch {set gwm_priv($w,pDprint_background)}] {
	    set gwm_priv($w,pDprint_background) White
	}
	if [catch {set gwm_priv($w,pDbackground_opt)}] {
	    set gwm_priv($w,pDbackground_opt) colour
	}

# Create and pack three frames one above the other and divide the top frame
# into two side by side.
	pack [frame $w.top] [frame $w.file] [frame $w.bot] -fill x

	pack [frame $w.top.l] [frame $w.top.pad -width 10] [frame $w.top.r] \
	    -side left -fill y

# Create a label and a stack or radio buttons for selecting the print
# format and pack them into the top left frame.
	label $w.top.l.label -text Format: -width 20 -anchor w

        set visual [winfo visual $gwm]
        if { $visual == "truecolor" || $visual == "directcolor" } {
	    tk_optionMenu $w.top.l.fmt gwm_priv($w,pDprint_format) \
	        "B/W PostScript" "Colour PostScript" "Encapsulated PS" \
	        "Encap Colour PS"
        } else {
	    tk_optionMenu $w.top.l.fmt gwm_priv($w,pDprint_format) \
	        "B/W PostScript" "Colour PostScript" "Encapsulated PS" \
	        "Encap Colour PS" "HP inkjet"
        }
	pack $w.top.l.label $w.top.l.fmt -anchor w -pady 2 -fill x

# Create a label, radio buttons for selecting the back ground colour and
# an entry widget for entering a colour name.
	label $w.top.r.lab -text "Background colour"
	radiobutton $w.top.r.bg -text "As window" -relief flat \
	    -variable gwm_priv($w,pDbackground_opt) -value window
	radiobutton $w.top.r.col -text "Colour:" -relief flat \
	    -variable gwm_priv($w,pDbackground_opt) -value colour

	pack $w.top.r.lab $w.top.r.bg $w.top.r.col -anchor w -pady 2
	pack [entry $w.top.r.name -relief sunken -bd 2 -width 14] -anchor w \
	    -pady 5

# Initialise the entry widget.
	$w.top.r.name insert 0 $gwm_priv($w,pDprint_background)

# Pack a label and an entry widget for entering a file name into the
# middle frame.
	pack [label $w.file.lab -text Filename:] -side left -padx 10
	pack [entry $w.file.name -relief sunken -bd 2]  -pady 10 -padx 10 \
	    -fill x

# Create an "OK", "Cancel" and "Help" buttons.
	button $w.bot.ok -text OK -width 6 -command \
            "set gwm_priv(pDbutton) ok; gwm_printDialogEvent $w $gwm \"$c\""
	button $w.bot.can -text Cancel -width 6 -command \
            "set gwm_priv(pDbutton) can; gwm_printDialogEvent $w $gwm \"$c\""
	button $w.bot.help -text Help -width 6 \
	    -command "displayHelp print"

# Pack them into the bottom frame with a default border around the OK
# button.
	frame $w.bot.default -relief sunken -bd 1
	raise $w.bot.ok $w.bot.default
	pack $w.bot.default -side left -expand 1 -padx 3m -pady 2m
	pack $w.bot.ok -in $w.bot.default -padx 1m -pady 1m -ipadx 1m

	pack $w.bot.can -side left -expand 1 -padx 3m -pady 2m -ipadx 1m
	pack $w.bot.help -side left -expand 1 -padx 3m -pady 2m -ipadx 1m

# Bind the return key to the OK button.
	bind $w <Return> "$w.bot.ok flash; set gwm_priv(pDbutton) ok"
	bind $w.file.name <Return> "$w.bot.ok flash; set gwm_priv(pDbutton) ok"

# Withdraw the window, then update all the geometry information
# so we know how big it wants to be, then center the window in
# parent.
	wm withdraw $w
	update idletasks
	set parent [winfo parent $w]
	set x [expr [winfo width $parent]/2 - [winfo reqwidth $w]/2 \
            + [winfo x $parent]]
	set y [expr [winfo height $parent]/2 - [winfo reqheight $w]/2 \
            + [winfo y $parent]]
	wm geom $w +$x+$y
	wm resizable $w 0 0
    }

# De-iconify the window.
    wm deiconify $w
}

proc gwm_printDialogEvent {w gwm c} {

    global gwm_priv
    if ![string compare $gwm_priv(pDbutton) ok] {

    # The OK button as pressed so set the print background option of the
    # gwm widget.
	switch $gwm_priv($w,pDprint_format) {
	    "B/W PostScript"
	    	{$gwm configure -printformat ps}
	    "Colour PostScript"
	    	{$gwm configure -printformat colour_ps}
	    "Encapsulated PS"
	    	{$gwm configure -printformat eps}
            "Encap Colour PS"
	    	{$gwm configure -printformat colour_eps}
	    "HP inkjet"
	    	{$gwm configure -printformat HPInkjet}
	}

	if [string compare $gwm_priv($w,pDbackground_opt) colour] {
	    $gwm configure -printbg [$gwm get colour 0]
	} {
	    $gwm configure -printbg [$w.top.r.name get]
	}

    # Set the variable to be used to signal completion of the print to zero
	$gwm configure -printvariable gwm_printvar
	global gwm_printvar
	set gwm_printvar 0

    # Set a trace on gwm_printvar that re-enables the control that
    # popped up the dialog box.
	trace variable gwm_printvar w "gwm_printComplete \{$c\}"

    # Start the print
	if [catch {$gwm print [$w.file.name get]}] {

        # Report the error.
	    set message "The file [$w.file.name get] could not be opened \
for printing. Please check the file and directory name"
	    tk_dialog .error Error $message error 0 OK
	} {

	# Disable the control that popped us up
	    foreach x $c {$x configure -state disabled}

	# Unmap the dialog box.
    	    wm withdraw $w
	}
    } {

    # Unmap the dialog box.
    	wm withdraw $w
    }
}

proc gwm_printComplete {c name1 name2 op} {

# gwm_printComplete:
#
# This procedure is called when the gwm widget has finished printing.
#
# Arguments:
#
# c -        The widget to be re-enabled
# name1 -    The normal arguments to a variable trace routine
# name2 -    ditto
# op -       ditto

# Enable the widget.
    foreach x $c {$x configure -state normal}

# Delete ourself.
    trace vdelete $name1 $op "gwm_printComplete $c"
}
