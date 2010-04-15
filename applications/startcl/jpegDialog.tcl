# jpegDialog.tcl
#
# This file defines the procedure gwm_jpegDialog, which creates a modal
# dialog box for dumping the contents of a gwm widget to a JPEG image.

# gwm_jpegDialog:
#
# This procedure creates a modal dialog box with controls for printing a
# gwm widget to a JPEG format file. The control that activated the dialog
# is disabled until the printing operation is complete to prevent attempts
# to start a second print before the previous one has finished.
#
# Arguments:
#
# w -           Window to use for dialog top-level.
# gwm -         The gwm window to be printed.
# c -           The name of the control used to pop up the dialog box.
#
proc gwm_jpegDialog {w gwm c} {
    global gwm_priv

    if ![winfo exists $w] {

# Create the top level window.
	toplevel $w -class Dialog -bd 10
	wm title $w "Gwm JPEG"
	wm iconname $w JPEG
	wm transient $w .

# Create and pack three frames one above the other.
	pack [frame $w.top] [frame $w.mid] [frame $w.bot] -fill x

# Create a label and a slider for setting the quality/compression.
# format and pack them into the top left frame.
	label $w.top.label -text Quality:
	set gwm_priv($w,jDquality) [$gwm cget -jpegquality]
	scale $w.top.quality -variable gwm_priv($w,jDquality) -from 50 \
	    -to 95 -showvalue 1 -orient horizontal
	pack $w.top.label $w.top.quality -pady 2 -side left

# Create a check button for selecting whether or not to make a "progressive"
# JPEG.
	set gwm_priv($w,jDprogressive) 0
	checkbutton $w.top.progressive -text "Progressive" \
	    -variable gwm_priv($w,jDprogressive)
	pack $w.top.progressive -pady 2

# Pack a label and an entry widget for entering a file name into the
# middle frame.
	pack [label $w.mid.lab -text Filename:] -side left -padx 10
	pack [entry $w.mid.name -relief sunken -bd 2]  -pady 10 -padx 10 -fill x

# Create an "OK", "Cancel" and "Help" buttons.
	button $w.bot.ok -text OK -width 6 -command \
            "set gwm_priv(jDbutton) ok; gwm_jpegDialogEvent $w $gwm \"$c\""
	button $w.bot.can -text Cancel -width 6 -command \
            "set gwm_priv(jDbutton) can; gwm_jpegDialogEvent $w $gwm \"$c\""
	button $w.bot.help -text Help -width 6 -command {displayHelp capture}

# Pack them into the bottom frame with a default border around the OK
# button.
	frame $w.bot.default -relief sunken -bd 1
	raise $w.bot.ok $w.bot.default
	pack $w.bot.default -side left -expand 1 -padx 3m -pady 2m
	pack $w.bot.ok -in $w.bot.default -padx 1m -pady 1m -ipadx 1m

	pack $w.bot.can -side left -expand 1 -padx 3m -pady 2m -ipadx 1m
	pack $w.bot.help -side left -expand 1 -padx 3m -pady 2m -ipadx 1m

# Bind the return key to the OK button.
	bind $w <Return> "$w.bot.ok flash; set gwm_priv(jDbutton) ok"
	bind $w.mid.name <Return> "$w.bot.ok flash; set gwm_priv(jDbutton) ok"

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

# De-iconfiy the window.
    wm deiconify $w
}

proc gwm_jpegDialogEvent {w gwm c} {

    global gwm_priv
    if ![string compare $gwm_priv(jDbutton) ok] {

    # The OK button as pressed so set the print background option of the
    # gwm widget.
        $gwm configure -printformat JPEG

    # Set the progressive and quality options.
        $gwm configure -jpegquality $gwm_priv($w,jDquality) \
        -jpegprogressive $gwm_priv($w,jDprogressive) \
	-printbg [$gwm get colour 0]

    # Set the variable to be used to signal completion of the print to zero
	$gwm configure -printvariable gwm_printvar
	global gwm_printvar
	set gwm_printvar 0

    # Set a trace on gwm_printvar that re-enables the control that
    # popped up the dialog box.
	trace variable gwm_printvar w "gwm_printComplete \{$c\}"

    # Start the print
        if [catch {$gwm print [$w.mid.name get]}] {

	# Report the error.
	    set message "The file [$w.mid.name get] could not be opened \
for writing. Please check the file and directory name"
	    tk_dialog .error Error $message error 0 OK
	} {

	# Disable the controls that popped us up
	    foreach x $c {$x configure -state disabled}
	# Unmap the dialog box.
	    wm withdraw $w
	}
    } {

    # Unmap the dialog box.
	wm withdraw $w
    }
}
