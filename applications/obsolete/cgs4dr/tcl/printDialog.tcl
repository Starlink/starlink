# printDialog.tcl
#
# This file defines the procedure gwm_printDialog, which creates a modal
# dialog box for printing the contents of a gwm widget.

# gwm_printDialog:
#
# This procedure creates a modal dialog box with controls for setting the
# colours of a gwm widget. The control that activated the dialog is disabled
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
    global env
    global gwm_priv
    global cgs4drXopts

# Create the top level window.
    toplevel $w -class Dialog -bd 10
    wm title $w "Gwm Print"
    wm iconname $w Print
    wm transient $w .
    cgs4drCursor pirate orange black
    $w config -cursor {arrow green black}

# Initialise the elements of the global array gwm_priv that are used to
# save the state of the dialog box if they don't exist yet.
    if [catch {set gwm_priv($w,print_format)}] {
	set gwm_priv($w,print_format) ps
    }
    if [catch {set gwm_priv($w,print_background)}] {
	set gwm_priv($w,print_background) White
    }
    if [catch {set gwm_priv($w,background_opt)}] {
	set gwm_priv($w,background_opt) colour
    }

# Create and pack three frames one above the other and divide the top frame
# into two side by side.
    pack [frame $w.top] [frame $w.file] [frame $w.bot] -fill both -pady 2m
    pack [frame $w.top.l] -side left -fill both -expand y
    pack [frame $w.top.r] -side right -fill both  -expand y

# Create a label and a stack or radio buttons for selecting the print
# format and pack them into the top left frame.
    label $w.top.l.label -fg blue -text "Output Format:"
    radiobutton $w.top.l.ps -text "B/W PostScript" -relief flat \
	-variable gwm_priv($w,print_format) -value ps  -anchor w -width 20
    radiobutton $w.top.l.colour_ps -text "Colour PostScript" -relief flat \
	-variable gwm_priv($w,print_format) -value colour_ps -anchor w -width 20
    radiobutton $w.top.l.eps -text "Encapsulated PS" -relief flat \
	-variable gwm_priv($w,print_format) -value eps -anchor w -width 20
    radiobutton $w.top.l.colour_eps -text "Encap Colour PS" -relief flat \
	-variable gwm_priv($w,print_format) -value colour_eps -anchor w -width 20
    radiobutton $w.top.l.inkjet -text "HP Inkjet" -relief flat \
	-variable gwm_priv($w,print_format) -value HPinkjet -anchor w -width 20
    pack $w.top.l.label $w.top.l.ps $w.top.l.colour_ps $w.top.l.eps \
	$w.top.l.colour_eps $w.top.l.inkjet -side top

# Create a label, radio buttons for selecting the back ground colour and
# an entry widget for entering a colour name.
    label $w.top.r.blab -fg blue -text "Background:"
    radiobutton $w.top.r.bg -text "As window" -relief flat \
	-variable gwm_priv($w,background_opt) -value window -anchor w
      radiobutton $w.top.r.bcol -text "Colour:" -relief flat \
	-variable gwm_priv($w,background_opt) -value colour -anchor w
      entry $w.top.r.bname -relief sunken -bd 2 -width 15
    pack $w.top.r.blab $w.top.r.bg $w.top.r.bcol $w.top.r.bname -side top

# Initialise the entry widget.
    $w.top.r.bname insert 0 $gwm_priv($w,print_background)

# Pack a label and an entry widget for entering a file name into the middle frame.
    pack [frame $w.file.t] [frame $w.file.m] [frame $w.file.b]
    label $w.file.t.wlab -fg blue -text "Output Options:"
    pack $w.file.t.wlab

    radiobutton $w.file.m.pbut -text "Printer" -width 15 -relief flat -variable cgs4drXopts(PrintOption) -value printer
    label $w.file.m.plab -text "Command:" -width 15
    entry $w.file.m.pname -width 30 -relief sunken -bd 2 -textvariable cgs4drXopts(PrintCommand)
    pack $w.file.m.pbut $w.file.m.plab $w.file.m.pname -side left -anchor w

    radiobutton $w.file.b.fbut -text "File" -width 15 -relief flat -variable cgs4drXopts(PrintOption) -value file
    label $w.file.b.flab -text "Filename:" -width 15
    entry $w.file.b.fname -width 30 -relief sunken -bd 2 -textvariable cgs4drXopts(PrintFile)
    pack $w.file.b.fbut $w.file.b.flab $w.file.b.fname -side left -anchor w

# Create an "OK" and a "Cancel" button.
    button $w.bot.ok -text OK -command {set gwm_priv(button) "ok"}
    button $w.bot.can -text Cancel -command {set gwm_priv(button) "can"}

# Pack them into the bottom frame with a default border around the OK button.
    frame $w.bot.default -relief sunken -bd 1
    raise $w.bot.ok $w.bot.default
    pack $w.bot.default -side left -expand 1 -padx 3m -pady 2m
    pack $w.bot.ok -in $w.bot.default -padx 1m -pady 1m -ipadx 1m

    pack $w.bot.can -side left -expand 1 -padx 3m -pady 2m -ipadx 1m

# Bind the return key to the OK button.
    bind $w <Return> "$w.bot.ok flash; set gwm_priv(button) ok"
    bind $w.file.b.fname <Return> "$w.bot.ok flash; set gwm_priv(button) ok"

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
    wm deiconify $w

#  Set a grab and claim the focus.
    set oldFocus [focus]
    grab $w
    focus $w

# Wait for the user to respond.
    for {} {1} {} {
	tkwait variable gwm_priv(button)
	if ![string compare $gwm_priv(button) ok] {

	# The OK button was pressed so set the print background option of the
	# gwm widget.
	  $gwm configure -printformat $gwm_priv($w,print_format)
	  if [string compare $gwm_priv($w,background_opt) colour] {
            $gwm configure -printbg [$gwm get colour 0]
	  } {
            $gwm configure -printbg [$w.top.r.bname get]
	  }

	# Set the variable to be used to signal completion of the print to zero
	    global gwm_printvar
	    set gwm_printvar 0
	    $gwm configure -printvariable gwm_printvar

	# Set a trace on gwm_printvar that re-enables the control that
	# popped up the dialog box.
            set cgs4drXopts(PrintFile) [string trim [$w.file.b.fname get]]
            if {$cgs4drXopts(PrintFile) == ""} {set cgs4drXopts(PrintFile) "$env(HOME)/gwm.ps"}
            if {[file exists $cgs4drXopts(PrintFile)] == 1} {exec /usr/bin/rm -f $cgs4drXopts(PrintFile)}
            set cgs4drXopts(PrintCommand) [string trim [$w.file.m.pname get]]
            if {$cgs4drXopts(PrintCommand) == ""} {set cgs4drXopts(PrintCommand) "/usr/bin/lp -c"}
	    trace variable gwm_printvar w "gwm_printComplete $c"

	# Start the print
	    if [catch {$gwm print $cgs4drXopts(PrintFile)}] {

	    # Report the error and re-grab the pointer
		set message "The file $cgs4drXopts(PrintFile) could not be opened for printing. \
                   Please check the file and directory name."
		tk_dialog .error Error $message error 0 Dismiss
		grab $w
	    } {

	    # Disable the control that popped us up
		$c configure -state disabled
                cgs4drCursor watch red white
	    # Destroy the dialog box and restore the focus.
		destroy $w
		focus $oldFocus
	    }
	} {

	# Destroy the dialog box and restore the focus.
            cgs4drCursor arrow green black
	    destroy $w
	    focus $oldFocus
	}
    }
}

proc gwm_printComplete {c name elem op} {

# gwm_printComplete:
#
# This procedure is called when the gwm widget has finished printing.
#
# Arguments:
#
# c -       The widget to be re-enabled
# name -    The normal arguments to a variable trace routine
# elem -    ditto
# op -      ditto
  global $name

# Delete ourself.
  trace vdelete $name $op "gwm_printComplete $c"

# Send the output to printer in background
  global cgs4drXopts
  if {[string tolower $cgs4drXopts(PrintOption)] == "printer"} {
    catch {eval exec $cgs4drXopts(PrintCommand) $cgs4drXopts(PrintFile)} err
    tk_dialog .info "Print Status" "$err" info 0 Dismiss
  }

# Enable the widget.
  $c configure -state normal
  cgs4drCursor arrow green black
}
