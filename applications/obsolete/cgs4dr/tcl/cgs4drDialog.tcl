proc cgs4drDialog {w title text bitmap default args} {
    global tkPriv

    # Create the top-level window and divide it into top and bottom parts.

    catch {destroy $w}
    toplevel $w -class Dialog
    $w config -cursor {pencil blue yellow}
    wm title $w $title
    wm iconname $w Dialog
    wm protocol $w WM_DELETE_WINDOW { }
    wm transient $w [winfo toplevel [winfo parent $w]]
    frame $w.top -relief raised -bd 1
    pack $w.top -side top -fill both
    frame $w.bot -relief raised -bd 1
    pack $w.bot -side bottom -fill both

    # 2. Fill the top part with bitmap and message (use the option
    # database for -wraplength so that it can be overridden by
    # the caller).

    #option add *Dialog.msg.wrapLength 3i widgetDefault
    label $w.msg -justify left -text $text -width 60
    pack $w.msg -in $w.top -side right -expand 1 -fill both -padx 3m -pady 3m
    if {$bitmap != ""} {
	label $w.bitmap -bitmap $bitmap
	pack $w.bitmap -in $w.top -side left -padx 3m -pady 3m
    }

    # 3. Create a row of buttons at the bottom of the dialog.

    set i 0
    foreach but $args {
	button $w.button$i -text $but -command "set tkPriv(button) $i"
	if {$i == $default} {
	    frame $w.default -relief sunken -bd 1
	    raise $w.button$i $w.default
	    pack $w.default -in $w.bot -side left -expand 1 -padx 3m -pady 2m
	    pack $w.button$i -in $w.default -padx 2m -pady 2m
	    bind $w <Return> "$w.button$i flash; set tkPriv(button) $i"
	} else {
	    pack $w.button$i -in $w.bot -side left -expand 1 \
		    -padx 3m -pady 2m
	}
	incr i
    }

    # 4. Withdraw the window, then update all the geometry information
    # so we know how big it wants to be, then center the window in the
    # display and de-iconify it.

    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
	    - [winfo vrootx [winfo parent $w]]]
    set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
	    - [winfo vrooty [winfo parent $w]]]
    wm geom $w +$x+$y
    wm deiconify $w

    # 5. Set a grab and claim the focus too.

    set oldFocus [focus]
    set oldGrab [grab current $w]
    if {$oldGrab != ""} {
	set grabStatus [grab status $oldGrab]
    }
    grab $w
    if {$default >= 0} {
	focus $w.button$default
    } else {
	focus $w
    }

    # 6. Wait for the user to respond, then restore the focus and
    # return the index of the selected button.  Restore the focus
    # before deleting the window, since otherwise the window manager
    # may take the focus away so we can't redirect it.  Finally,
    # restore any grab that was in effect.

    tkwait variable tkPriv(button)
    catch {focus $oldFocus}
    destroy $w
    if {$oldGrab != ""} {
	if {$grabStatus == "global"} {
	    grab -global $oldGrab
	} else {
	    grab $oldGrab
	}
    }
    return $tkPriv(button)
}
