proc dialogShow {w default {proc {expr 0 *}}} {
#+
# Displays a dialog box created with dialogStart and waits for a button
# to be pressed. The return value is the index of the button.
#-
    global tk_priv

# Withdraw the window, then update all the geometry information
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

# Set a grab and claim the focus.
    set oldFocus [focus]
    grab $w
    tkwait visibility $w
    focus $default

# Wait for the user to respond, then restore the focus and
# return the index of the selected button.

    set done 1
    while {$done != 0} {
        tkwait variable tk_priv(button)
	set done [eval "$proc $tk_priv(button)"]
	grab $w
    }
    focus $oldFocus
    return $tk_priv(button)
}
