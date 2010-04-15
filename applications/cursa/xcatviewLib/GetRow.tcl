proc GetRow rows {

#+ GetRow
#
#  Get a row number from the user.
#
#  Given
#    rows  The number of rows in the catalogue.
#
#
#  Method:
#   Declare the global variables.
#   Create the top level window.
#   Bind the return key to global variable button for "ok".
#   Create the scale widget.
#   Pack the scale widget into the window.
#   Bind a mouse click in the scale to copy the value to the entry
#   widget.
#   Create a frame to hold the row number and label.
#   Create the row number and label widgets.
#   Pack the row number and label widgets into their frame.
#   Pack this frame into the window.
#   Create a frame to hold the row of buttons.
#   Create the buttons.
#   Pack the buttons into their frame.
#   Pack this frame into the window.
#   Withdraw the window.
#   Update all the geometry information (to determine how big it
#   wants to be).
#   Centre the window in # parent and de-iconify it.
#   Set a grab and claim the focus.
#   Destroy the dialogue box and restore the focus.
#
#  Author:
#   ACD: A C Davenhall (Leicester).

#  History:
#   18/10/94 (ACD): Original version.
#   19/11/96 (ACD): Completed modifications for Tcl/Tk v4.
#-

#
#  Declare the global variables.

#
#  The state returned by clicking on the "OK" or "Cancel" buttons.

    global getRowButton

#
#  The selected new current row number.

    global currentRow

#
#  Create the top level window.

    toplevel     .getrow   -class Dialog   -bd 10
    wm title     .getrow   "Select Row Number"
    wm iconname  .getrow   Row
    wm transient .getrow   .

#
#  Bind the return key to set global variable button to "ok".  That is,
#  hitting return will have the same effect as clicking on the "ok"
#  button.

    bind  .getrow  <Return> {set getRowButton "ok" }

#
#  Create the scale.

    scale .getrow.scale  -from 1  -to $rows  -orient horizontal \
      -length 60m  -highlightthickness 0

#
#  Pack the scale into the window.

    pack .getrow.scale        -side top

#
#  Bind a mouse click in the scale to copy the value to the entry
#  widget.

    bind .getrow.scale <ButtonRelease-1> {
      .getrow.input.value delete 0 end
      .getrow.input.value insert 0 [.getrow.scale get]
    }

#
#  Create a frame to hold the row number and label.

    frame  .getrow.input

#
#  Create the label and value box for the row number.

    label .getrow.input.label  -anchor w -text "Row number:"

    entry .getrow.input.value  -relief sunken  -bd 2  -width 10
    .getrow.input.value  insert 0 1
    bind  .getrow.input.value  <Return> {set getRowButton "ok" }

#
#  Pack these widgets into their frame and pack the frame into the
#  window.

    pack .getrow.input.label  -side left
    pack .getrow.input.value  -side left

    pack .getrow.input        -side top

#
#  Create a frame to hold the row of buttons.

    frame .getrow.buttonrow

#
#  Create each of the buttons.
#
#  OK.

    button .getrow.buttonrow.ok \
      -text OK \
      -width 6 \
      -command {set getRowButton "ok"}

#
#  Cancel.

    button .getrow.buttonrow.can \
      -text Cancel \
      -width 6 \
      -command {set getRowButton "can"}

#
#  Help.

    button .getrow.buttonrow.help \
      -text Help \
      -width 6 \
      -command {HelpText GetRow_help}

#
#  Pack the buttons into their enclosing frame with a default border around
#  the OK button.

    frame .getrow.buttonrow.default -relief sunken -bd 1
    raise .getrow.buttonrow.ok .getrow.buttonrow.default

    pack  .getrow.buttonrow.ok -in .getrow.buttonrow.default \
      -padx 1m -pady 1m -ipadx 1m

    pack .getrow.buttonrow.default  \
      -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

    pack .getrow.buttonrow.can   \
      -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

    pack .getrow.buttonrow.help  \
      -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

#
#  Pack the frame of buttons into the window.

    pack .getrow.buttonrow  -side top

#
#  Withdraw the window, then update all the geometry information
#  to determine how big the window wants to be, then centre the window
#  in parent and de-iconify it.

    wm withdraw .getrow
    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .getrow]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .getrow]/2 + \
      [winfo y .]]
    wm geom .getrow +$x+$y
    wm deiconify .getrow

#
#  Set a grab and claim the focus.

    set oldFocus [focus]
    grab  .getrow
    focus .getrow

    tkwait variable getRowButton

    if {$getRowButton == "ok"} then {
       set currentRow [.getrow.input.value get]
    }

    if {$getRowButton == "can"} {
       set currentRow ""
    }

#
#  Destroy the dialogue box and restore the focus.

    destroy  .getrow
    focus    $oldFocus

}
