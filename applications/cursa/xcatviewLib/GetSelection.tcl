proc GetSelection selectionList {

#+ GetSelection
#
#  Get a new current selection.
#
#  Arguments:
#    selectionList (Given)
#      A list of all the selections currently defined.
#
#  Author:
#   ACD.

#  History:
#   27/5/94   (ACD): Original version.
#   20/10/94  (ACD): First stable version.
#   20/10/96 (ACD): Modified for Tcl/Tk v4 and expect v5.
#   19/11/96 (ACD): Completed modifications for Tcl/Tk v4.
#-

#
# Create the top level window.
    toplevel     .getselection   -class Dialog   -bd 10
    wm title     .getselection   "Choose selection"
    wm iconname  .getselection   Selections
    wm transient .getselection   .

#
#  Define the global variables.
#  ---------------------------

#
#  The number of the chosen selection.

    global selectionNumber

#
#  Flag returned by clicking "OK" or "cancel" buttons.

    global GetSelectionButton

#
#  Bind the return key to be equivalent to hitting return.

    bind .getselection  <Return>  {set GetSelectionButton "ok"}
#
#  Create a list box and scroll bar for the list of columns.

    frame .getselection.sels

    scrollbar .getselection.sels.xscroll -orient horizontal \
      -command {.getselection.sels.list xview} \
      -relief sunken  -bd 2

    scrollbar .getselection.sels.yscroll -orient vertical \
      -command {.getselection.sels.list yview} \
      -relief sunken  -bd 2

    listbox .getselection.sels.list  -relief groove  -bd 2  \
      -xscroll {.getselection.sels.xscroll set} \
      -yscroll {.getselection.sels.yscroll set} \
      -width 60 -height 10  \
      -font *-courier-medium-r-normal--*-100-*-*-*-*-*-*

#
#  Pack the list box into the window.

    pack  .getselection.sels.yscroll  -side left    -fill y
    pack  .getselection.sels.xscroll  -side bottom  -fill x
    pack  .getselection.sels.list     -expand y     -fill both

    pack  .getselection.sels  -side left

#
#  Insert the list of selections into the list box.

    set numSelect [llength $selectionList]

    set counter 0

    while {$counter < $numSelect} {
       set currentSelect [lrange $selectionList $counter $counter]
       .getselection.sels.list insert end [join $currentSelect " "]
       set counter [expr $counter + 1]
    }

#
#  Create a frame to hold the buttons.

    frame .getselection.buttonFrame

#
#  An entry box for the chosen selection number and its associated
#  caption are included at the top of the button window.

    text .getselection.buttonFrame.title  -height 1 -width 10  \
      -relief flat   -highlightthickness 0
    pack .getselection.buttonFrame.title  -side top
    .getselection.buttonFrame.title  insert 1.0 "Selection:"

    entry .getselection.buttonFrame.sel  -relief sunken  -bd 2  -width 5
    pack .getselection.buttonFrame.sel  -side top
    .getselection.buttonFrame.sel  insert 0 1
    bind .getselection.buttonFrame.sel <Return> {set GetSelectionButton "ok"}

#
#  Create the "OK", "cancel", and "help" buttons.

    button .getselection.buttonFrame.ok -text OK -width 6 \
      -command {set GetSelectionButton "ok"}

    button .getselection.buttonFrame.can -text Cancel -width 6 \
      -command {set GetSelectionButton "can"}

    button .getselection.buttonFrame.help -text Help -width 6 \
      -command {HelpText GetSelection_help}

#
#  Pack the buttons into their frame with a default border around
#  the OK button.

    frame .getselection.buttonFrame.default -relief sunken -bd 1
    raise .getselection.buttonFrame.ok .getselection.buttonFrame.default

    pack .getselection.buttonFrame.ok -in .getselection.buttonFrame.default \
      -padx 1m -pady 1m -ipadx 1m

    pack .getselection.buttonFrame.default -side top -expand 1 \
      -padx 3m -pady 2m
    pack .getselection.buttonFrame.can  -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .getselection.buttonFrame.help -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m

#
#  Pack this frame into the window.

    pack  .getselection.buttonFrame  -side left

#
#  Bind a mouse click in the listbox to copy the chosen selection
#  to the entry box.

    bind .getselection.sels.list  <ButtonRelease-1>  {
       set numSel [.getselection.sels.list curselect]
       set numSel [expr $numSel - 1]

       if {$numSel > 0} then {
          .getselection.buttonFrame.sel delete 0 end
          .getselection.buttonFrame.sel insert 0 $numSel
       }
    }

#
#  Withdraw the window, then update all the geometry information
#  so we know how big it wants to be, then centre the window in
#  parent and de-iconify it.

    wm withdraw .getselection
    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .getselection]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .getselection]/2 + \
      [winfo y .]]
    wm geom .getselection +$x+$y
    wm deiconify .getselection

#  Set a grab and claim the focus.
    set oldFocus [focus]
    grab .getselection
    focus .getselection

    tkwait variable GetSelectionButton

    if {$GetSelectionButton == "ok"} {
       set selectionNumber [.getselection.buttonFrame.sel get]
    }

    if {$GetSelectionButton == "can"} {
       set selectionNumber  ""
    }

# Destroy the dialogue box and restore the focus.
    destroy  .getselection
    focus    $oldFocus

}
