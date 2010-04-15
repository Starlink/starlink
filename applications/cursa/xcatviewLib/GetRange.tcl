proc GetRange rangeColumns {

#+ GetRange
#
#  Procedure to get the name of the column and the minimum and
#  maximum values for a range selection.
#
#  Given:
#    rangeColumns
#      A list of columns on which range selections can be performed.
#
#  Method:
#   Create the top level window.
#   Declare the global variables.
#   Bind the return key to global variable button for "ok".
#   Create the frame to hold the list box.
#   Create the list box and its scroll bars.
#   Pack them into their frame.
#   Pack this frame into the window.
#   Insert the list of columns into the listbox.
#   Create the frame to hold the entry boxes.
#   Create the entry boxes and their associated text.
#   Pack the entry boxes and labels into their frame.
#   Pack this frame into the window.
#   Bind an action to a mose-click in the listbox.
#   Create the frame for the buttons.
#   Create the buttons.
#   Pack the buttons into their frame.
#   Pack this frame into the window.
#   Bind actions to a mouse-click for the listbox.
#   Insert the lists of columns into the listbox.
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
#   19/10/94 (ACD): Original version.
#   20/10/94 (ACD): First stable version.
#   20/10/96 (ACD): Modified for Tcl/Tk v4 and expect v5.
#   19/11/96 (ACD): Completed modifications for Tcl/Tk v4.
#-

#
#   Create the top level window.

     toplevel     .getrange   -class Dialog   -bd 10
     wm title     .getrange   "Range Selection"
     wm iconname  .getrange   Range
     wm transient .getrange   .

#
#   Declare the global variables.
#   ----------------------------

#
#   The state returned by clicking on the "OK" or "Cancel" buttons.

     global getRangeButton

#
#   The minimum value for the range selection.

     global rangeMin

#
#   The maximum value for the range selection.

     global rangeMax

#
#   The column on which the range selection is to be performed.

     global rangeColumn

#
#   Bind the return key to set global variable button to "ok".  That is,
#   hitting return will have the same effect as clicking on the "ok"
#   button.

     bind  .getrange  <Return> {set getRangeButton "ok" }

#
#   Create a frame to hold the listbox, create the listbox and its
#   associated scroll bars.

     frame  .getrange.cols

     scrollbar .getrange.cols.xscroll  -orient horizontal \
       -command {.getrange.cols.list  xview} \
       -relief sunken  -bd 2

     scrollbar .getrange.cols.yscroll  -orient vertical \
       -command {.getrange.cols.list  yview} \
       -relief sunken  -bd 2

     listbox .getrange.cols.list  -relief groove  -bd 2  \
       -width 16 -height 7  \
       -xscroll {.getrange.cols.xscroll  set}  \
       -yscroll {.getrange.cols.yscroll  set}

#
#   Pack the listbox and scroll bars into their frame and pack the
#   frame into the window.

     pack .getrange.cols.yscroll  -side left    -fill y
     pack .getrange.cols.xscroll  -side bottom  -fill x
     pack .getrange.cols.list     -expand yes   -fill both

     pack .getrange.cols  -side left  -padx 3m

#
#   Insert the list of columns on which range selections may be
#   performed into the list box.

     set numColumns [llength $rangeColumns]

     set counter 0

     while {$counter < $numColumns} {
        set currentColumn [lrange $rangeColumns $counter $counter]
        .getrange.cols.list insert end $currentColumn
        set counter [expr $counter + 1]
     }

#
#   Create a frame to hold the entry boxes and their labels, then
#   create these boxes and labels and pack them into their frame.

     frame .getrange.entry

     text  .getrange.entry.coltitle  -height 1 -width 20 -relief flat  \
       -highlightthickness 0
     pack  .getrange.entry.coltitle  -side top
     .getrange.entry.coltitle  insert 1.0 "Column:"

     entry .getrange.entry.column  -relief sunken -bd 2 -width 20
     pack  .getrange.entry.column  -side top
     .getrange.entry.column  configure  -state disabled

     text  .getrange.entry.mintitle  -height 1 -width 20 -relief flat  \
       -highlightthickness 0
     pack  .getrange.entry.mintitle  -side top
     .getrange.entry.mintitle  insert 1.0 "Minimum value:"

     entry .getrange.entry.min  -relief sunken -bd 2 -width 20
     pack  .getrange.entry.min  -side top
     bind  .getrange.entry.min  <Return> {set getRangeButton "ok" }

     text  .getrange.entry.maxtitle  -height 1 -width 20 -relief flat  \
       -highlightthickness 0
     pack  .getrange.entry.maxtitle  -side top
     .getrange.entry.maxtitle  insert 1.0 "Maximum value:"

     entry .getrange.entry.max  -relief sunken -bd 2 -width 20
     pack  .getrange.entry.max  -side top
     bind  .getrange.entry.max  <Return> {set getRangeButton "ok" }

#
#   Pack the frame for the entry boxes and labels into the window.

     pack .getrange.entry  -side left  -padx 3m

#
#   Bind an action to a mouse click in the list box.  The action
#   required is to copy the selected column to the catalogue entry
#   box.  Note that entry to this box from the keyboard is disabled.

     bind .getrange.cols.list  <ButtonRelease-1>  {
        set numCol [.getrange.cols.list curselect]
        if {$numCol > -1} then {
           set chosenCol [.getrange.cols.list get $numCol]

           .getrange.entry.column  configure -state normal
           .getrange.entry.column  delete 0 end
           .getrange.entry.column  insert end $chosenCol
           .getrange.entry.column  configure -state disabled
        }
     }

#
#   Create a frame to hold the row of buttons.

     frame .getrange.buttonrow

#
#   Create each of the buttons.
#
#   OK.

     button .getrange.buttonrow.ok \
       -text OK \
       -width 6 \
       -command {set getRangeButton "ok"}

#
#   Cancel.

     button .getrange.buttonrow.can \
       -text Cancel \
       -width 6 \
       -command {set getRangeButton "can"}

#
#   Help.

     button .getrange.buttonrow.help \
       -text Help \
       -width 6 \
       -command {HelpText GetRange_help}

#
#   Pack the buttons into their enclosing frame with a default border around
#   the OK button.

     frame .getrange.buttonrow.default -relief sunken -bd 1
     raise .getrange.buttonrow.ok .getrange.buttonrow.default

     pack  .getrange.buttonrow.ok -in .getrange.buttonrow.default \
       -side top  -padx 1m -pady 1m -ipadx 1m

     pack .getrange.buttonrow.default  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getrange.buttonrow.can   \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getrange.buttonrow.help  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

#
#   Pack the frame of buttons into the window.

     pack .getrange.buttonrow  -side left

#
#   Bind actions to a mouse-click for the columns listbox.
#
#   The action to be taken on a mouse-click in the catalogues
#   listbox are:
#
#      Get the index of the new directory.
#      If the index is greater than 0 then
#        Get the name of the directory.
#        Set to the new current directory.
#        Get the details of the catalogues and subdirectories of this
#        new current directory.
#      end if

#      bind .getcatalogue.spec.dir.list  <ButtonRelease-1>  {
#        set numDir [.getcatalogue.spec.dir.list curselect]
#        if {$numDir > -1} then {
#           set newDir [.getcatalogue.spec.dir.list get $numDir]
#           if {[catch {cd $newDir}]} then {
#              Error "Failure executing Unix cd (change directory) command."
#              return
#           }
#           GetDirCat
#        }
#     }


#
#   Withdraw the window, then update all the geometry information
#   to determine how big the window wants to be, then centre the window
#   in parent and de-iconify it.

     wm withdraw .getrange
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .getrange]/2 + \
       [winfo x .]]
     set y [expr [winfo height .]/2 - [winfo reqheight .getrange]/2 + \
       [winfo y .]]
     wm geom .getrange +$x+$y
     wm deiconify .getrange

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab  .getrange
     focus .getrange

     tkwait variable getRangeButton

     if {$getRangeButton == "ok"} then {
        set rangeColumn [.getrange.entry.column get]

#
#      Note that the ranges are preceded by a colon(':') in order to
#      avoid confusing exp_send (in Action.tcl), which otherwise fails
#      to send negative numbers (the minus sign is interpretted as a
#      tcl flag!).

        set    rangeMin    :
        append rangeMin    [.getrange.entry.min    get]

        set    rangeMax    :
        append rangeMax    [.getrange.entry.max    get]
     }

     if {$getRangeButton == "can"} then {
        set rangeColumn  ""
        set rangeMin     ""
        set rangeMax     ""
     }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .getrange
     focus    $oldFocus

}
