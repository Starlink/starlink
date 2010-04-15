proc GetFormat { } {

#+ GetFormat
#
#  Procedure to get the new display format and units for a column.
#
#  Given:
#    None.
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
#   20/3/95  (ACD): Original version.
#   12/4/95  (ACD): First stable version.
#   20/10/96 (ACD): Modified for Tcl/Tk v4 and expect v5.
#   19/11/96 (ACD): Completed modifications for Tcl/Tk v4.
#   19/12/96 (ACD): Fixed bug in getting the details of an expression
#     which was introduced by Tcl/Tk v4.
#-

#
#   Create the top level window.

     toplevel     .getformat   -class Dialog   -bd 10
     wm title     .getformat   "Column Display Format"
     wm iconname  .getformat   Format
     wm transient .getformat   .

#
#   Declare the global variables.
#   ----------------------------

#
#   The state returned by clicking on the "OK" or "Cancel" buttons.

     global getFormatButton

#
#   The details of the column returned by the A-task.

     global catalogueList

#
#   The current list of columns for display.

     global chosenColumns

#
#   The name, units and display format for the chosen column.

     global columnName
     global columnUnits
     global columnFormat


#
#   Bind the return key to set global variable button to "ok".  That is,
#   hitting return will have the same effect as clicking on the "ok"
#   button.

#     bind  .getformat  <Return> {set getFormatButton "ok" }


#
#   Create a frame to hold the listbox, create the listbox and its
#   associated scroll bars.

     frame  .getformat.cols

     scrollbar .getformat.cols.xscroll  -orient horizontal \
       -command {.getformat.cols.list  xview} \
       -relief sunken  -bd 2

     scrollbar .getformat.cols.yscroll  -orient vertical \
       -command {.getformat.cols.list  yview} \
       -relief sunken  -bd 2

     listbox .getformat.cols.list  -relief groove  -bd 2  \
       -width 16 -height 7  \
       -xscroll {.getformat.cols.xscroll  set}  \
       -yscroll {.getformat.cols.yscroll  set}

#
#   Pack the listbox and scroll bars into their frame and pack the
#   frame into the window.

     pack .getformat.cols.yscroll  -side left    -fill y
     pack .getformat.cols.xscroll  -side bottom  -fill x
     pack .getformat.cols.list     -expand yes   -fill both

     pack .getformat.cols  -side left  -padx 3m  -fill y

#
#   Insert the list of columns on which range selections may be
#   performed into the list box.  The default is all the columns.

     set numColumns [llength $catalogueList]

     set counter 0

     while {$counter < $numColumns} {
        set currentColumn [lrange $catalogueList $counter $counter]
        .getformat.cols.list insert end $currentColumn
        set counter [expr $counter + 1]
     }

#
#   Create a frame to hold the entry boxes and their labels, then
#   create these boxes and labels and pack them into their frame.

     frame .getformat.entry

#
#   ... the column name,

     text  .getformat.entry.coltitle  -height 1 -width 20 -relief flat  \
       -highlightthickness 0
     pack  .getformat.entry.coltitle  -side top  -anchor w
     .getformat.entry.coltitle  insert 1.0 "Column:"

     entry .getformat.entry.column  -width 20  -relief flat  \
       -highlightthickness 0
     pack  .getformat.entry.column  -side top  -anchor w
     .getformat.entry.column  configure  -state disabled

#
#   ... the data type,

     text  .getformat.entry.typtitle  -height 1 -width 20 -relief flat  \
       -highlightthickness 0
     pack  .getformat.entry.typtitle  -side top  -anchor w
     .getformat.entry.typtitle  insert 1.0 "Data type:"

     entry .getformat.entry.type  -width 20  -relief flat  \
       -highlightthickness 0
     pack  .getformat.entry.type  -side top  -anchor w
     .getformat.entry.type  configure  -state disabled

#
#   ... the display format.

     text  .getformat.entry.fmtitle  -height 1 -width 20 -relief flat  \
       -highlightthickness 0
     pack  .getformat.entry.fmtitle  -side top  -anchor w
     .getformat.entry.fmtitle  insert 1.0 "Display format:"

     entry .getformat.entry.format  -relief sunken  -bd 2  -width 20
     pack  .getformat.entry.format  -side top  -anchor w
     bind  .getformat.entry.format  <Return> {set getFormatButton "ok" }
     bindtags .getformat.entry.format {.getformat.entry.format Entry}

#
#   ... the units,

     text  .getformat.entry.unititle  -height 1 -width 20 -relief flat  \
       -highlightthickness 0
     pack  .getformat.entry.unititle  -side top  -anchor w
     .getformat.entry.unititle  insert 1.0 "Units:"

     entry .getformat.entry.units  -relief sunken  -bd 2  -width 35
     pack  .getformat.entry.units  -side top  -anchor w

#
#   Finally create the button and entry box to control the set of
#   columns which are to be displayed, and pack them into the frame.

     frame .getformat.entry.cols

     menubutton .getformat.entry.cols.but  \
       -relief sunken  -bd 2 -width 10  \
       -text "Columns"  \
       -menu .getformat.entry.cols.but.menu
     pack .getformat.entry.cols.but  -side left

     entry .getformat.entry.cols.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getformat.entry.cols.value  -side left  -padx 2m
     .getformat.entry.cols.value  insert 0 "All"
     .getformat.entry.cols.value  configure  -state disabled

     pack  .getformat.entry.cols -side top -pady 10m

#
#   Pack the frame for the entry boxes and labels into the window.

     pack .getformat.entry  -side left  -padx 3m

#
#   Define the options for the 'columns' menu.

     menu .getformat.entry.cols.but.menu

     .getformat.entry.cols.but.menu  add command \
        -label All  \
        -command {
          .getformat.entry.cols.value  configure  -state normal
          .getformat.entry.cols.value  delete 0 end
          .getformat.entry.cols.value  insert 0 "All"
          .getformat.entry.cols.value  configure  -state disabled

          .getformat.cols.list delete 0 end

          set numColumns [llength $catalogueList]

          set counter 0

          while {$counter < $numColumns} {
             set currentColumn [lrange $catalogueList $counter $counter]
             .getformat.cols.list insert end $currentColumn
             set counter [expr $counter + 1]
          }

         }

     .getformat.entry.cols.but.menu  add command \
        -label "Current list"  \
        -command {
          .getformat.entry.cols.value  configure  -state normal
          .getformat.entry.cols.value  delete 0 end
          .getformat.entry.cols.value  insert 0 "Current list"
          .getformat.entry.cols.value  configure  -state disabled

          .getformat.cols.list delete 0 end

          set chosenList [split $chosenColumns {;}]

          set numChosen [llength $chosenList]

          set counter 0

          while {$counter < $numChosen} {
             set currentChosen [lrange $chosenList $counter $counter]
             .getformat.cols.list insert end $currentChosen
             set counter [expr $counter + 1]
          }
         }

#
#   Bind an action to a mouse click in the list box.  The action
#   required is to copy the selected column to the catalogue entry
#   box and then get the details for the column.

     bind .getformat.cols.list  <ButtonRelease-1>  {
        set numCol [.getformat.cols.list curselect]
        if {$numCol > -1} then {
           set chosenItem [.getformat.cols.list get $numCol]

           set bracePos [string first "\{" $chosenItem]

           if {$bracePos > -1} then {
              set bracePos [expr $bracePos - 1]

              set chosenCol [string range $chosenItem 0 $bracePos]
           } else {
              set chosenCol $chosenItem
           }

           .getformat.entry.column  configure -state normal
           .getformat.entry.column  delete 0 end
           .getformat.entry.column  insert end $chosenCol
           .getformat.entry.column  configure -state disabled

           global columnName
           set columnName $chosenCol

           Action SHOWFMT

           global catalogueList

           set columnType   [lrange $catalogueList 0 0]
           set columnUnits  [lrange $catalogueList 1 1]
           set columnFormat [lrange $catalogueList 2 2]

#           if {$columnUnits == "{}"} then {
#              set columnUnits " "
#           }

           .getformat.entry.type  configure -state normal
           .getformat.entry.type  delete 0 end
           .getformat.entry.type  insert end $columnType
           .getformat.entry.type  configure -state disabled

           .getformat.entry.units  configure -state normal
           .getformat.entry.units  delete 0 end
           .getformat.entry.units  insert end $columnUnits

           .getformat.entry.format  configure -state normal
           .getformat.entry.format  delete 0 end
           .getformat.entry.format  insert end $columnFormat

           .getformat.entry.units  configure -state normal

           focus -force .getformat

        }
     }

     bindtags .getformat.cols.list {.getformat.cols.list Listbox}

#
#   Create a frame to hold the row of buttons.

     frame .getformat.buttonrow

#
#   Create each of the buttons.
#
#   OK.

     button .getformat.buttonrow.ok \
       -text OK \
       -width 6 \
       -command {set getFormatButton "ok"}

#
#   Cancel.

     button .getformat.buttonrow.can \
       -text Cancel \
       -width 6 \
       -command {set getFormatButton "can"}

#
#   Help.

     button .getformat.buttonrow.help \
       -text Help \
       -width 6 \
       -command {HelpText GetFormat_help}

#
#   Pack the buttons into their enclosing frame with a default border around
#   the OK button.

     frame .getformat.buttonrow.default -relief sunken -bd 1
     raise .getformat.buttonrow.ok .getformat.buttonrow.default

     pack  .getformat.buttonrow.ok -in .getformat.buttonrow.default \
       -side top  -padx 1m -pady 1m -ipadx 1m

     pack .getformat.buttonrow.default  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getformat.buttonrow.can   \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getformat.buttonrow.help  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

#
#   Pack the frame of buttons into the window.

     pack .getformat.buttonrow  -side left

#
#   Withdraw the window, then update all the geometry information
#   to determine how big the window wants to be, then centre the window
#   in parent and de-iconify it.

     wm withdraw .getformat
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .getformat]/2 + \
       [winfo x .]]
     set y [expr [winfo height .]/2 - [winfo reqheight .getformat]/2 + \
       [winfo y .]]
     wm geom .getformat +$x+$y
     wm deiconify .getformat

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab  .getformat
     focus .getformat

     tkwait variable getFormatButton

     if {$getFormatButton == "ok"} then {
        set columnName    [.getformat.entry.column get]
        set columnUnits   [.getformat.entry.units  get]
        set columnFormat  [.getformat.entry.format get]

        if {$columnUnits == ""} then {
           set columnUnits "<none>"
        }

        if {$columnFormat == ""} then {
           set columnFormat "<none>"
        }
     }

     if {$getFormatButton == "can"} then {
        set columnName    ""
        set columnUnits   ""
        set columnFormat  ""
     }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .getformat
     focus    $oldFocus

}
