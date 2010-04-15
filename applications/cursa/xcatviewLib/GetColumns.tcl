proc GetColumns { } {

#+ GetColumns
#
#  Get the list of columns and expressions to be displayed.
#
#  Arguments:
#    None.
#
#  Author:
#   ACD.

#  History:
#   24/5/94  (ACD): Original version.
#   31/3/95  (ACD): First stable version.
#   19/11/96 (ACD): Modified for Tcl/Tk v4.
#   9/12/98  (ACD): Added check for selected columns to the copy button.
#-

#
# Create the top level window.
    toplevel     .getcolumns   -class Dialog   -bd 10
    wm title     .getcolumns   "Input Columns"
    wm iconname  .getcolumns   Columns
    wm transient .getcolumns   .

#
#  Global variables.
#  ----------------

    global chosenColumns
    global catalogueList

#
#  Create a frame, list box and scroll bar for the list of columns.

    frame  .getcolumns.allcols

    scrollbar .getcolumns.allcols.colsxscroll -orient horizontal \
      -command {.getcolumns.allcols.listcolumns xview} \
      -relief sunken  -bd 2

    scrollbar .getcolumns.allcols.colsyscroll -orient vertical \
      -command {.getcolumns.allcols.listcolumns yview} \
      -relief sunken  -bd 2

    listbox .getcolumns.allcols.listcolumns  -relief groove  -bd 2  \
      -selectmode extended  \
      -xscroll {.getcolumns.allcols.colsxscroll set}  \
      -yscroll {.getcolumns.allcols.colsyscroll set}

#
#  Pack the list box into the window.

    pack  .getcolumns.allcols.colsyscroll  -side left    -fill y
    pack  .getcolumns.allcols.colsxscroll  -side bottom  -fill x
    pack  .getcolumns.allcols.listcolumns  -expand yes   -fill both

    pack  .getcolumns.allcols  -side left  -fill y  -expand yes

#
#  Insert the list of columns in the catalogue into the appropriate
#  list box.

    set numColumns [llength $catalogueList]

    set counter 0

    while {$counter < $numColumns} {
       set currentColumn [lrange $catalogueList $counter $counter]
       .getcolumns.allcols.listcolumns insert end $currentColumn
       set counter [expr $counter + 1]
    }

#
#  Create a frame to hold the buttons.

    frame .getcolumns.buttonFrame

#
#  Create the "copy", "expression", "all", "clear", "configuration", "OK",
#  "cancel", and "help" buttons.

    button .getcolumns.buttonFrame.copy -text Copy -width 6 \
      -command {
         set isSelection [selection own]
         if {$isSelection != ""} then {
            set ChosenColumns [selection get]
            set numElements [llength $ChosenColumns]
            set counter 0

            while {$counter < $numElements} {
              .getcolumns.selcols.selcolumns insert end \
                 [lindex $ChosenColumns $counter]
              set counter [expr $counter + 1]
            }
         } else {
            Error "Click on one or more column names before clicking this button."
         } }
    button .getcolumns.buttonFrame.expn -text Exprn. -width 6 \
      -command {GetExprn
         global exprnDefn
         if {$exprnDefn != ""} then {
            .getcolumns.selcols.selcolumns insert end $exprnDefn
         } }
    button .getcolumns.buttonFrame.all  -text All    -width 6 \
      -command {global catalogueList
         set numColumns [llength $catalogueList]
         set counter 0

         while {$counter < $numColumns} {
           set currentColumn [lrange $catalogueList $counter $counter]
           .getcolumns.selcols.selcolumns insert end $currentColumn
           set counter [expr $counter + 1]
         } }
    button .getcolumns.buttonFrame.clear -text Clear -width 6 \
      -command {
         set DeletedColumns [.getcolumns.selcols.selcolumns curselection]
         set numElements [llength $DeletedColumns]
         if {$numElements > 0} then {
           set counter $numElements

           while {$counter > 0} {
             set counter [expr $counter - 1]
             .getcolumns.selcols.selcolumns delete \
                [lindex $DeletedColumns $counter]
           }
         } else {
            .getcolumns.selcols.selcolumns delete 0 end
         }
       }
    button .getcolumns.buttonFrame.config -text Config. -width 6 \
      -command {GetDispConfig}
    button .getcolumns.buttonFrame.ok -text OK -width 6 \
      -command {set GetColumnsButton "ok"}
    button .getcolumns.buttonFrame.can -text Cancel -width 6 \
      -command {set GetColumnsButton "can"}
    button .getcolumns.buttonFrame.help -text Help -width 6 \
      -command {HelpText GetColumns_help}

#
#  Create the arrow.

    canvas .getcolumns.buttonFrame.arrow -height 20.0m -width 30.0m \
       -highlightthickness 0
    .getcolumns.buttonFrame.arrow create polygon \
      2.0m 6.0m  18.0m 6.0m  18.0m 2.0m  28.0m 10.0m  18.0m 18.0m \
      18.0m 14.0m  2.0m 14.0m  -fill black

#
#  Pack the buttons and arrow into their frame with a default border around
#  the OK button.

    frame .getcolumns.buttonFrame.default -relief sunken -bd 1
    raise .getcolumns.buttonFrame.ok .getcolumns.buttonFrame.default

    pack .getcolumns.buttonFrame.ok -in .getcolumns.buttonFrame.default \
      -padx 1m -pady 1m -ipadx 1m


    pack .getcolumns.buttonFrame.copy    -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .getcolumns.buttonFrame.expn    -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .getcolumns.buttonFrame.all     -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .getcolumns.buttonFrame.clear   -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m

    pack .getcolumns.buttonFrame.arrow   -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m

    pack .getcolumns.buttonFrame.config  -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .getcolumns.buttonFrame.default -side top -expand 1 -padx 3m -pady 2m
    pack .getcolumns.buttonFrame.can     -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .getcolumns.buttonFrame.help    -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m

#
#  Pack this frame into the window.

    pack  .getcolumns.buttonFrame  -expand y -side left

#
#  Create a frame, list box and scroll bar for the list of columns.

    frame  .getcolumns.selcols

    scrollbar .getcolumns.selcols.selxscroll -orient horizontal \
      -command {.getcolumns.selcols.selcolumns xview} \
      -relief sunken  -bd 2

    scrollbar .getcolumns.selcols.selyscroll -orient vertical \
      -command {.getcolumns.selcols.selcolumns yview} \
      -relief sunken  -bd 2

    listbox .getcolumns.selcols.selcolumns  -relief groove  -bd 2  \
      -selectmode extended  \
      -xscroll {.getcolumns.selcols.selxscroll set}  \
      -yscroll {.getcolumns.selcols.selyscroll set}

#
#  Pack the list box into the window.

    pack  .getcolumns.selcols.selyscroll  -side right   -fill y
    pack  .getcolumns.selcols.selxscroll  -side bottom -fill x
    pack  .getcolumns.selcols.selcolumns  -expand yes  -fill both

    pack  .getcolumns.selcols  -side left  -fill y  -expand yes

#
#  Insert the list of chosen columns into the appropriate list box.

    set chosenList [split $chosenColumns {;}]

    set numChosen [llength $chosenList]

    set counter 0

    while {$counter < $numChosen} {
       set currentChosen [lrange $chosenList $counter $counter]
       .getcolumns.selcols.selcolumns insert end $currentChosen
       set counter [expr $counter + 1]
    }

#
#  Withdraw the window, then update all the geometry information
#  so we know how big it wants to be, then centre the window in
#  parent and de-iconify it.

    wm withdraw .getcolumns
    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .getcolumns]/2 + [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .getcolumns]/2 + [winfo y .]]
    wm geom .getcolumns +$x+$y
    wm deiconify .getcolumns

#  Set a grab and claim the focus.
    set oldFocus [focus]
    grab .getcolumns
    focus .getcolumns

    global GetColumnsButton
    tkwait variable GetColumnsButton

#
#  If the "ok" button is pressed then export the list of chosen columns
#  into the appropriate global variable.  The procedure is;
#
#  Determine the number of elements in the list.
#  Intialise the assembled list.
#  for every element
#    Get the contents of the element.
#    Append the element to the assembled list.
#    If the element is not the last then
#      Append a semi-colopn.
#    end if
#  end for

    if {$GetColumnsButton == "ok"} {
       set numChosen [.getcolumns.selcols.selcolumns size]

       set counter 0

       set    chosenColumns ""

       while {$counter < $numChosen} {
          set currentColumn [.getcolumns.selcols.selcolumns get $counter]
          append chosenColumns $currentColumn

          if {$counter < $numChosen - 1} then {
             append chosenColumns ";"
          }

          set counter [expr $counter + 1]
       }
    }

    if {$GetColumnsButton == "can"} {
       set  chosenColumns   ""
    }

#
#  Destroy the dialogue box and restore the focus.

    destroy  .getcolumns
    focus    $oldFocus

}
