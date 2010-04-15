proc GetStatsCols { } {

#+ GetStatsCols
#
#  Get the list of columns for which statistics are to be computed.
#
#  Arguments:
#    None.
#
#  Author:
#   ACD.

#  History:
#   5/12/96  (ACD): Original version.
#   9/12/98  (ACD): Added check for selected columns to the copy button.
#-

#
# Create the top level window.
    toplevel     .getstatscols   -class Dialog   -bd 10
    wm title     .getstatscols   "Input Columns"
    wm iconname  .getstatscols   Columns
    wm transient .getstatscols   .

#
#  Global variables.
#  ----------------

    global catalogueList
    global statsColumns
    global statsFile
    global statsDecPl

#
#  Local copies of these variables.

    global localDecPl
    set    localDecPl  $statsDecPl

    global localStatsFile
    set    localStatsFile  "none"

#
#  Create a frame, list box and scroll bar for the list of columns.

    frame  .getstatscols.box
    frame  .getstatscols.box.allcols

    scrollbar .getstatscols.box.allcols.colsxscroll -orient horizontal \
      -command {.getstatscols.box.allcols.listcolumns xview} \
      -relief sunken  -bd 2

    scrollbar .getstatscols.box.allcols.colsyscroll -orient vertical \
      -command {.getstatscols.box.allcols.listcolumns yview} \
      -relief sunken  -bd 2

    listbox .getstatscols.box.allcols.listcolumns  -relief groove  -bd 2  \
      -selectmode extended  \
      -xscroll {.getstatscols.box.allcols.colsxscroll set}  \
      -yscroll {.getstatscols.box.allcols.colsyscroll set}

#
#  Pack the list box into the window.

    pack  .getstatscols.box.allcols.colsyscroll  -side left    -fill y
    pack  .getstatscols.box.allcols.colsxscroll  -side bottom  -fill x
    pack  .getstatscols.box.allcols.listcolumns  -expand yes   -fill both

    pack  .getstatscols.box.allcols  -side left  -fill y  -expand yes

#
#  Insert the list of columns in the catalogue into the appropriate
#  list box.

    set numColumns [llength $catalogueList]

    set counter 0

    while {$counter < $numColumns} {
       set currentColumn [lrange $catalogueList $counter $counter]
       .getstatscols.box.allcols.listcolumns insert end $currentColumn
       set counter [expr $counter + 1]
    }

#
#  Create a frame to hold the buttons.

    frame .getstatscols.box.buttonFrame

#
#  Create the "copy", "all", "clear", "OK", "cancel", and "help" buttons.

    button .getstatscols.box.buttonFrame.copy -text Copy -width 6 \
      -command {
         set isSelection [selection own]
         if {$isSelection != ""} then {
            set ChosenColumns [selection get]
            set numElements [llength $ChosenColumns]
            set counter 0

            while {$counter < $numElements} {
              .getstatscols.box.selcols.selcolumns insert end \
                 [lindex $ChosenColumns $counter]
              set counter [expr $counter + 1]
            }
         } else {
            Error "Click on one or more column names before clicking this button."
         } }
    button .getstatscols.box.buttonFrame.all  -text All    -width 6 \
      -command {global catalogueList
         set numColumns [llength $catalogueList]
         set counter 0

         while {$counter < $numColumns} {
           set currentColumn [lrange $catalogueList $counter $counter]
           .getstatscols.box.selcols.selcolumns insert end $currentColumn
           set counter [expr $counter + 1]
         } }
    button .getstatscols.box.buttonFrame.clear -text Clear -width 6 \
      -command {
         set DeletedColumns [.getstatscols.box.selcols.selcolumns curselection]
         set numElements [llength $DeletedColumns]
         if {$numElements > 0} then {
           set counter $numElements

           while {$counter > 0} {
             set counter [expr $counter - 1]
             .getstatscols.box.selcols.selcolumns delete \
                [lindex $DeletedColumns $counter]
           }
         } else {
            .getstatscols.box.selcols.selcolumns delete 0 end
         }
       }
    button .getstatscols.box.buttonFrame.ok -text OK -width 6 \
      -command {set GetColumnsButton "ok"}
    button .getstatscols.box.buttonFrame.can -text Cancel -width 6 \
      -command {set GetColumnsButton "can"}
    button .getstatscols.box.buttonFrame.help -text Help -width 6 \
      -command {HelpText GetStatsCols_help}

#
#  Create the arrow.

    canvas .getstatscols.box.buttonFrame.arrow -height 20.0m -width 30.0m \
       -highlightthickness 0
    .getstatscols.box.buttonFrame.arrow create polygon \
      2.0m 6.0m  18.0m 6.0m  18.0m 2.0m  28.0m 10.0m  18.0m 18.0m \
      18.0m 14.0m  2.0m 14.0m  -fill black

#
#  Pack the buttons and arrow into their frame with a default border around
#  the OK button.

    frame .getstatscols.box.buttonFrame.default -relief sunken -bd 1
    raise .getstatscols.box.buttonFrame.ok .getstatscols.box.buttonFrame.default

    pack .getstatscols.box.buttonFrame.ok -in .getstatscols.box.buttonFrame.default \
      -padx 1m -pady 1m -ipadx 1m


    pack .getstatscols.box.buttonFrame.copy    -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .getstatscols.box.buttonFrame.all     -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .getstatscols.box.buttonFrame.clear   -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m

    pack .getstatscols.box.buttonFrame.arrow   -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m

    pack .getstatscols.box.buttonFrame.default -side top -expand 1 -padx 3m -pady 2m
    pack .getstatscols.box.buttonFrame.can     -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .getstatscols.box.buttonFrame.help    -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m

#
#  Pack this frame into the window.

    pack  .getstatscols.box.buttonFrame  -expand y -side left

#
#  Create a frame, list box and scroll bar for the list of columns.

    frame  .getstatscols.box.selcols

    scrollbar .getstatscols.box.selcols.selxscroll -orient horizontal \
      -command {.getstatscols.box.selcols.selcolumns xview} \
      -relief sunken  -bd 2

    scrollbar .getstatscols.box.selcols.selyscroll -orient vertical \
      -command {.getstatscols.box.selcols.selcolumns yview} \
      -relief sunken  -bd 2

    listbox .getstatscols.box.selcols.selcolumns  -relief groove  -bd 2  \
      -selectmode extended  \
      -xscroll {.getstatscols.box.selcols.selxscroll set}  \
      -yscroll {.getstatscols.box.selcols.selyscroll set}

#
#  Pack the list box into the window.

    pack  .getstatscols.box.selcols.selyscroll  -side right   -fill y
    pack  .getstatscols.box.selcols.selxscroll  -side bottom -fill x
    pack  .getstatscols.box.selcols.selcolumns  -expand yes  -fill both

    pack  .getstatscols.box.selcols  -side left  -fill y  -expand yes

#
#  Insert the list of chosen columns into the appropriate list box.

    set chosenList [split $statsColumns {;}]

    set numChosen [llength $chosenList]

    set counter 0

    while {$counter < $numChosen} {
       set currentChosen [lrange $chosenList $counter $counter]
       .getstatscols.box.selcols.selcolumns insert end $currentChosen
       set counter [expr $counter + 1]
    }

#
#  Pack the frame containing the listboxes and buttons into the window.

    pack  .getstatscols.box  -side top

#
#  Create the menu button and value box for the file name and pack
#  them into the frame.

    frame .getstatscols.file

    menubutton .getstatscols.file.but  \
      -relief sunken  -bd 2 -width 13  \
      -text "File"  \
      -menu .getstatscols.file.but.menu
    pack .getstatscols.file.but  -side left

    entry .getstatscols.file.value  -width 50  -relief flat  \
      -highlightthickness 0  -text "None"
    pack  .getstatscols.file.value  -side left  -padx 2m
    .getstatscols.file.value  delete 0 end
    .getstatscols.file.value  insert 0 "None"
    .getstatscols.file.value  configure  -state disabled

    pack .getstatscols.file  -side top  -anchor w  -pady 2m

#
#  Create the menu options for the file button.

    menu .getstatscols.file.but.menu

    .getstatscols.file.but.menu  add command \
       -label No  \
       -command {
         .getstatscols.file.value  configure  -state normal
         .getstatscols.file.value  delete 0 end
         .getstatscols.file.value  insert 0 "None"
         .getstatscols.file.value  configure  -state disabled

         global localStatsFile
         set    localStatsFile  "none"
        }

    .getstatscols.file.but.menu  add command \
       -label "Yes" \
       -command {
          GetParam  70  "File name:" statistics_file_help
          if {$GetParamValue != ""} then {
             set  localStatsFile  $GetParamValue

            .getstatscols.file.value  configure  -state normal
            .getstatscols.file.value  delete 0 end
            .getstatscols.file.value  insert 0 $localStatsFile
            .getstatscols.file.value  configure  -state disabled
          }
        }

#
#  Create the menu button and value box for the number of decimal
#  places and pack them into the frame.

    frame .getstatscols.decpl

    menubutton .getstatscols.decpl.but  \
      -relief sunken  -bd 2 -width 13  \
      -text "Decimal places"  \
      -menu .getstatscols.decpl.but.menu
    pack .getstatscols.decpl.but  -side left

    entry .getstatscols.decpl.value  -width 5  -relief sunken  -bd 2
    pack  .getstatscols.decpl.value  -side left  -padx 2m
    .getstatscols.decpl.value  insert 0 $statsDecPl

    pack .getstatscols.decpl  -side top  -anchor w  -pady 2m

#
#  Create the menu options for the decimal places button.

    menu .getstatscols.decpl.but.menu

    .getstatscols.decpl.but.menu  add command \
       -label 1  \
       -command {
         .getstatscols.decpl.value  delete 0 end
         .getstatscols.decpl.value  insert 0 "1"

         global localDecPl
         set    localDecPl  1
        }

    .getstatscols.decpl.but.menu  add command \
       -label 2  \
       -command {
         .getstatscols.decpl.value  delete 0 end
         .getstatscols.decpl.value  insert 0 "2"

         global localDecPl
         set    localDecPl  2
        }

    .getstatscols.decpl.but.menu  add command \
       -label 3  \
       -command {
         .getstatscols.decpl.value  delete 0 end
         .getstatscols.decpl.value  insert 0 "3"

         global localDecPl
         set    localDecPl  3
        }

    .getstatscols.decpl.but.menu  add command \
       -label 4  \
       -command {
         .getstatscols.decpl.value  delete 0 end
         .getstatscols.decpl.value  insert 0 "4"

         global localDecPl
         set    localDecPl  4
        }

    .getstatscols.decpl.but.menu  add command \
       -label 5  \
       -command {
         .getstatscols.decpl.value  delete 0 end
         .getstatscols.decpl.value  insert 0 "5"

         global localDecPl
         set    localDecPl  5
        }

    .getstatscols.decpl.but.menu  add command \
       -label 6  \
       -command {
         .getstatscols.decpl.value  delete 0 end
         .getstatscols.decpl.value  insert 0 "6"

         global localDecPl
         set    localDecPl  6
        }

    .getstatscols.decpl.but.menu  add command \
       -label 7  \
       -command {
         .getstatscols.decpl.value  delete 0 end
         .getstatscols.decpl.value  insert 0 "7"

         global localDecPl
         set    localDecPl  7
        }

    .getstatscols.decpl.but.menu  add command \
       -label 8  \
       -command {
         .getstatscols.decpl.value  delete 0 end
         .getstatscols.decpl.value  insert 0 "8"

         global localDecPl
         set    localDecPl  8
        }

    .getstatscols.decpl.but.menu  add command \
       -label 9  \
       -command {
         .getstatscols.decpl.value  delete 0 end
         .getstatscols.decpl.value  insert 0 "9"

         global localDecPl
         set    localDecPl  9
        }

    .getstatscols.decpl.but.menu  add command \
       -label 10  \
       -command {
         .getstatscols.decpl.value  delete 0 end
         .getstatscols.decpl.value  insert 0 "10"

         global localDecPl
         set    localDecPl  10
        }


#
#  Withdraw the window, then update all the geometry information
#  so we know how big it wants to be, then centre the window in
#  parent and de-iconify it.

    wm withdraw .getstatscols
    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .getstatscols]/2 + [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .getstatscols]/2 + [winfo y .]]
    wm geom .getstatscols +$x+$y
    wm deiconify .getstatscols

#  Set a grab and claim the focus.
    set oldFocus [focus]
    grab .getstatscols
    focus .getstatscols

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
       set numChosen [.getstatscols.box.selcols.selcolumns size]

       set counter 0

       set    statsColumns ""

       while {$counter < $numChosen} {
          set currentColumn [.getstatscols.box.selcols.selcolumns get $counter]
          append statsColumns $currentColumn

          if {$counter < $numChosen - 1} then {
             append statsColumns ";"
          }

          set counter [expr $counter + 1]
       }

       set  statsDecPl  [.getstatscols.decpl.value get]
       set  statsFile   $localStatsFile
    }

    if {$GetColumnsButton == "can"} {
       set  statsColumns   ""
    }

#
#  Destroy the dialogue box and restore the focus.

    destroy  .getstatscols
    focus    $oldFocus

}
