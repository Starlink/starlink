proc GetFile { } {

#+ GetFile
#
#  Get the details to specify a text file.  The details are the name of
#  the file and the various options for the items to be included in the
#  file.
#
#  Given
#    none.
#
#  Author:
#   ACD: A C Davenhall (Leicester).

#  History:
#   21/10/94 (ACD): Original version.
#   17/3/95  (ACD): First stable version.
#   20/10/96 (ACD): Modified for Tcl/Tk v4 and expect v5.
#   19/11/96 (ACD): Completed modifications for Tcl/Tk v4.
#-

#
#   Create the top level window.

     toplevel     .getfile   -class Dialog   -bd 10
     wm title     .getfile   "Text file"
     wm iconname  .getfile   Textfile
     wm transient .getfile   .

#
#   Declare the global variables.
#   ----------------------------

#
#   The state returned by clicking on the "OK" or "Cancel" buttons.

     global getFileButton

#
#   The options for the various items in the text file.

     global textfileSummary
     global textfileColumns
     global textfileParameters
     global textfileText
     global textfileTable

#
#   Local copies for these items.

     global localSummary
     global localColumns
     global localParameters
     global localText
     global localTable

#
#   The first and last rows to be listed.

     global textfileFirstRow
     global textfileLastRow

#
#   The name of the file.

     global textfileName

#
#   Bind the return key to set global variable button to "ok".  That is,
#   hitting return will have the same effect as clicking on the "ok"
#   button.

     bind  .getfile  <Return> {set getFileButton "ok" }

#
#   Create a frame to hold the entry box and title for the file name.

     frame .getfile.name

#
#   Create the entry box and its associated title.

     text .getfile.name.title  -height 1 -width 60 -relief flat  \
       -highlightthickness 0
     pack .getfile.name.title  -side top
     .getfile.name.title  insert 1.0 "File name:"

     entry .getfile.name.value  -relief sunken -bd 2 -width 60
     pack  .getfile.name.value  -side top

     bind  .getfile.name.value  <Return> {set getFileButton "ok" }

#
#   Pack the frame into the window.

     pack  .getfile.name  -side top

#
#   Create the frame to hold the two vertical rows of buttons and
#   the row of current values for the options.

     frame .getfile.button

#
#   Create the frames to hold the vertical row of option menu buttons
#   and the row of values for the current options.

     frame .getfile.button.opt

#
#   Create the menu buttons and value boxes and pack them into their frame.

     frame .getfile.button.opt.summary

     menubutton .getfile.button.opt.summary.but  \
       -relief sunken  -bd 2 -width 10  \
       -text "Summary"  \
       -menu .getfile.button.opt.summary.but.menu
     pack .getfile.button.opt.summary.but  -side left

     entry .getfile.button.opt.summary.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getfile.button.opt.summary.value  -side left  -padx 2m
     .getfile.button.opt.summary.value  configure  -state disabled

     pack  .getfile.button.opt.summary -side top -pady 2m

     frame .getfile.button.opt.columns

     menubutton .getfile.button.opt.columns.but  \
       -relief sunken  -bd 2 -width 10  \
       -text "Columns" \
       -menu .getfile.button.opt.columns.but.menu
     pack .getfile.button.opt.columns.but  -side left

     entry .getfile.button.opt.columns.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getfile.button.opt.columns.value  -side left  -padx 2m
     .getfile.button.opt.columns.value  configure  -state disabled

     pack  .getfile.button.opt.columns -side top -pady 2m


     frame .getfile.button.opt.parameters

     menubutton .getfile.button.opt.parameters.but  \
       -relief sunken  -bd 2 -width 10  \
       -text "Parameters"  \
       -menu .getfile.button.opt.parameters.but.menu
     pack .getfile.button.opt.parameters.but  -side left

     entry .getfile.button.opt.parameters.value  -width 15  -relief flat \
       -highlightthickness 0
     pack  .getfile.button.opt.parameters.value  -side left  -padx 2m
     .getfile.button.opt.parameters.value  configure  -state disabled

     pack  .getfile.button.opt.parameters -side top -pady 2m


     frame .getfile.button.opt.text

     menubutton .getfile.button.opt.text.but  \
       -relief sunken  -bd 2 -width 10  \
       -text "Text" \
       -menu .getfile.button.opt.text.but.menu
     pack .getfile.button.opt.text.but  -side left

     entry .getfile.button.opt.text.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getfile.button.opt.text.value  -side left  -padx 2m
     .getfile.button.opt.text.value  configure  -state disabled

     pack  .getfile.button.opt.text -side top -pady 2m


     frame .getfile.button.opt.table

     menubutton .getfile.button.opt.table.but  \
       -relief sunken  -bd 2 -width 10  \
       -text "Table"  \
       -menu .getfile.button.opt.table.but.menu
     pack .getfile.button.opt.table.but  -side left

     entry .getfile.button.opt.table.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getfile.button.opt.table.value  -side left  -padx 2m
     .getfile.button.opt.table.value  configure  -state disabled

     pack  .getfile.button.opt.table -side top -pady 2m

#
#   Also create the text and value boxes which go below them and pack
#   them into their frame.

     frame .getfile.button.opt.firstrow

     label .getfile.button.opt.firstrow.label -text "First row:"  \
       -width 10
     pack  .getfile.button.opt.firstrow.label  -side left

     entry .getfile.button.opt.firstrow.value -width 15  \
       -relief sunken  -bd 2
     pack  .getfile.button.opt.firstrow.value  -side left -padx 2m
     .getfile.button.opt.firstrow.value insert 0 $textfileFirstRow

     pack  .getfile.button.opt.firstrow -side top -pady 2m


     frame .getfile.button.opt.lastrow

     button .getfile.button.opt.lastrow.label \
       -text "Last row:"  \
       -width 10 \
       -command {
          .getfile.button.opt.lastrow.value delete 0 end
          .getfile.button.opt.lastrow.value insert 0 "last"
        }
     pack  .getfile.button.opt.lastrow.label  -side left

     entry .getfile.button.opt.lastrow.value -width 15  \
       -relief sunken  -bd 2
     pack  .getfile.button.opt.lastrow.value  -side left -padx 2m
     if {$textfileLastRow == 0} then {
        .getfile.button.opt.lastrow.value insert 0 "last"
     } else {
        .getfile.button.opt.lastrow.value insert 0 $textfileLastRow
     }

     pack  .getfile.button.opt.lastrow -side top -pady 2m

#
#   Define the options for the 'summary' menu.

     menu .getfile.button.opt.summary.but.menu

     .getfile.button.opt.summary.but.menu  add command \
        -label None  \
        -command {
          .getfile.button.opt.summary.value  configure  -state normal
          .getfile.button.opt.summary.value  delete 0 end
          .getfile.button.opt.summary.value  insert 0 "None"
          .getfile.button.opt.summary.value  configure  -state disabled

          global localSummary
          set    localSummary "A"
         }

     .getfile.button.opt.summary.but.menu  add command \
        -label Present  \
        -command {
          .getfile.button.opt.summary.value  configure  -state normal
          .getfile.button.opt.summary.value  delete 0 end
          .getfile.button.opt.summary.value  insert 0 "Present"
          .getfile.button.opt.summary.value  configure  -state disabled

          global localSummary
          set    localSummary "F"
         }

#
#   Define the options for the 'columns' menu.

     menu .getfile.button.opt.columns.but.menu

     .getfile.button.opt.columns.but.menu  add command \
        -label None  \
        -command {
          .getfile.button.opt.columns.value  configure  -state normal
          .getfile.button.opt.columns.value  delete 0 end
          .getfile.button.opt.columns.value  insert 0 "None"
          .getfile.button.opt.columns.value  configure  -state disabled

          global localColumns
          set    localColumns "A"
         }

     .getfile.button.opt.columns.but.menu  add command \
        -label Summary  \
        -command {
          .getfile.button.opt.columns.value  configure  -state normal
          .getfile.button.opt.columns.value  delete 0 end
          .getfile.button.opt.columns.value  insert 0 "Summary"
          .getfile.button.opt.columns.value  configure  -state disabled

          global localColumns
          set    localColumns "S"
         }

     .getfile.button.opt.columns.but.menu  add command \
        -label Full  \
        -command {
          .getfile.button.opt.columns.value  configure  -state normal
          .getfile.button.opt.columns.value  delete 0 end
          .getfile.button.opt.columns.value  insert 0 "Full"
          .getfile.button.opt.columns.value  configure  -state disabled

          global localColumns
          set    localColumns "F"
         }

#
#   Define the options for the 'parameters' menu.

     menu .getfile.button.opt.parameters.but.menu

     .getfile.button.opt.parameters.but.menu  add command \
        -label None  \
        -command {
          .getfile.button.opt.parameters.value  configure  -state normal
          .getfile.button.opt.parameters.value  delete 0 end
          .getfile.button.opt.parameters.value  insert 0 "None"
          .getfile.button.opt.parameters.value  configure  -state disabled

          global localParameters
          set    localParameters "A"
         }

     .getfile.button.opt.parameters.but.menu  add command \
        -label Summary  \
        -command {
          .getfile.button.opt.parameters.value  configure  -state normal
          .getfile.button.opt.parameters.value  delete 0 end
          .getfile.button.opt.parameters.value  insert 0 "Summary"
          .getfile.button.opt.parameters.value  configure  -state disabled

          global localParameters
          set    localParameters "S"
         }

     .getfile.button.opt.parameters.but.menu  add command \
        -label Full  \
        -command {
          .getfile.button.opt.parameters.value  configure  -state normal
          .getfile.button.opt.parameters.value  delete 0 end
          .getfile.button.opt.parameters.value  insert 0 "Full"
          .getfile.button.opt.parameters.value  configure  -state disabled

          global localParameters
          set    localParameters "F"
         }

#
#   Define the options for the 'text' menu.

     menu .getfile.button.opt.text.but.menu

     .getfile.button.opt.text.but.menu  add command \
        -label None  \
        -command {
          .getfile.button.opt.text.value  configure  -state normal
          .getfile.button.opt.text.value  delete 0 end
          .getfile.button.opt.text.value  insert 0 "None"
          .getfile.button.opt.text.value  configure  -state disabled

          global localText
          set    localText "A"
         }

     .getfile.button.opt.text.but.menu  add command \
        -label Present  \
        -command {
          .getfile.button.opt.text.value  configure  -state normal
          .getfile.button.opt.text.value  delete 0 end
          .getfile.button.opt.text.value  insert 0 "Present"
          .getfile.button.opt.text.value  configure  -state disabled

          global localText
          set    localText "F"
         }

#
#   Define the options for the 'table' menu.

     menu .getfile.button.opt.table.but.menu

     .getfile.button.opt.table.but.menu  add command \
        -label None  \
        -command {
          .getfile.button.opt.table.value  configure  -state normal
          .getfile.button.opt.table.value  delete 0 end
          .getfile.button.opt.table.value  insert 0 "None"
          .getfile.button.opt.table.value  configure  -state disabled

          global localTable
          set    localTable "A"
         }

     .getfile.button.opt.table.but.menu  add command \
        -label "No column headings"  \
        -command {
          .getfile.button.opt.table.value  configure  -state normal
          .getfile.button.opt.table.value  delete 0 end
          .getfile.button.opt.table.value  insert 0 "No headings"
          .getfile.button.opt.table.value  configure  -state disabled

          global localTable
          set    localTable "S"
         }

     .getfile.button.opt.table.but.menu  add command \
        -label "With column headings"  \
        -command {
          .getfile.button.opt.table.value  configure  -state normal
          .getfile.button.opt.table.value  delete 0 end
          .getfile.button.opt.table.value  insert 0 "With headings"
          .getfile.button.opt.table.value  configure  -state disabled

          global localTable
          set    localTable "F"
         }

#
#   Pack the frames enclosing the rows of buttons and entry boxes into
#   their frame.

     pack .getfile.button.opt    -side left

#
#   Create a frame to hold the row of control buttons.

     frame .getfile.button.ctrl

#
#   Create each of the buttons.
#
#   OK.

     button .getfile.button.ctrl.ok \
       -text OK \
       -width 6 \
       -command {set getFileButton "ok"}

#
#   Cancel.

     button .getfile.button.ctrl.can \
       -text Cancel \
       -width 6 \
       -command {set getFileButton "can"}

#
#   Help.

     button .getfile.button.ctrl.help \
       -text Help \
       -width 6 \
       -command {HelpText GetFile_help}

#
#   Pack the buttons into their enclosing frame with a default border around
#   the OK button.

     frame .getfile.button.ctrl.default -relief sunken -bd 1
     raise .getfile.button.ctrl.ok .getfile.button.ctrl.default

     pack  .getfile.button.ctrl.ok -in .getfile.button.ctrl.default \
       -side top  -padx 1m -pady 1m -ipadx 1m

     pack .getfile.button.ctrl.default  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getfile.button.ctrl.can   \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getfile.button.ctrl.help  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

#
#   Pack the frame of buttons into its frame.

     pack .getfile.button.ctrl  -side left

#
#   Pack the frame holding all the buttons into the window.


     pack .getfile.button  -side top

#
#   Withdraw the window, then update all the geometry information
#   to determine how big the window wants to be, then centre the window
#   in parent and de-iconify it.

     wm withdraw .getfile
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .getfile]/2 + \
       [winfo x .]]
     set y [expr [winfo height .]/2 - [winfo reqheight .getfile]/2 + \
       [winfo y .]]
     wm geom .getfile +$x+$y
     wm deiconify .getfile

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab  .getfile
     focus .getfile

#
#   Set the defaults for the return values for the options.

     if {$textfileSummary == "A"} then {
        .getfile.button.opt.summary.value  configure  -state normal
        .getfile.button.opt.summary.value  delete 0 end
        .getfile.button.opt.summary.value  insert 0 "None"
        .getfile.button.opt.summary.value  configure  -state disabled

        set localSummary "A"
     } else {
        .getfile.button.opt.summary.value  configure  -state normal
        .getfile.button.opt.summary.value  delete 0 end
        .getfile.button.opt.summary.value  insert 0 "Present"
        .getfile.button.opt.summary.value  configure  -state disabled

        set localSummary "F"
     }


     if {$textfileColumns == "A" } then {
        .getfile.button.opt.columns.value  configure  -state normal
        .getfile.button.opt.columns.value  delete 0 end
        .getfile.button.opt.columns.value  insert 0 "None"
        .getfile.button.opt.columns.value  configure  -state disabled

        set    localColumns "A"
     } elseif {$textfileColumns == "S" } then {
        .getfile.button.opt.columns.value  configure  -state normal
        .getfile.button.opt.columns.value  delete 0 end
        .getfile.button.opt.columns.value  insert 0 "Summary"
        .getfile.button.opt.columns.value  configure  -state disabled

        set localColumns "S"
     } else {
        .getfile.button.opt.columns.value  configure  -state normal
        .getfile.button.opt.columns.value  delete 0 end
        .getfile.button.opt.columns.value  insert 0 "Full"
        .getfile.button.opt.columns.value  configure  -state disabled

        set localColumns "F"
     }

     if {$textfileParameters == "A"} then {
        .getfile.button.opt.parameters.value  configure  -state normal
        .getfile.button.opt.parameters.value  delete 0 end
        .getfile.button.opt.parameters.value  insert 0 "None"
        .getfile.button.opt.parameters.value  configure  -state disabled

        set localParameters "A"
     } elseif {$textfileParameters == "S"} then {
        .getfile.button.opt.parameters.value  configure  -state normal
        .getfile.button.opt.parameters.value  delete 0 end
        .getfile.button.opt.parameters.value  insert 0 "Summary"
        .getfile.button.opt.parameters.value  configure  -state disabled

        set localParameters "S"
     } else {
        .getfile.button.opt.parameters.value  configure  -state normal
        .getfile.button.opt.parameters.value  delete 0 end
        .getfile.button.opt.parameters.value  insert 0 "Full"
        .getfile.button.opt.parameters.value  configure  -state disabled

        set localParameters "F"
     }

     if {$textfileText == "A"} then {
        .getfile.button.opt.text.value  configure  -state normal
        .getfile.button.opt.text.value  delete 0 end
        .getfile.button.opt.text.value  insert 0 "None"
        .getfile.button.opt.text.value  configure  -state disabled

        set localText "A"
     } else {
        .getfile.button.opt.text.value  configure  -state normal
        .getfile.button.opt.text.value  delete 0 end
        .getfile.button.opt.text.value  insert 0 "Present"
        .getfile.button.opt.text.value  configure  -state disabled

        set localText "F"
     }

     if {$textfileTable == "A"} then {
        .getfile.button.opt.table.value  configure  -state normal
        .getfile.button.opt.table.value  delete 0 end
        .getfile.button.opt.table.value  insert 0 "None"
        .getfile.button.opt.table.value  configure  -state disabled

        set localTable "A"
     } elseif {$textfileTable == "S"} then {
        .getfile.button.opt.table.value  configure  -state normal
        .getfile.button.opt.table.value  delete 0 end
        .getfile.button.opt.table.value  insert 0 "No headings"
        .getfile.button.opt.table.value  configure  -state disabled

        set localTable "S"
     } else {
        .getfile.button.opt.table.value  configure  -state normal
        .getfile.button.opt.table.value  delete 0 end
        .getfile.button.opt.table.value  insert 0 "With headings"
        .getfile.button.opt.table.value  configure  -state disabled

        set localTable "F"
     }

#
#   Wait for the "OK" button to be pushed.

     tkwait variable getFileButton

     if {$getFileButton == "ok"} then {
        set textfileName [.getfile.name.value  get]

        set  textfileSummary     $localSummary
        set  textfileColumns     $localColumns
        set  textfileParameters  $localParameters
        set  textfileText        $localText
        set  textfileTable       $localTable

        set textfileFirstRow [.getfile.button.opt.firstrow.value get]

        set lastrow [.getfile.button.opt.lastrow.value get]

        if {$lastrow == "last"} then {
           set textfileLastRow 0
        } else {
           set textfileLastRow $lastrow
        }
     }

     if {$getFileButton == "can"} then {
        set textfileName ""
     }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .getfile
     focus    $oldFocus

}
