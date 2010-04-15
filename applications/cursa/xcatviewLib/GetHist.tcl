proc GetHist { } {

#+ GetHist
#
#  Procedure to get the details for a histogram.
#
#  Given:
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)

#  History:
#   17/11/99 (ACD): Original version.
#   18/11/99 (ACD): First stable version.
#-

#
#   Create the top level window.

     toplevel     .gethist   -class Dialog   -bd 10
     wm title     .gethist   "Histogram"
     wm iconname  .gethist   Hist
     wm transient .gethist   .

#
#   Declare the global variables.
#   ----------------------------

#
#   The state returned by clicking on the "OK" or "Cancel" buttons.

     global getHistButton

#
#   The details of the column returned by the A-task.

     global catalogueList

#
#   The current list of columns for display.

     global chosenColumns

#
#   The options for the various items specifying the plot.

     global histFlag
     global histTitle
     global histXaxis
     global histLineColour
     global histAutoScale
     global histXmin
     global histXmax
     global histBinSpec
     global histBinDet
     global histNormal

#
#   Local copies of some of these items.

     global localLineColour
     global localAutoScale
     global localBinSpec
     global localBinDet
     global localNormal

#
#   Bind the return key to set global variable button to "ok".  That is,
#   hitting return will have the same effect as clicking on the "ok"
#   button.

     bind  .gethist  <Return> {set getHistButton "ok" }

#
#   Create a frame to hold the listbox, create the listbox and its
#   associated scroll bars.

     frame  .gethist.cols

     scrollbar .gethist.cols.xscroll  -orient horizontal \
       -command {.gethist.cols.list  xview} \
       -relief sunken  -bd 2

     scrollbar .gethist.cols.yscroll  -orient vertical \
       -command {.gethist.cols.list  yview} \
       -relief sunken  -bd 2

     listbox .gethist.cols.list  -relief groove  -bd 2  \
       -width 16 -height 7  \
       -xscroll {.gethist.cols.xscroll  set}  \
       -yscroll {.gethist.cols.yscroll  set}

#
#   Pack the listbox and scroll bars into their frame and pack the
#   frame into the window.

     pack .gethist.cols.yscroll  -side left    -fill y
     pack .gethist.cols.xscroll  -side bottom  -fill x
     pack .gethist.cols.list     -expand yes   -fill both

     pack .gethist.cols  -side left  -padx 3m  -fill y

#
#   Insert the list of columns on which range selections may be
#   performed into the list box.  The default is all the columns.

     set numColumns [llength $catalogueList]

     set counter 0

     while {$counter < $numColumns} {
        set currentColumn [lrange $catalogueList $counter $counter]
        .gethist.cols.list insert end $currentColumn
        set counter [expr $counter + 1]
     }

#
#   Create a frame to hold the entry boxes and their labels, then
#   create these boxes and labels and pack them into their frame.

     frame .gethist.opt

#
#   Title.

     frame .gethist.opt.title

     label .gethist.opt.title.label -text "Title:"  \
       -width 10
     pack  .gethist.opt.title.label -side left

     entry .gethist.opt.title.value -width 30  \
       -relief sunken  -bd 2
     pack  .gethist.opt.title.value  -side left -padx 2m

     pack  .gethist.opt.title -side top -pady 2m

#
#   X axis.

     frame .gethist.opt.xaxis

     label .gethist.opt.xaxis.label -text "X axis:"  \
       -width 10
     pack  .gethist.opt.xaxis.label -side left


     entry .gethist.opt.xaxis.value -width 30  \
       -relief sunken  -bd 2
     pack  .gethist.opt.xaxis.value  -side left -padx 2m

     pack  .gethist.opt.xaxis -side top -pady 2m

#
#   Bin details.

     frame .gethist.opt.binDet

     menubutton .gethist.opt.binDet.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Bin details"  \
       -menu .gethist.opt.binDet.but.menu
     pack .gethist.opt.binDet.but -side left

     entry .gethist.opt.binDet.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .gethist.opt.binDet.value  -side left  -padx 2m
     .gethist.opt.binDet.value  configure  -state disabled

     pack  .gethist.opt.binDet -side top -pady 2m

#
#   Normalise histogram?

     frame .gethist.opt.normal

     menubutton .gethist.opt.normal.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Normalise"  \
       -menu .gethist.opt.normal.but.menu
     pack .gethist.opt.normal.but -side left

     entry .gethist.opt.normal.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .gethist.opt.normal.value  -side left  -padx 2m
     .gethist.opt.normal.value  configure  -state disabled

     pack  .gethist.opt.normal -side top -pady 2m

#
#   Line colour.

     frame .gethist.opt.lineColour

     menubutton .gethist.opt.lineColour.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Line Colour"  \
       -menu .gethist.opt.lineColour.but.menu
     pack .gethist.opt.lineColour.but -side left

     entry .gethist.opt.lineColour.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .gethist.opt.lineColour.value  -side left  -padx 2m
     .gethist.opt.lineColour.value  configure  -state disabled

     pack  .gethist.opt.lineColour -side top -pady 2m

#
#   Auto-scale flag.

     frame .gethist.opt.autoScale

     menubutton .gethist.opt.autoScale.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Auto-scale?"  \
       -menu .gethist.opt.autoScale.but.menu
     pack .gethist.opt.autoScale.but -side left

     entry .gethist.opt.autoScale.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .gethist.opt.autoScale.value  -side left  -padx 2m
     .gethist.opt.autoScale.value  configure  -state disabled

     pack  .gethist.opt.autoScale -side top -pady 2m

#
#   X-minimum.

     frame .gethist.opt.xmin

     label .gethist.opt.xmin.label -text "X minimum:"  \
       -width 20
     pack  .gethist.opt.xmin.label -side left

     entry .gethist.opt.xmin.value -width 30  \
       -relief sunken  -bd 2
     pack  .gethist.opt.xmin.value  -side left -padx 2m

     pack  .gethist.opt.xmin -side top -pady 2m

#
#   X-maximum.

     frame .gethist.opt.xmax

     label .gethist.opt.xmax.label -text "X maximum:"  \
       -width 20
     pack  .gethist.opt.xmax.label -side left

     entry .gethist.opt.xmax.value -width 30  \
       -relief sunken  -bd 2
     pack  .gethist.opt.xmax.value  -side left -padx 2m

     pack  .gethist.opt.xmax -side top -pady 2m

#
#   Bind an action to a mouse click in the list box.  The action
#   required is to copy the selected column to the catalogue entry
#   box and then get the details for the column.

     bind .gethist.cols.list  <ButtonRelease-1>  {
        set numCol [.gethist.cols.list curselect]
        if {$numCol > -1} then {
           set chosenItem [.gethist.cols.list get $numCol]

           set bracePos [string first "\{" $chosenItem]

           if {$bracePos > -1} then {
              set bracePos [expr $bracePos - 1]

              set chosenCol [string range $chosenItem 0 $bracePos]
           } else {
              set chosenCol $chosenItem
           }

           .gethist.opt.xaxis.value configure -state normal
           .gethist.opt.xaxis.value delete 0 end
           .gethist.opt.xaxis.value insert end $chosenCol
           .gethist.opt.xaxis.value configure -state disabled

           global histXaxis
           set histXaxis $chosenCol

           focus -force .gethist

        }
     }

     bindtags .gethist.cols.list {.gethist.cols.list Listbox}

#
#   Define the options for the bin details menu.

     menu .gethist.opt.binDet.but.menu

     .gethist.opt.binDet.but.menu add command \
        -label "Bin width"  \
        -command {
          GetParam  40 "Enter bin width:" GetHistBinWidth_help

          if {$GetParamValue != ""} then {
             global localBinSpec
             set    localBinSpec "YES"

             global localBinDet
             set    localBinDet $GetParamValue

             .gethist.opt.binDet.value  configure  -state normal
             .gethist.opt.binDet.value  delete 0 end
             .gethist.opt.binDet.value  insert 0 "Bin width: "
             .gethist.opt.binDet.value  insert end $localBinDet
             .gethist.opt.binDet.value  configure  -state disabled
          }
         }

     .gethist.opt.binDet.but.menu add command \
        -label "Number of bins"  \
        -command {
          GetParam  40 "Enter number of bins:" GetHistBinNumber_help

          if {$GetParamValue != ""} then {
             global localBinSpec
             set    localBinSpec "NO"

             global localBinDet
             set    localBinDet $GetParamValue

             .gethist.opt.binDet.value  configure  -state normal
             .gethist.opt.binDet.value  delete 0 end
             .gethist.opt.binDet.value  insert 0 "Number of bins: "
             .gethist.opt.binDet.value  insert end $localBinDet
             .gethist.opt.binDet.value  configure  -state disabled
          }
         }

#
#   Define the options for the 'normalise histogram'  menu.

     menu .gethist.opt.normal.but.menu

     .gethist.opt.normal.but.menu add command \
        -label "Yes"  \
        -command {
          .gethist.opt.normal.value  configure  -state normal
          .gethist.opt.normal.value  delete 0 end
          .gethist.opt.normal.value  insert 0 "Yes"
          .gethist.opt.normal.value  configure  -state disabled

          global localNormal
          set    localNormal "YES"
         }

     .gethist.opt.normal.but.menu add command \
        -label "No"  \
        -command {
          .gethist.opt.normal.value  configure  -state normal
          .gethist.opt.normal.value  delete 0 end
          .gethist.opt.normal.value  insert 0 "No"
          .gethist.opt.normal.value  configure  -state disabled

          global localNormal
          set    localNormal  "NO"
         }

#
#   Define the options for the 'line colour' menu.

     menu .gethist.opt.lineColour.but.menu

     .gethist.opt.lineColour.but.menu add command \
        -label "Default"  \
        -command {
          .gethist.opt.lineColour.value  configure  -state normal
          .gethist.opt.lineColour.value  delete 0 end
          .gethist.opt.lineColour.value  insert 0 "Default"
          .gethist.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "DEFAULT"
         }

     .gethist.opt.lineColour.but.menu add command \
        -label "Red"  \
        -command {
          .gethist.opt.lineColour.value  configure  -state normal
          .gethist.opt.lineColour.value  delete 0 end
          .gethist.opt.lineColour.value  insert 0 "Red"
          .gethist.opt.lineColour.value  configure  -state disabled

          global localSymbolColour
          set    localLineColour "RED"
         }

     .gethist.opt.lineColour.but.menu add command \
        -label "Green"  \
        -command {
          .gethist.opt.lineColour.value  configure  -state normal
          .gethist.opt.lineColour.value  delete 0 end
          .gethist.opt.lineColour.value  insert 0 "Green"
          .gethist.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "GREEN"
         }

     .gethist.opt.lineColour.but.menu add command \
        -label "Blue"  \
        -command {
          .gethist.opt.lineColour.value  configure  -state normal
          .gethist.opt.lineColour.value  delete 0 end
          .gethist.opt.lineColour.value  insert 0 "Blue"
          .gethist.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "BLUE"
         }

     .gethist.opt.lineColour.but.menu add command \
        -label "Cyan"  \
        -command {
          .gethist.opt.lineColour.value  configure  -state normal
          .gethist.opt.lineColour.value  delete 0 end
          .gethist.opt.lineColour.value  insert 0 "Cyan"
          .gethist.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "CYAN"
         }

     .gethist.opt.lineColour.but.menu add command \
        -label "Magenta"  \
        -command {
          .gethist.opt.lineColour.value  configure  -state normal
          .gethist.opt.lineColour.value  delete 0 end
          .gethist.opt.lineColour.value  insert 0 "Magenta"
          .gethist.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "MAGENTA"
         }

     .gethist.opt.lineColour.but.menu add command \
        -label "Yellow"  \
        -command {
          .gethist.opt.lineColour.value  configure  -state normal
          .gethist.opt.lineColour.value  delete 0 end
          .gethist.opt.lineColour.value  insert 0 "Yellow"
          .gethist.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "YELLOW"
         }

#
#   Define the options for the 'auto-scale'  menu.

     menu .gethist.opt.autoScale.but.menu

     .gethist.opt.autoScale.but.menu add command \
        -label "Yes"  \
        -command {
          .gethist.opt.autoScale.value  configure  -state normal
          .gethist.opt.autoScale.value  delete 0 end
          .gethist.opt.autoScale.value  insert 0 "Yes"
          .gethist.opt.autoScale.value  configure  -state disabled

          global localAutoScale
          set    localAutoScale "YES"

          .gethist.opt.xmin.value configure  -state disabled
          .gethist.opt.xmax.value configure  -state disabled
         }

     .gethist.opt.autoScale.but.menu add command \
        -label "No"  \
        -command {
          .gethist.opt.autoScale.value  configure  -state normal
          .gethist.opt.autoScale.value  delete 0 end
          .gethist.opt.autoScale.value  insert 0 "No"
          .gethist.opt.autoScale.value  configure  -state disabled

          global localAutoScale
          set    localAutoScale "NO"

          .gethist.opt.xmin.value configure  -state normal
          .gethist.opt.xmax.value configure  -state normal
         }

#
#   Pack the frame for the entry boxes and labels into the window.

     pack .gethist.opt  -side left  -padx 3m

#
#   Create a frame to hold the row of buttons.

     frame .gethist.buttonrow

#
#   Create each of the buttons.
#
#   OK.

     button .gethist.buttonrow.ok \
       -text OK \
       -width 6 \
       -command {set getHistButton "ok"}

#
#   Cancel.

     button .gethist.buttonrow.can \
       -text Cancel \
       -width 6 \
       -command {set getHistButton "can"}

#
#   Help.

     button .gethist.buttonrow.help \
       -text Help \
       -width 6 \
       -command {HelpText GetHist_help}

#
#   Pack the buttons into their enclosing frame with a default border around
#   the OK button.

     frame .gethist.buttonrow.default -relief sunken -bd 1
     raise .gethist.buttonrow.ok .gethist.buttonrow.default

     pack  .gethist.buttonrow.ok -in .gethist.buttonrow.default \
       -side top  -padx 1m -pady 1m -ipadx 1m

     pack .gethist.buttonrow.default  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .gethist.buttonrow.can   \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .gethist.buttonrow.help  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

#
#   Pack the frame of buttons into the window.

     pack .gethist.buttonrow  -side left

#
#   Withdraw the window, then update all the geometry information
#   to determine how big the window wants to be, then centre the window
#   in parent and de-iconify it.

     wm withdraw .gethist
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .gethist]/2 + \
       [winfo x .]]
     set y [expr [winfo height .]/2 - [winfo reqheight .gethist]/2 + \
       [winfo y .]]
     wm geom .gethist +$x+$y
     wm deiconify .gethist

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab  .gethist
     focus .gethist

#
#   Set the defaults for the return values for the options.

#   ...title.

     .gethist.opt.title.value delete 0 end
     .gethist.opt.title.value insert 0 $histTitle

#   ... X axis.

     .gethist.opt.xaxis.value delete 0 end
     .gethist.opt.xaxis.value insert end $histXaxis

#   ... X minimum.

     .gethist.opt.xmin.value configure  -state normal
     .gethist.opt.xmin.value delete 0 end
     .gethist.opt.xmin.value insert end [string trim $histXmin "a"]

#   ... X maximum.

     .gethist.opt.xmax.value configure  -state normal
     .gethist.opt.xmax.value delete 0 end
     .gethist.opt.xmax.value insert end [string trim $histXmax "a"]

#    ... bin specification and details.

      set localBinSpec $histBinSpec
      set localBinDet  $histBinDet

      .gethist.opt.binDet.value  configure  -state normal
      .gethist.opt.binDet.value  delete 0 end
      if {$localBinSpec == "YES"} then {
         .gethist.opt.binDet.value  insert 0 "Bin width: "
      } else {
         .gethist.opt.binDet.value  insert 0 "Number of bins: "
      }
      .gethist.opt.binDet.value  insert end $localBinDet
      .gethist.opt.binDet.value  configure  -state disabled

#    ... normalisation.

      .gethist.opt.normal.value  configure  -state normal
      .gethist.opt.normal.value  delete 0 end
      if {$histNormal == "YES"} then {
         .gethist.opt.normal.value  insert 0 "Yes"
      } else {
         .gethist.opt.normal.value  insert 0 "No"
      }
      .gethist.opt.normal.value  configure  -state disabled

      set localNormal $histNormal

#    ... line colour.

      .gethist.opt.lineColour.value  configure  -state normal
      .gethist.opt.lineColour.value  delete 0 end
      if {$histLineColour == "DEFAULT"} then {
         .gethist.opt.lineColour.value  insert 0 "Default"
      } elseif {$histLineColour == "RED"} then {
         .gethist.opt.lineColour.value  insert 0 "Red"
      } elseif {$histLineColour == "GREEN"} then {
         .gethist.opt.lineColour.value  insert 0 "Green"
      } elseif {$histLineColour == "BLUE"} then {
         .gethist.opt.lineColour.value  insert 0 "Blue"
      } elseif {$histLineColour == "CYAN"} then {
         .gethist.opt.lineColour.value  insert 0 "Cyan"
      } elseif {$histLineColour == "MAGENTA"} then {
         .gethist.opt.lineColour.value  insert 0 "Magenta"
      } else {
         .gethist.opt.lineColour.value  insert 0 "Yellow"
      }
      .gethist.opt.lineColour.value  configure  -state disabled

      set localLineColour $histLineColour

#    ... Auto-scale flag.

      .gethist.opt.autoScale.value  configure  -state normal
      .gethist.opt.autoScale.value  delete 0 end
      if {$histAutoScale == "YES"} then {
         .gethist.opt.autoScale.value  insert 0 "Yes"

         .gethist.opt.xmin.value configure  -state disabled
         .gethist.opt.xmax.value configure  -state disabled

      } else {
         .gethist.opt.autoScale.value  insert 0 "No"
      }
      .gethist.opt.autoScale.value  configure  -state disabled

      set localAutoScale $histAutoScale

#
#   Wait for the "OK" button to be pushed.

     tkwait variable getHistButton

     if {$getHistButton == "ok"} then {
        set histFlag         "T"

        set histTitle        [.gethist.opt.title.value get]
        if {$histTitle == ""} then {
           set histTitle "NONE"
        }

        set histXaxis        [.gethist.opt.xaxis.value get]
        set histLineColour   $localLineColour
        set histBinSpec      $localBinSpec
        set histBinDet       $localBinDet
        set histNormal       $localNormal
        set histAutoScale    $localAutoScale

        set histXmin         "a"
        append histXmin      [.gethist.opt.xmin.value get]
        set histXmax         "a"
        append histXmax      [.gethist.opt.xmax.value get]

        if {$histXmin == "a"} then {
           set histXmin "a0.0"
        }
        if {$histXmax == "a"} then {
           set histXmax "a0.0"
        }

#
#      Check that the axes have been defined.

        if {($histXaxis == "" )} then {
           set histFlag         "F"

           Error "X Axes not defined; cannot draw histogram."
        }
     }

     if {$getHistButton == "can"} then {
       set histFlag         "F"
     }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .gethist
     focus    $oldFocus

}
