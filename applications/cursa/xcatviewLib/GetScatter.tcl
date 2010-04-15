proc GetScatter { } {

#+ GetScatter
#
#  Procedure to get the details for a scatter plot.
#
#  Given:
#    None.
#
#  Author:
#   ACD: A C Davenhall (Edinburgh)

#  History:
#   10/7/98  (ACD): Original version.
#   9/12/98  (ACD): First stable version.
#-

#
#   Create the top level window.

     toplevel     .getscatter   -class Dialog   -bd 10
     wm title     .getscatter   "Scatter-Plot"
     wm iconname  .getscatter   Plot
     wm transient .getscatter   .

#
#   Declare the global variables.
#   ----------------------------

#
#   The state returned by clicking on the "OK" or "Cancel" buttons.

     global getScatterButton

#
#   The details of the column returned by the A-task.

     global catalogueList

#
#   The current list of columns for display.

     global chosenColumns

#
#   The options for the various items specifying the plot.

     global scatterFlag
     global scatterTitle
     global scatterXaxis
     global scatterYaxis
     global scatterPlotSymbol
     global scatterSymbolColour
     global scatterAutoScale
     global scatterXmin
     global scatterXmax
     global scatterYmin
     global scatterYmax

#
#   Local copies of some of these items.

     global localPlotSymbol
     global localSymbolColour
     global localAutoScale

#
#   Bind the return key to set global variable button to "ok".  That is,
#   hitting return will have the same effect as clicking on the "ok"
#   button.

     bind  .getscatter  <Return> {set getScatterButton "ok" }

#
#   Create a frame to hold the listbox, create the listbox and its
#   associated scroll bars.

     frame  .getscatter.cols

     scrollbar .getscatter.cols.xscroll  -orient horizontal \
       -command {.getscatter.cols.list  xview} \
       -relief sunken  -bd 2

     scrollbar .getscatter.cols.yscroll  -orient vertical \
       -command {.getscatter.cols.list  yview} \
       -relief sunken  -bd 2

     listbox .getscatter.cols.list  -relief groove  -bd 2  \
       -width 16 -height 7  \
       -xscroll {.getscatter.cols.xscroll  set}  \
       -yscroll {.getscatter.cols.yscroll  set}

#
#   Pack the listbox and scroll bars into their frame and pack the
#   frame into the window.

     pack .getscatter.cols.yscroll  -side left    -fill y
     pack .getscatter.cols.xscroll  -side bottom  -fill x
     pack .getscatter.cols.list     -expand yes   -fill both

     pack .getscatter.cols  -side left  -padx 3m  -fill y

#
#   Insert the list of columns on which range selections may be
#   performed into the list box.  The default is all the columns.

     set numColumns [llength $catalogueList]

     set counter 0

     while {$counter < $numColumns} {
        set currentColumn [lrange $catalogueList $counter $counter]
        .getscatter.cols.list insert end $currentColumn
        set counter [expr $counter + 1]
     }

#
#   Create a frame to hold the entry boxes and their labels, then
#   create these boxes and labels and pack them into their frame.

     frame .getscatter.opt

#
#   Title.

     frame .getscatter.opt.title

     label .getscatter.opt.title.label -text "Title:"  \
       -width 10
     pack  .getscatter.opt.title.label -side left

     entry .getscatter.opt.title.value -width 30  \
       -relief sunken  -bd 2
     pack  .getscatter.opt.title.value  -side left -padx 2m

     pack  .getscatter.opt.title -side top -pady 2m

#
#   X axis.

     frame .getscatter.opt.xaxis

     button .getscatter.opt.xaxis.but -text "X axis:"  \
       -width 10 \
       -command {set numCol [.getscatter.cols.list curselect]
           if {$numCol > -1} then {
              set chosenItem [.getscatter.cols.list get $numCol]

              set bracePos [string first "\{" $chosenItem]

              if {$bracePos > -1} then {
                 set bracePos [expr $bracePos - 1]

                 set chosenCol [string range $chosenItem 0 $bracePos]
              } else {
                 set chosenCol $chosenItem
              }

              .getscatter.opt.xaxis.value delete 0 end
              .getscatter.opt.xaxis.value insert end $chosenCol
           } else {
              Error "Click on a column name before clicking this button."
           }
        }
     pack  .getscatter.opt.xaxis.but -side left

     entry .getscatter.opt.xaxis.value -width 30  \
       -relief sunken  -bd 2
     pack  .getscatter.opt.xaxis.value  -side left -padx 2m

     pack  .getscatter.opt.xaxis -side top -pady 2m

#
#   Y axis.

     frame .getscatter.opt.yaxis

     button .getscatter.opt.yaxis.but -text "Y axis:"  \
       -width 10 \
       -command {set numCol [.getscatter.cols.list curselect]
           if {$numCol > -1} then {
              set chosenItem [.getscatter.cols.list get $numCol]

              set bracePos [string first "\{" $chosenItem]

              if {$bracePos > -1} then {
                 set bracePos [expr $bracePos - 1]

                 set chosenCol [string range $chosenItem 0 $bracePos]
              } else {
                 set chosenCol $chosenItem
              }

              .getscatter.opt.yaxis.value delete 0 end
              .getscatter.opt.yaxis.value insert end $chosenCol
           } else {
              Error "Click on a column name before clicking this button."
           }
        }
     pack  .getscatter.opt.yaxis.but -side left

     entry .getscatter.opt.yaxis.value -width 30  \
       -relief sunken  -bd 2
     pack  .getscatter.opt.yaxis.value  -side left -padx 2m

     pack  .getscatter.opt.yaxis -side top -pady 2m

#
#   Plotting symbol.

     frame .getscatter.opt.plotSymbol

     menubutton .getscatter.opt.plotSymbol.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Plotting Symbol"  \
       -menu .getscatter.opt.plotSymbol.but.menu
     pack .getscatter.opt.plotSymbol.but -side left

     entry .getscatter.opt.plotSymbol.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getscatter.opt.plotSymbol.value  -side left  -padx 2m
     .getscatter.opt.plotSymbol.value  configure  -state disabled

     pack  .getscatter.opt.plotSymbol -side top -pady 2m

#
#   Symbol colour.

     frame .getscatter.opt.symbolColour

     menubutton .getscatter.opt.symbolColour.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Symbol Colour"  \
       -menu .getscatter.opt.symbolColour.but.menu
     pack .getscatter.opt.symbolColour.but -side left

     entry .getscatter.opt.symbolColour.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getscatter.opt.symbolColour.value  -side left  -padx 2m
     .getscatter.opt.symbolColour.value  configure  -state disabled

     pack  .getscatter.opt.symbolColour -side top -pady 2m

#
#   Auto-scale flag.

     frame .getscatter.opt.autoScale

     menubutton .getscatter.opt.autoScale.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Auto-scale?"  \
       -menu .getscatter.opt.autoScale.but.menu
     pack .getscatter.opt.autoScale.but -side left

     entry .getscatter.opt.autoScale.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getscatter.opt.autoScale.value  -side left  -padx 2m
     .getscatter.opt.autoScale.value  configure  -state disabled

     pack  .getscatter.opt.autoScale -side top -pady 2m

#
#   X-minimum.

     frame .getscatter.opt.xmin

     label .getscatter.opt.xmin.label -text "X minimum:"  \
       -width 20
     pack  .getscatter.opt.xmin.label -side left

     entry .getscatter.opt.xmin.value -width 30  \
       -relief sunken  -bd 2
     pack  .getscatter.opt.xmin.value  -side left -padx 2m

     pack  .getscatter.opt.xmin -side top -pady 2m

#
#   X-maximum.

     frame .getscatter.opt.xmax

     label .getscatter.opt.xmax.label -text "X maximum:"  \
       -width 20
     pack  .getscatter.opt.xmax.label -side left

     entry .getscatter.opt.xmax.value -width 30  \
       -relief sunken  -bd 2
     pack  .getscatter.opt.xmax.value  -side left -padx 2m

     pack  .getscatter.opt.xmax -side top -pady 2m

#
#   Y-minimum.

     frame .getscatter.opt.ymin

     label .getscatter.opt.ymin.label -text "Y minimum:"  \
       -width 20
     pack  .getscatter.opt.ymin.label -side left

     entry .getscatter.opt.ymin.value -width 30  \
       -relief sunken  -bd 2
     pack  .getscatter.opt.ymin.value  -side left -padx 2m

     pack  .getscatter.opt.ymin -side top -pady 2m

#
#   Y-maximum

     frame .getscatter.opt.ymax

     label .getscatter.opt.ymax.label -text "Y maximum:"  \
       -width 20
     pack  .getscatter.opt.ymax.label -side left

     entry .getscatter.opt.ymax.value -width 30  \
       -relief sunken  -bd 2
     pack  .getscatter.opt.ymax.value  -side left -padx 2m

     pack  .getscatter.opt.ymax -side top -pady 2m

#
#   Define the options for the 'plotting symbol' menu.

     menu .getscatter.opt.plotSymbol.but.menu

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Open circle"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Open circle"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "OPENCIRCLE"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Filled circle"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Filled circle"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "FILLEDCIRCLE"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Open square"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Open square"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "OPENSQUARE"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Filled square"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Filled square"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "FILLEDSQUARE"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Open triangle"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Open triangle"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "OPENTRIANGLE"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Filled triangle"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Filled triangle"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "FILLEDTRIANGLE"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Open star"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Open star"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "OPENSTAR"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Filled star"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Filled star"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "FILLEDSTAR"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Plus"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Plus"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "PLUS"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Multiplication"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Multiplication"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "MULT"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Asterisk"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Asterisk"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "ASTERISK"
         }

     .getscatter.opt.plotSymbol.but.menu add command \
        -label "Dot"  \
        -command {
          .getscatter.opt.plotSymbol.value  configure  -state normal
          .getscatter.opt.plotSymbol.value  delete 0 end
          .getscatter.opt.plotSymbol.value  insert 0 "Dot"
          .getscatter.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "DOT"
         }

#
#   Define the options for the 'symbol colour' menu.

     menu .getscatter.opt.symbolColour.but.menu

     .getscatter.opt.symbolColour.but.menu add command \
        -label "Default"  \
        -command {
          .getscatter.opt.symbolColour.value  configure  -state normal
          .getscatter.opt.symbolColour.value  delete 0 end
          .getscatter.opt.symbolColour.value  insert 0 "Default"
          .getscatter.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "DEFAULT"
         }

     .getscatter.opt.symbolColour.but.menu add command \
        -label "Red"  \
        -command {
          .getscatter.opt.symbolColour.value  configure  -state normal
          .getscatter.opt.symbolColour.value  delete 0 end
          .getscatter.opt.symbolColour.value  insert 0 "Red"
          .getscatter.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "RED"
         }

     .getscatter.opt.symbolColour.but.menu add command \
        -label "Green"  \
        -command {
          .getscatter.opt.symbolColour.value  configure  -state normal
          .getscatter.opt.symbolColour.value  delete 0 end
          .getscatter.opt.symbolColour.value  insert 0 "Green"
          .getscatter.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "GREEN"
         }

     .getscatter.opt.symbolColour.but.menu add command \
        -label "Blue"  \
        -command {
          .getscatter.opt.symbolColour.value  configure  -state normal
          .getscatter.opt.symbolColour.value  delete 0 end
          .getscatter.opt.symbolColour.value  insert 0 "Blue"
          .getscatter.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "BLUE"
         }

     .getscatter.opt.symbolColour.but.menu add command \
        -label "Cyan"  \
        -command {
          .getscatter.opt.symbolColour.value  configure  -state normal
          .getscatter.opt.symbolColour.value  delete 0 end
          .getscatter.opt.symbolColour.value  insert 0 "Cyan"
          .getscatter.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "CYAN"
         }

     .getscatter.opt.symbolColour.but.menu add command \
        -label "Magenta"  \
        -command {
          .getscatter.opt.symbolColour.value  configure  -state normal
          .getscatter.opt.symbolColour.value  delete 0 end
          .getscatter.opt.symbolColour.value  insert 0 "Magenta"
          .getscatter.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "MAGENTA"
         }

     .getscatter.opt.symbolColour.but.menu add command \
        -label "Yellow"  \
        -command {
          .getscatter.opt.symbolColour.value  configure  -state normal
          .getscatter.opt.symbolColour.value  delete 0 end
          .getscatter.opt.symbolColour.value  insert 0 "Yellow"
          .getscatter.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "YELLOW"
         }

#
#   Define the options for the 'auto-scale'  menu.

     menu .getscatter.opt.autoScale.but.menu

     .getscatter.opt.autoScale.but.menu add command \
        -label "Yes"  \
        -command {
          .getscatter.opt.autoScale.value  configure  -state normal
          .getscatter.opt.autoScale.value  delete 0 end
          .getscatter.opt.autoScale.value  insert 0 "Yes"
          .getscatter.opt.autoScale.value  configure  -state disabled

          global localAutoScale
          set    localAutoScale "YES"

          .getscatter.opt.xmin.value configure  -state disabled
          .getscatter.opt.xmax.value configure  -state disabled
          .getscatter.opt.ymin.value configure  -state disabled
          .getscatter.opt.ymax.value configure  -state disabled
         }

     .getscatter.opt.autoScale.but.menu add command \
        -label "No"  \
        -command {
          .getscatter.opt.autoScale.value  configure  -state normal
          .getscatter.opt.autoScale.value  delete 0 end
          .getscatter.opt.autoScale.value  insert 0 "No"
          .getscatter.opt.autoScale.value  configure  -state disabled

          global localAutoScale
          set    localAutoScale "NO"

          .getscatter.opt.xmin.value configure  -state normal
          .getscatter.opt.xmax.value configure  -state normal
          .getscatter.opt.ymin.value configure  -state normal
          .getscatter.opt.ymax.value configure  -state normal
         }

#
#   Pack the frame for the entry boxes and labels into the window.

     pack .getscatter.opt  -side left  -padx 3m


#
#   Bind an action to a mouse click in the list box.  The action
#   required is to copy the selected column to the catalogue entry
#   box and then get the details for the column.

     bind .getscatter.cols.list  <ButtonRelease-1>  {
        set numCol [.getscatter.cols.list curselect]
        if {$numCol > -1} then {
           set chosenItem [.getscatter.cols.list get $numCol]

           set bracePos [string first "\{" $chosenItem]

           if {$bracePos > -1} then {
              set bracePos [expr $bracePos - 1]

              set chosenCol [string range $chosenItem 0 $bracePos]
           } else {
              set chosenCol $chosenItem
           }
        }
     }

     bindtags .getscatter.cols.list {.getscatter.cols.list Listbox}

#
#   Create a frame to hold the row of buttons.

     frame .getscatter.buttonrow

#
#   Create each of the buttons.
#
#   OK.

     button .getscatter.buttonrow.ok \
       -text OK \
       -width 6 \
       -command {set getScatterButton "ok"}

#
#   Cancel.

     button .getscatter.buttonrow.can \
       -text Cancel \
       -width 6 \
       -command {set getScatterButton "can"}

#
#   Help.

     button .getscatter.buttonrow.help \
       -text Help \
       -width 6 \
       -command {HelpText GetScatter_help}

#
#   Pack the buttons into their enclosing frame with a default border around
#   the OK button.

     frame .getscatter.buttonrow.default -relief sunken -bd 1
     raise .getscatter.buttonrow.ok .getscatter.buttonrow.default

     pack  .getscatter.buttonrow.ok -in .getscatter.buttonrow.default \
       -side top  -padx 1m -pady 1m -ipadx 1m

     pack .getscatter.buttonrow.default  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getscatter.buttonrow.can   \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getscatter.buttonrow.help  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

#
#   Pack the frame of buttons into the window.

     pack .getscatter.buttonrow  -side left

#
#   Withdraw the window, then update all the geometry information
#   to determine how big the window wants to be, then centre the window
#   in parent and de-iconify it.

     wm withdraw .getscatter
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .getscatter]/2 + \
       [winfo x .]]
     set y [expr [winfo height .]/2 - [winfo reqheight .getscatter]/2 + \
       [winfo y .]]
     wm geom .getscatter +$x+$y
     wm deiconify .getscatter

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab  .getscatter
     focus .getscatter

#
#   Set the defaults for the return values for the options.

#   ...title.

     .getscatter.opt.title.value delete 0 end
     .getscatter.opt.title.value insert 0 $scatterTitle

#   ... X axis.

     .getscatter.opt.xaxis.value delete 0 end
     .getscatter.opt.xaxis.value insert end $scatterXaxis

#   ... Y axis.

     .getscatter.opt.yaxis.value delete 0 end
     .getscatter.opt.yaxis.value insert end $scatterYaxis

#   ... X minimum.

     .getscatter.opt.xmin.value configure  -state normal
     .getscatter.opt.xmin.value delete 0 end
     .getscatter.opt.xmin.value insert end [string trim $scatterXmin "a"]

#   ... X maximum.

     .getscatter.opt.xmax.value configure  -state normal
     .getscatter.opt.xmax.value delete 0 end
     .getscatter.opt.xmax.value insert end [string trim $scatterXmax "a"]

#   ... Y minimum.

     .getscatter.opt.ymin.value configure  -state normal
     .getscatter.opt.ymin.value delete 0 end
     .getscatter.opt.ymin.value insert end [string trim $scatterYmin "a"]

#   ... Y maximum.

     .getscatter.opt.ymax.value configure  -state normal
     .getscatter.opt.ymax.value delete 0 end
     .getscatter.opt.ymax.value insert end [string trim $scatterYmax "a"]

#   ... plotting symbol

      .getscatter.opt.plotSymbol.value  configure  -state normal
      .getscatter.opt.plotSymbol.value  delete 0 end
      if {$scatterPlotSymbol == "OPENCIRCLE"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Open circle"
      } elseif {$scatterPlotSymbol == "FILLEDCIRCLE"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Filled circle"
      } elseif {$scatterPlotSymbol == "OPENSQUARE"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Open square"
      } elseif {$scatterPlotSymbol == "FILLEDSQUARE"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Filled square"
      } elseif {$scatterPlotSymbol == "OPENTRIANGLE"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Open triangle"
      } elseif {$scatterPlotSymbol == "FILLEDTRIANGLE"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Filled triangle"
      } elseif {$scatterPlotSymbol == "OPENSTAR"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Open star"
      } elseif {$scatterPlotSymbol == "FILLEDSTAR"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Filled star"
      } elseif {$scatterPlotSymbol == "PLUS"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Plus"
      } elseif {$scatterPlotSymbol == "MULT"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Multiplication"
      } elseif {$scatterPlotSymbol == "ASTERISK"} then {
         .getscatter.opt.plotSymbol.value  insert 0 "Asterisk"
      } else {
         .getscatter.opt.plotSymbol.value  insert 0 "Dot"
      }
      .getscatter.opt.plotSymbol.value  configure  -state disabled

      set localPlotSymbol $scatterPlotSymbol

#    ... symbol colour.

      .getscatter.opt.symbolColour.value  configure  -state normal
      .getscatter.opt.symbolColour.value  delete 0 end
      if {$scatterSymbolColour == "DEFAULT"} then {
         .getscatter.opt.symbolColour.value  insert 0 "Default"
      } elseif {$scatterSymbolColour == "RED"} then {
         .getscatter.opt.symbolColour.value  insert 0 "Red"
      } elseif {$scatterSymbolColour == "GREEN"} then {
         .getscatter.opt.symbolColour.value  insert 0 "Green"
      } elseif {$scatterSymbolColour == "BLUE"} then {
         .getscatter.opt.symbolColour.value  insert 0 "Blue"
      } elseif {$scatterSymbolColour == "CYAN"} then {
         .getscatter.opt.symbolColour.value  insert 0 "Cyan"
      } elseif {$scatterSymbolColour == "MAGENTA"} then {
         .getscatter.opt.symbolColour.value  insert 0 "Magenta"
      } else {
         .getscatter.opt.symbolColour.value  insert 0 "Yellow"
      }
      .getscatter.opt.symbolColour.value  configure  -state disabled

      set localSymbolColour $scatterSymbolColour

#    ... Auto-scale flag.

      .getscatter.opt.autoScale.value  configure  -state normal
      .getscatter.opt.autoScale.value  delete 0 end
      if {$scatterAutoScale == "YES"} then {
         .getscatter.opt.autoScale.value  insert 0 "Yes"

         .getscatter.opt.xmin.value configure  -state disabled
         .getscatter.opt.xmax.value configure  -state disabled
         .getscatter.opt.ymin.value configure  -state disabled
         .getscatter.opt.ymax.value configure  -state disabled

      } else {
         .getscatter.opt.autoScale.value  insert 0 "No"
      }
      .getscatter.opt.autoScale.value  configure  -state disabled

      set localAutoScale $scatterAutoScale

#
#   Wait for the "OK" button to be pushed.

     tkwait variable getScatterButton

     if {$getScatterButton == "ok"} then {
        set scatterFlag         "T"

        set scatterTitle        [.getscatter.opt.title.value get]
        if {$scatterTitle == ""} then {
           set scatterTitle "NONE"
        }

        set scatterXaxis        [.getscatter.opt.xaxis.value get]
        set scatterYaxis        [.getscatter.opt.yaxis.value get]
        set scatterPlotSymbol   $localPlotSymbol
        set scatterSymbolColour $localSymbolColour
        set scatterAutoScale    $localAutoScale

        set scatterXmin         "a"
        append scatterXmin      [.getscatter.opt.xmin.value get]
        set scatterXmax         "a"
        append scatterXmax      [.getscatter.opt.xmax.value get]
        set scatterYmin         "a"
        append scatterYmin      [.getscatter.opt.ymin.value get]
        set scatterYmax         "a"
        append scatterYmax      [.getscatter.opt.ymax.value get]

        if {$scatterXmin == "a"} then {
           set scatterXmin "a0.0"
        }
        if {$scatterXmax == "a"} then {
           set scatterXmax "a0.0"
        }
        if {$scatterYmin == "a"} then {
           set scatterYmin "a0.0"
        }
        if {$scatterYmax == "a"} then {
           set scatterYmax "a0.0"
        }

#
#      Check that the axes have been defined.

        if {($scatterXaxis == "" ) || ($scatterYaxis == "" )} then {
           set scatterFlag         "F"

           Error "Axes not defined; cannot draw plot."
        }
     }

     if {$getScatterButton == "can"} then {
       set scatterFlag         "F"
     }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .getscatter
     focus    $oldFocus

}
