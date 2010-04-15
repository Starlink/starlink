proc GetScatterOver selectionList {

#+ GetScatterOver
#
#  Get details for an overlay scatterplot.
#
#  Arguments:
#    selectionList (Given)
#      A list of all the selections currently defined.
#
#  Author:
#   A C Davenhall (Edinburgh).

#  History:
#   22/11/98 (ACD): Original version (from GetScatter).
#-

#
# Create the top level window.
    toplevel     .getscatterover   -class Dialog   -bd 10
    wm title     .getscatterover   "Scatter-Plot Overlay"
    wm iconname  .getscatterover   overlay
    wm transient .getscatterover   .

#
#  Define the global variables.
#  ---------------------------

#
#  The current selection.

    global selectionNumber

#
#  The number of the chosen selection.

    global scatterSelection

#
#  Plot symbol and colour.

    global scatterPlotSymbol
    global scatterSymbolColour

    global scatterFlag

#
#   Local copies of some of these items.

     global localPlotSymbol
     global localSymbolColour

#
#  Flag returned by clicking "OK" or "cancel" buttons.

    global GetScatterOverButton

#
#  Bind the return key to be equivalent to hitting return.

    bind .getscatterover  <Return>  {set GetScatterOverButton "ok"}
#
#  Create a list box and scroll bar for the list of columns.

    frame .getscatterover.sels

    scrollbar .getscatterover.sels.xscroll -orient horizontal \
      -command {.getscatterover.sels.list xview} \
      -relief sunken  -bd 2

    scrollbar .getscatterover.sels.yscroll -orient vertical \
      -command {.getscatterover.sels.list yview} \
      -relief sunken  -bd 2

    listbox .getscatterover.sels.list  -relief groove  -bd 2  \
      -xscroll {.getscatterover.sels.xscroll set} \
      -yscroll {.getscatterover.sels.yscroll set} \
      -width 60 -height 10  \
      -font *-courier-medium-r-normal--*-100-*-*-*-*-*-*

#
#  Pack the list box into the window.

    pack  .getscatterover.sels.yscroll  -side left    -fill y
    pack  .getscatterover.sels.xscroll  -side bottom  -fill x
    pack  .getscatterover.sels.list     -expand y     -fill both

    pack  .getscatterover.sels  -side left

#
#  Insert the list of selections into the list box.

    set numSelect [llength $selectionList]

    set counter 0

    while {$counter < $numSelect} {
       set currentSelect [lrange $selectionList $counter $counter]
       .getscatterover.sels.list insert end [join $currentSelect " "]
       set counter [expr $counter + 1]
    }

#
#  Create a frame to hold the plotting options.

    frame .getscatterover.opt

#
#  An entry box for the chosen selection number and its associated
#  caption.

    frame .getscatterover.opt.title

    label .getscatterover.opt.title.label -text "Selection:"  \
      -width 20
    pack  .getscatterover.opt.title.label -side left

    entry .getscatterover.opt.title.value -relief sunken  -bd 2 -width 15
    pack .getscatterover.opt.title.value -side top
    bind .getscatterover.opt.title.value  <Return> \
      {set GetScatterOverButton "ok"}

    pack  .getscatterover.opt.title -side top -pady 2m

#
#   Plotting symbol.

     frame .getscatterover.opt.plotSymbol

     menubutton .getscatterover.opt.plotSymbol.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Plotting Symbol"  \
       -menu .getscatterover.opt.plotSymbol.but.menu
     pack .getscatterover.opt.plotSymbol.but -side left

     entry .getscatterover.opt.plotSymbol.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getscatterover.opt.plotSymbol.value  -side left  -padx 2m
     .getscatterover.opt.plotSymbol.value  configure  -state disabled

     pack  .getscatterover.opt.plotSymbol -side top -pady 2m

#
#   Symbol colour.

     frame .getscatterover.opt.symbolColour

     menubutton .getscatterover.opt.symbolColour.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Symbol Colour"  \
       -menu .getscatterover.opt.symbolColour.but.menu
     pack .getscatterover.opt.symbolColour.but -side left

     entry .getscatterover.opt.symbolColour.value  -width 15  -relief flat \
       -highlightthickness 0
     pack  .getscatterover.opt.symbolColour.value  -side left  -padx 2m
     .getscatterover.opt.symbolColour.value  configure  -state disabled

     pack  .getscatterover.opt.symbolColour -side top -pady 2m

#
#   Pack this frame into the window.

     pack  .getscatterover.opt  -side left


#
#  Create a frame to hold the control buttons.

    frame .getscatterover.buttonFrame

#
#  Create the "OK", "cancel", and "help" buttons.

    button .getscatterover.buttonFrame.ok -text OK -width 6 \
      -command {set GetScatterOverButton "ok"}

    button .getscatterover.buttonFrame.can -text Cancel -width 6 \
      -command {set GetScatterOverButton "can"}

    button .getscatterover.buttonFrame.help -text Help -width 6 \
      -command {HelpText GetScatterOver_help}

#
#  Pack the buttons into their frame with a default border around
#  the OK button.

    frame .getscatterover.buttonFrame.default -relief sunken -bd 1
    raise .getscatterover.buttonFrame.ok .getscatterover.buttonFrame.default

    pack .getscatterover.buttonFrame.ok -in .getscatterover.buttonFrame.default \
      -padx 1m -pady 1m -ipadx 1m

    pack .getscatterover.buttonFrame.default -side top -expand 1 \
      -padx 3m -pady 2m
    pack .getscatterover.buttonFrame.can  -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .getscatterover.buttonFrame.help -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m

#
#  Pack this frame into the window.

    pack  .getscatterover.buttonFrame  -side left


#
#   Define the options for the 'plotting symbol' menu.

     menu .getscatterover.opt.plotSymbol.but.menu

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Open circle"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Open circle"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "OPENCIRCLE"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Filled circle"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Filled circle"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "FILLEDCIRCLE"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Open square"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Open square"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "OPENSQUARE"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Filled square"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Filled square"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "FILLEDSQUARE"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Open triangle"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Open triangle"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "OPENTRIANGLE"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Filled triangle"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Filled triangle"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "FILLEDTRIANGLE"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Open star"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Open star"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "OPENSTAR"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Filled star"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Filled star"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "FILLEDSTAR"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Plus"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Plus"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "PLUS"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Multiplication"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Multiplication"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "MULT"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Asterisk"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Asterisk"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "ASTERISK"
         }

     .getscatterover.opt.plotSymbol.but.menu add command \
        -label "Dot"  \
        -command {
          .getscatterover.opt.plotSymbol.value  configure  -state normal
          .getscatterover.opt.plotSymbol.value  delete 0 end
          .getscatterover.opt.plotSymbol.value  insert 0 "Dot"
          .getscatterover.opt.plotSymbol.value  configure  -state disabled

          global localPlotSymbol
          set    localPlotSymbol "DOT"
         }

#
#   Define the options for the 'symbol colour' menu.

     menu .getscatterover.opt.symbolColour.but.menu

     .getscatterover.opt.symbolColour.but.menu add command \
        -label "Default"  \
        -command {
          .getscatterover.opt.symbolColour.value  configure  -state normal
          .getscatterover.opt.symbolColour.value  delete 0 end
          .getscatterover.opt.symbolColour.value  insert 0 "Default"
          .getscatterover.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "DEFAULT"
         }

     .getscatterover.opt.symbolColour.but.menu add command \
        -label "Red"  \
        -command {
          .getscatterover.opt.symbolColour.value  configure  -state normal
          .getscatterover.opt.symbolColour.value  delete 0 end
          .getscatterover.opt.symbolColour.value  insert 0 "Red"
          .getscatterover.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "RED"
         }

     .getscatterover.opt.symbolColour.but.menu add command \
        -label "Green"  \
        -command {
          .getscatterover.opt.symbolColour.value  configure  -state normal
          .getscatterover.opt.symbolColour.value  delete 0 end
          .getscatterover.opt.symbolColour.value  insert 0 "Green"
          .getscatterover.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "GREEN"
         }

     .getscatterover.opt.symbolColour.but.menu add command \
        -label "Blue"  \
        -command {
          .getscatterover.opt.symbolColour.value  configure  -state normal
          .getscatterover.opt.symbolColour.value  delete 0 end
          .getscatterover.opt.symbolColour.value  insert 0 "Blue"
          .getscatterover.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "BLUE"
         }

     .getscatterover.opt.symbolColour.but.menu add command \
        -label "Cyan"  \
        -command {
          .getscatterover.opt.symbolColour.value  configure  -state normal
          .getscatterover.opt.symbolColour.value  delete 0 end
          .getscatterover.opt.symbolColour.value  insert 0 "Cyan"
          .getscatterover.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "CYAN"
         }

     .getscatterover.opt.symbolColour.but.menu add command \
        -label "Magenta"  \
        -command {
          .getscatterover.opt.symbolColour.value  configure  -state normal
          .getscatterover.opt.symbolColour.value  delete 0 end
          .getscatterover.opt.symbolColour.value  insert 0 "Magenta"
          .getscatterover.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "MAGENTA"
         }

     .getscatterover.opt.symbolColour.but.menu add command \
        -label "Yellow"  \
        -command {
          .getscatterover.opt.symbolColour.value  configure  -state normal
          .getscatterover.opt.symbolColour.value  delete 0 end
          .getscatterover.opt.symbolColour.value  insert 0 "Yellow"
          .getscatterover.opt.symbolColour.value  configure  -state disabled

          global localSymbolColour
          set    localSymbolColour "YELLOW"
         }

#
#  Set the defaults for the return values for the options.

#   ... selection to be plotted.

      .getscatterover.opt.title.value delete 0 end
      .getscatterover.opt.title.value insert end $selectionNumber

#   ... plotting symbol

      .getscatterover.opt.plotSymbol.value  configure  -state normal
      .getscatterover.opt.plotSymbol.value  delete 0 end
      if {$scatterPlotSymbol == "OPENCIRCLE"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Open circle"
      } elseif {$scatterPlotSymbol == "FILLEDCIRCLE"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Filled circle"
      } elseif {$scatterPlotSymbol == "OPENSQUARE"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Open square"
      } elseif {$scatterPlotSymbol == "FILLEDSQUARE"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Filled square"
      } elseif {$scatterPlotSymbol == "OPENTRIANGLE"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Open triangle"
      } elseif {$scatterPlotSymbol == "FILLEDTRIANGLE"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Filled triangle"
      } elseif {$scatterPlotSymbol == "OPENSTAR"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Open star"
      } elseif {$scatterPlotSymbol == "FILLEDSTAR"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Filled star"
      } elseif {$scatterPlotSymbol == "PLUS"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Plus"
      } elseif {$scatterPlotSymbol == "MULT"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Multiplication"
      } elseif {$scatterPlotSymbol == "ASTERISK"} then {
         .getscatterover.opt.plotSymbol.value  insert 0 "Asterisk"
      } else {
         .getscatterover.opt.plotSymbol.value  insert 0 "Dot"
      }
      .getscatterover.opt.plotSymbol.value  configure  -state disabled

      set localPlotSymbol $scatterPlotSymbol

#    ... symbol colour.

      .getscatterover.opt.symbolColour.value  configure  -state normal
      .getscatterover.opt.symbolColour.value  delete 0 end
      if {$scatterSymbolColour == "DEFAULT"} then {
         .getscatterover.opt.symbolColour.value  insert 0 "Default"
      } elseif {$scatterSymbolColour == "RED"} then {
         .getscatterover.opt.symbolColour.value  insert 0 "Red"
      } elseif {$scatterSymbolColour == "GREEN"} then {
         .getscatterover.opt.symbolColour.value  insert 0 "Green"
      } elseif {$scatterSymbolColour == "BLUE"} then {
         .getscatterover.opt.symbolColour.value  insert 0 "Blue"
      } elseif {$scatterSymbolColour == "CYAN"} then {
         .getscatterover.opt.symbolColour.value  insert 0 "Cyan"
      } elseif {$scatterSymbolColour == "MAGENTA"} then {
         .getscatterover.opt.symbolColour.value  insert 0 "Magenta"
      } else {
         .getscatterover.opt.symbolColour.value  insert 0 "Yellow"
      }
      .getscatterover.opt.symbolColour.value  configure  -state disabled

      set localSymbolColour $scatterSymbolColour

#
#  Bind a mouse click in the listbox to copy the chosen selection
#  to the entry box.

    bind .getscatterover.sels.list  <ButtonRelease-1>  {
       set numSel [.getscatterover.sels.list curselect]
       set numSel [expr $numSel - 1]

       if {$numSel > 0} then {
          .getscatterover.opt.title.value delete 0 end
          .getscatterover.opt.title.value insert 0 $numSel
       }
    }

#
#  Withdraw the window, then update all the geometry information
#  so we know how big it wants to be, then centre the window in
#  parent and de-iconify it.

    wm withdraw .getscatterover
    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .getscatterover]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .getscatterover]/2 + \
      [winfo y .]]
    wm geom .getscatterover +$x+$y
    wm deiconify .getscatterover

#  Set a grab and claim the focus.
    set oldFocus [focus]
    grab .getscatterover
    focus .getscatterover

    tkwait variable GetScatterOverButton

    if {$GetScatterOverButton == "ok"} {
       set scatterPlotSymbol   $localPlotSymbol
       set scatterSymbolColour $localSymbolColour

       set scatterSelection [.getscatterover.opt.title.value get]

       set scatterFlag         "T"
    }

    if {$GetScatterOverButton == "can"} {
       set scatterFlag         "F"
    }

# Destroy the dialogue box and restore the focus.
    destroy  .getscatterover
    focus    $oldFocus

}
