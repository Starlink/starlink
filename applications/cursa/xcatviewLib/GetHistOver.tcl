proc GetHistOver selectionList {

#+ GetHistOver
#
#  Get details for an overlay histogram.
#
#  Arguments:
#    selectionList (Given)
#      A list of all the selections currently defined.
#
#  Author:
#   A C Davenhall (Edinburgh).

#  History:
#   19/11/99 (ACD): Original version (from GetScatterOver).
#-

#
# Create the top level window.
    toplevel     .gethistover   -class Dialog   -bd 10
    wm title     .gethistover   "Histogram Overlay"
    wm iconname  .gethistover   overlay
    wm transient .gethistover   .

#
#  Define the global variables.
#  ---------------------------

#
#  The current selection.

    global selectionNumber

#
#  The number of the chosen selection.

    global histSelection

#
#  Histogram colour.

    global histLineColour

    global histFlag

#
#   Local copies of some of these items.

     global localLineColour

#
#  Flag returned by clicking "OK" or "cancel" buttons.

    global GetHistOverButton

#
#  Bind the return key to be equivalent to hitting return.

    bind .gethistover  <Return>  {set GetHistOverButton "ok"}
#
#  Create a list box and scroll bar for the list of columns.

    frame .gethistover.sels

    scrollbar .gethistover.sels.xscroll -orient horizontal \
      -command {.gethistover.sels.list xview} \
      -relief sunken  -bd 2

    scrollbar .gethistover.sels.yscroll -orient vertical \
      -command {.gethistover.sels.list yview} \
      -relief sunken  -bd 2

    listbox .gethistover.sels.list  -relief groove  -bd 2  \
      -xscroll {.gethistover.sels.xscroll set} \
      -yscroll {.gethistover.sels.yscroll set} \
      -width 60 -height 10  \
      -font *-courier-medium-r-normal--*-100-*-*-*-*-*-*

#
#  Pack the list box into the window.

    pack  .gethistover.sels.yscroll  -side left    -fill y
    pack  .gethistover.sels.xscroll  -side bottom  -fill x
    pack  .gethistover.sels.list     -expand y     -fill both

    pack  .gethistover.sels  -side left

#
#  Insert the list of selections into the list box.

    set numSelect [llength $selectionList]

    set counter 0

    while {$counter < $numSelect} {
       set currentSelect [lrange $selectionList $counter $counter]
       .gethistover.sels.list insert end [join $currentSelect " "]
       set counter [expr $counter + 1]
    }

#
#  Create a frame to hold the plotting options.

    frame .gethistover.opt

#
#  An entry box for the chosen selection number and its associated
#  caption.

    frame .gethistover.opt.title

    label .gethistover.opt.title.label -text "Selection:"  \
      -width 20
    pack  .gethistover.opt.title.label -side left

    entry .gethistover.opt.title.value -relief sunken  -bd 2 -width 15
    pack .gethistover.opt.title.value -side top
    bind .gethistover.opt.title.value  <Return> \
      {set GetHistOverButton "ok"}

    pack  .gethistover.opt.title -side top -pady 2m

#
#   Line colour.

     frame .gethistover.opt.lineColour

     menubutton .gethistover.opt.lineColour.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Line Colour"  \
       -menu .gethistover.opt.lineColour.but.menu
     pack .gethistover.opt.lineColour.but -side left

     entry .gethistover.opt.lineColour.value  -width 15  -relief flat \
       -highlightthickness 0
     pack  .gethistover.opt.lineColour.value  -side left  -padx 2m
     .gethistover.opt.lineColour.value  configure  -state disabled

     pack  .gethistover.opt.lineColour -side top -pady 2m

#
#   Pack this frame into the window.

     pack  .gethistover.opt  -side left


#
#  Create a frame to hold the control buttons.

    frame .gethistover.buttonFrame

#
#  Create the "OK", "cancel", and "help" buttons.

    button .gethistover.buttonFrame.ok -text OK -width 6 \
      -command {set GetHistOverButton "ok"}

    button .gethistover.buttonFrame.can -text Cancel -width 6 \
      -command {set GetHistOverButton "can"}

    button .gethistover.buttonFrame.help -text Help -width 6 \
      -command {HelpText GetHistOver_help}

#
#  Pack the buttons into their frame with a default border around
#  the OK button.

    frame .gethistover.buttonFrame.default -relief sunken -bd 1
    raise .gethistover.buttonFrame.ok .gethistover.buttonFrame.default

    pack .gethistover.buttonFrame.ok -in .gethistover.buttonFrame.default \
      -padx 1m -pady 1m -ipadx 1m

    pack .gethistover.buttonFrame.default -side top -expand 1 \
      -padx 3m -pady 2m
    pack .gethistover.buttonFrame.can  -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m
    pack .gethistover.buttonFrame.help -side top -expand 1 \
      -padx 3m -pady 2m -ipadx 1m

#
#  Pack this frame into the window.

    pack  .gethistover.buttonFrame  -side left


#
#   Define the options for the 'line colour' menu.

     menu .gethistover.opt.lineColour.but.menu

     .gethistover.opt.lineColour.but.menu add command \
        -label "Default"  \
        -command {
          .gethistover.opt.lineColour.value  configure  -state normal
          .gethistover.opt.lineColour.value  delete 0 end
          .gethistover.opt.lineColour.value  insert 0 "Default"
          .gethistover.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "DEFAULT"
         }

     .gethistover.opt.lineColour.but.menu add command \
        -label "Red"  \
        -command {
          .gethistover.opt.lineColour.value  configure  -state normal
          .gethistover.opt.lineColour.value  delete 0 end
          .gethistover.opt.lineColour.value  insert 0 "Red"
          .gethistover.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "RED"
         }

     .gethistover.opt.lineColour.but.menu add command \
        -label "Green"  \
        -command {
          .gethistover.opt.lineColour.value  configure  -state normal
          .gethistover.opt.lineColour.value  delete 0 end
          .gethistover.opt.lineColour.value  insert 0 "Green"
          .gethistover.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "GREEN"
         }

     .gethistover.opt.lineColour.but.menu add command \
        -label "Blue"  \
        -command {
          .gethistover.opt.lineColour.value  configure  -state normal
          .gethistover.opt.lineColour.value  delete 0 end
          .gethistover.opt.lineColour.value  insert 0 "Blue"
          .gethistover.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "BLUE"
         }

     .gethistover.opt.lineColour.but.menu add command \
        -label "Cyan"  \
        -command {
          .gethistover.opt.lineColour.value  configure  -state normal
          .gethistover.opt.lineColour.value  delete 0 end
          .gethistover.opt.lineColour.value  insert 0 "Cyan"
          .gethistover.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "CYAN"
         }

     .gethistover.opt.lineColour.but.menu add command \
        -label "Magenta"  \
        -command {
          .gethistover.opt.lineColour.value  configure  -state normal
          .gethistover.opt.lineColour.value  delete 0 end
          .gethistover.opt.lineColour.value  insert 0 "Magenta"
          .gethistover.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "MAGENTA"
         }

     .gethistover.opt.lineColour.but.menu add command \
        -label "Yellow"  \
        -command {
          .gethistover.opt.lineColour.value  configure  -state normal
          .gethistover.opt.lineColour.value  delete 0 end
          .gethistover.opt.lineColour.value  insert 0 "Yellow"
          .gethistover.opt.lineColour.value  configure  -state disabled

          global localLineColour
          set    localLineColour "YELLOW"
         }

#
#  Set the defaults for the return values for the options.

#   ... selection to be plotted.

      .gethistover.opt.title.value delete 0 end
      .gethistover.opt.title.value insert end $selectionNumber

#    ... symbol colour.

      .gethistover.opt.lineColour.value  configure  -state normal
      .gethistover.opt.lineColour.value  delete 0 end
      if {$histLineColour == "DEFAULT"} then {
         .gethistover.opt.lineColour.value  insert 0 "Default"
      } elseif {$histLineColour == "RED"} then {
         .gethistover.opt.lineColour.value  insert 0 "Red"
      } elseif {$histLineColour == "GREEN"} then {
         .gethistover.opt.lineColour.value  insert 0 "Green"
      } elseif {$histLineColour == "BLUE"} then {
         .gethistover.opt.lineColour.value  insert 0 "Blue"
      } elseif {$histLineColour == "CYAN"} then {
         .gethistover.opt.lineColour.value  insert 0 "Cyan"
      } elseif {$histLineColour == "MAGENTA"} then {
         .gethistover.opt.lineColour.value  insert 0 "Magenta"
      } else {
         .gethistover.opt.lineColour.value  insert 0 "Yellow"
      }
      .gethistover.opt.lineColour.value  configure  -state disabled

      set localLineColour $histLineColour

#
#  Bind a mouse click in the listbox to copy the chosen selection
#  to the entry box.

    bind .gethistover.sels.list  <ButtonRelease-1>  {
       set numSel [.gethistover.sels.list curselect]
       set numSel [expr $numSel - 1]

       if {$numSel > 0} then {
          .gethistover.opt.title.value delete 0 end
          .gethistover.opt.title.value insert 0 $numSel
       }
    }

#
#  Withdraw the window, then update all the geometry information
#  so we know how big it wants to be, then centre the window in
#  parent and de-iconify it.

    wm withdraw .gethistover
    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .gethistover]/2 + \
      [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .gethistover]/2 + \
      [winfo y .]]
    wm geom .gethistover +$x+$y
    wm deiconify .gethistover

#  Set a grab and claim the focus.
    set oldFocus [focus]
    grab .gethistover
    focus .gethistover

    tkwait variable GetHistOverButton

    if {$GetHistOverButton == "ok"} {
       set histLineColour $localLineColour

       set histSelection [.gethistover.opt.title.value get]

       set histFlag         "T"
    }

    if {$GetHistOverButton == "can"} {
       set histFlag         "F"
    }

# Destroy the dialogue box and restore the focus.
    destroy  .gethistover
    focus    $oldFocus

}
