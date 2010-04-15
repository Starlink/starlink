proc GetCat { } {

#+ GetCat
#
#  Get the details to specify a catalogue.  The details are the name of
#  the catalogue, any comments to be added to it and the various options for
#  the items to be included in the catalogue.
#
#  Given
#    none.
#
#  Author:
#   ACD: A C Davenhall (Leicester).
#
#  History:
#   21/10/94 (ACD): Original version.
#   17/3/94  (ACD): First stable version.
#   20/10/96 (ACD): Modified for Tcl/Tk v4 and expect v5.
#   19/11/96 (ACD): Completed modifications for Tcl/Tk v4.
#-

#
#   Create the top level window.

     toplevel     .getcat   -class Dialog   -bd 10
     wm title     .getcat   "Save a Catalogue"
     wm iconname  .getcat   Catalogue
     wm transient .getcat   .

#
#   Declare the global variables.
#   ----------------------------

#
#   The state returned by clicking on the "OK" or "Cancel" buttons.

     global getCatButton

#
#   The options for the various items in the text file.

     global saveCatColumns
     global saveCatText

#
#   Local copies of these items.

     global localCatColumns
     global localCatText

#
#   The name of the file and any comments to be added to it.

     global saveCatName
     global saveCatComm

#
#   Bind the return key to set global variable button to "ok".  That is,
#   hitting return will have the same effect as clicking on the "ok"
#   button.

     bind  .getcat  <Return> {set getCatButton "ok" }

#
#   Create a frame to hold the entry boxes and titles for the catalogue
#   name and any comments to be added to it.

     frame .getcat.inbox

#
#   Create the entry boxes and their associated titles.

     text .getcat.inbox.title1  -height 1 -width 60 -relief flat  \
       -highlightthickness 0
     pack .getcat.inbox.title1  -side top
     .getcat.inbox.title1  insert 1.0 "Catalogue name:"

     entry .getcat.inbox.cat  -relief sunken -bd 2 -width 60
     pack  .getcat.inbox.cat  -side top

     bind  .getcat.inbox.cat  <Return> {set getCatButton "ok" }

     text .getcat.inbox.title2  -height 1 -width 60 -relief flat  \
       -highlightthickness 0
     pack .getcat.inbox.title2  -side top
     .getcat.inbox.title2  insert 1.0 "Comments:"

     entry .getcat.inbox.comm  -relief sunken -bd 2 -width 60
     pack  .getcat.inbox.comm  -side top
     .getcat.inbox.comm insert 0 ""

     bind  .getcat.inbox.comm  <Return> {set getCatButton "ok" }

#
#   Pack the frame into the window.

     pack  .getcat.inbox  -side top

#
#   Create the frame to hold the two vertical rows of buttons and
#   the row of current values for the options.

     frame .getcat.button

#
#   Create the frames to hold the vertical row of option menu buttons
#   and the row of values for the current options.

     frame .getcat.button.opt

#
#   Create the menu buttons and value boxes and pack them into their frame.

     frame .getcat.button.opt.cols

     menubutton .getcat.button.opt.cols.but  \
       -relief sunken  -bd 2 -width 10  \
       -text "Columns"  \
       -menu .getcat.button.opt.cols.but.menu
     pack .getcat.button.opt.cols.but  -side left

     entry .getcat.button.opt.cols.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getcat.button.opt.cols.value  -side left  -padx 2m
     .getcat.button.opt.cols.value  configure  -state disabled

     pack  .getcat.button.opt.cols -side top -pady 2m

     frame .getcat.button.opt.text

     menubutton .getcat.button.opt.text.but  \
       -relief sunken  -bd 2 -width 10  \
       -text "Text" \
       -menu .getcat.button.opt.text.but.menu
     pack .getcat.button.opt.text.but  -side left

     entry .getcat.button.opt.text.value  -width 15  -relief flat  \
       -highlightthickness 0
     pack  .getcat.button.opt.text.value  -side left  -padx 2m
     .getcat.button.opt.text.value  configure  -state disabled

     pack  .getcat.button.opt.text -side top -pady 2m


#
#   Define the options for the 'columns' menu.

     menu .getcat.button.opt.cols.but.menu

     .getcat.button.opt.cols.but.menu  add command \
        -label All  \
        -command {
          .getcat.button.opt.cols.value  configure  -state normal
          .getcat.button.opt.cols.value  delete 0 end
          .getcat.button.opt.cols.value  insert 0 "All"
          .getcat.button.opt.cols.value  configure  -state disabled

          global localCatColumns
          set    localCatColumns "T"
         }

     .getcat.button.opt.cols.but.menu  add command \
        -label "Current list"  \
        -command {
          .getcat.button.opt.cols.value  configure  -state normal
          .getcat.button.opt.cols.value  delete 0 end
          .getcat.button.opt.cols.value  insert 0 "Current list"
          .getcat.button.opt.cols.value  configure  -state disabled

          global localCatColumns
          set    localCatColumns "F"
         }

#
#   Define the options for the 'text' menu.

     menu .getcat.button.opt.text.but.menu

     .getcat.button.opt.text.but.menu  add command \
        -label Yes  \
        -command {
          .getcat.button.opt.text.value  configure  -state normal
          .getcat.button.opt.text.value  delete 0 end
          .getcat.button.opt.text.value  insert 0 "Yes"
          .getcat.button.opt.text.value  configure  -state disabled

          global localCatText
          set    localCatText "T"
         }

     .getcat.button.opt.text.but.menu  add command \
        -label No  \
        -command {
          .getcat.button.opt.text.value  configure  -state normal
          .getcat.button.opt.text.value  delete 0 end
          .getcat.button.opt.text.value  insert 0 "No"
          .getcat.button.opt.text.value  configure  -state disabled

          global localCatText
          set    localCatText "F"
         }


#
#   Pack the frames enclosing the rows of buttons and entry boxes into
#   their frame.

     pack .getcat.button.opt    -side left

#
#   Create a frame to hold the row of control buttons.

     frame .getcat.button.ctrl

#
#   Create each of the buttons.
#
#   OK.

     button .getcat.button.ctrl.ok \
       -text OK \
       -width 6 \
       -command {set getCatButton "ok"}

#
#   Cancel.

     button .getcat.button.ctrl.can \
       -text Cancel \
       -width 6 \
       -command {set getCatButton "can"}

#
#   Help.

     button .getcat.button.ctrl.help \
       -text Help \
       -width 6 \
       -command {HelpText GetCat_help}

#
#   Pack the buttons into their enclosing frame with a default border around
#   the OK button.

     frame .getcat.button.ctrl.default -relief sunken -bd 1
     raise .getcat.button.ctrl.ok .getcat.button.ctrl.default

     pack  .getcat.button.ctrl.ok -in .getcat.button.ctrl.default \
       -side top  -padx 1m -pady 1m -ipadx 1m

     pack .getcat.button.ctrl.default  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getcat.button.ctrl.can   \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getcat.button.ctrl.help  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

#
#   Pack the frame of buttons into its frame.

     pack .getcat.button.ctrl  -side left

#
#   Pack the frame holding all the buttons into the window.


     pack .getcat.button  -side top

#
#   Withdraw the window, then update all the geometry information
#   to determine how big the window wants to be, then centre the window
#   in parent and de-iconify it.

     wm withdraw .getcat
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .getcat]/2 + \
       [winfo x .]]
     set y [expr [winfo height .]/2 - [winfo reqheight .getcat]/2 + \
       [winfo y .]]
     wm geom .getcat +$x+$y
     wm deiconify .getcat

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab  .getcat
     focus .getcat

#
#   Set the defaults for the return values for the options.  The current
#   values are used as the defaults.

     if {$saveCatColumns == "T"} then {
        .getcat.button.opt.cols.value  configure  -state normal
        .getcat.button.opt.cols.value  delete 0 end
        .getcat.button.opt.cols.value  insert 0 "All"
        .getcat.button.opt.cols.value  configure  -state disabled

        global localCatColumns
        set    localCatColumns "T"
     } else {
        .getcat.button.opt.cols.value  configure  -state normal
        .getcat.button.opt.cols.value  delete 0 end
        .getcat.button.opt.cols.value  insert 0 "Current list"
        .getcat.button.opt.cols.value  configure  -state disabled

        global localCatColumns
        set    localCatColumns "F"
     }

     if {$saveCatText == "T"} then {
        .getcat.button.opt.text.value  configure  -state normal
        .getcat.button.opt.text.value  delete 0 end
        .getcat.button.opt.text.value  insert 0 "Yes"
        .getcat.button.opt.text.value  configure  -state disabled

        global localCatText
        set    localCatText "T"
     } else {
        .getcat.button.opt.text.value  configure  -state normal
        .getcat.button.opt.text.value  delete 0 end
        .getcat.button.opt.text.value  insert 0 "No"
        .getcat.button.opt.text.value  configure  -state disabled

        global localCatText
        set    localCatText "F"
     }

#
#   Wait for the "OK" button to be pushed.

     tkwait variable getCatButton

     if {$getCatButton == "ok"} then {
        set saveCatName [.getcat.inbox.cat   get]
        set saveCatComm [.getcat.inbox.comm  get]

        if {$saveCatComm == ""} then {
           set saveCatComm "<none>"
        }

        set saveCatColumns $localCatColumns
        set saveCatText    $localCatText
     }

     if {$getCatButton == "can"} then {
        set saveCatName  ""
        set saveCatComm  ""
     }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .getcat
     focus    $oldFocus

}
