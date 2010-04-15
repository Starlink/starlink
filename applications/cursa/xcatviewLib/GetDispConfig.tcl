proc GetDispConfig { } {

#+ GetDispConfig
#
#  Get the display configuration options.  Note that the current values are
#  used as defaults.
#
#  Given
#    none.
#
#  Author:
#   ACD: A C Davenhall (Leicester).

#  History:
#   23/10/94 (ACD): Original version.
#   15/3/95  (ACD): First stable version.
#   28/2/96  (ACD): Added option to change the screen width.
#   20/10/96 (ACD): Modified for Tcl/Tk v4 and expect v5.
#   19/11/96 (ACD): Completed modifications for Tcl/Tk v4.
#   24/1/97  (ACD): Fixed bug whereby the 'screen changfed' flag was
#      not being set for some options.
#-

#
#   Create the top level window.

     toplevel     .getdispconfig   -class Dialog   -bd 10
     wm title     .getdispconfig   "Set the Display Configuration"
     wm iconname  .getdispconfig   Display
     wm transient .getdispconfig   .

#
#   Declare the global variables.
#   ----------------------------

#
#   The state returned by clicking on the "OK" or "Cancel" buttons.

     global getDispConfigButton

#
#   Flag indicating whether a sequence number is to be included in
#   the listing.

     global sequenceNumber

#
#   Flag indicating angles are to be displayed.

     global angleRepn

#
#   Value for the screen width and the flag indicating whether it has
#   changed.

     global screenWidth
     global widthChange

#
#   GetConfig's copies of these flags.

     global seqnoGetDispConfig
     global angleGetDispConfig
     global widthGetDispConfig
     global changeGetDispConfig

#   --------------------

#
#   Bind the return key to set global variable button to "ok".  That is,
#   hitting return will have the same effect as clicking on the "ok"
#   button.

     bind  .getdispconfig  <Return> {set getDispConfigButton "ok" }

#
#   Create the frame to hold the vertical row of option buttons and
#   their defaults.

     frame .getdispconfig.opt

#
#   Create the menu buttons and value boxes and pack them into their frame.

     frame .getdispconfig.opt.seqno

     menubutton .getdispconfig.opt.seqno.but  \
       -relief sunken  -bd 2 -width 17  \
       -text "Sequence Number"  \
       -menu .getdispconfig.opt.seqno.but.menu
     pack .getdispconfig.opt.seqno.but  -side left

     entry .getdispconfig.opt.seqno.value  -width 12  -relief flat  \
       -highlightthickness 0
     pack  .getdispconfig.opt.seqno.value  -side left  -padx 2m
     .getdispconfig.opt.seqno.value  configure  -state disabled

     pack  .getdispconfig.opt.seqno -side top -pady 5m


     frame .getdispconfig.opt.angle

     menubutton .getdispconfig.opt.angle.but  \
       -relief sunken  -bd 2 -width 17  \
       -text "Angle Format" \
       -menu .getdispconfig.opt.angle.but.menu
     pack .getdispconfig.opt.angle.but  -side left

     entry .getdispconfig.opt.angle.value  -width 12  -relief flat  \
       -highlightthickness 0
     pack  .getdispconfig.opt.angle.value  -side left  -padx 2m
     .getdispconfig.opt.angle.value  configure  -state disabled

     pack  .getdispconfig.opt.angle  -side top -pady 5m


     frame .getdispconfig.opt.width

     menubutton .getdispconfig.opt.width.but  \
       -relief sunken  -bd 2 -width 17  \
       -text "Screen Width" \
       -menu .getdispconfig.opt.width.but.menu
     pack .getdispconfig.opt.width.but  -side left

     entry .getdispconfig.opt.width.value  -width 12  -relief flat  \
       -highlightthickness 0
     pack  .getdispconfig.opt.width.value  -side left  -padx 2m
     .getdispconfig.opt.width.value  configure  -state disabled

     pack  .getdispconfig.opt.width  -side top -pady 5m

#
#   Define the options for the 'sequence number' menu.

     menu .getdispconfig.opt.seqno.but.menu

     .getdispconfig.opt.seqno.but.menu  add command \
        -label Yes  \
        -command {
          .getdispconfig.opt.seqno.value  configure  -state normal
          .getdispconfig.opt.seqno.value  delete 0 end
          .getdispconfig.opt.seqno.value  insert 0 "Yes"
          .getdispconfig.opt.seqno.value  configure  -state disabled

          global seqnoGetDispConfig
          set    seqnoGetDispConfig  "y"

          global changeGetDispConfig
          set    changeGetDispConfig "y"
         }

     .getdispconfig.opt.seqno.but.menu  add command \
        -label No  \
        -command {
          .getdispconfig.opt.seqno.value  configure  -state normal
          .getdispconfig.opt.seqno.value  delete 0 end
          .getdispconfig.opt.seqno.value  insert 0 "No"
          .getdispconfig.opt.seqno.value  configure  -state disabled

          global seqnoGetDispConfig
          set    seqnoGetDispConfig  "n"

          global changeGetDispConfig
          set    changeGetDispConfig "y"
         }

#
#   Define the options for the 'angle format' menu.

     menu .getdispconfig.opt.angle.but.menu

     .getdispconfig.opt.angle.but.menu  add command \
        -label "Sexagesimal hours or degrees"  \
        -command {
          .getdispconfig.opt.angle.value  configure  -state normal
          .getdispconfig.opt.angle.value  delete 0 end
          .getdispconfig.opt.angle.value  insert 0 "Sexagesimal"
          .getdispconfig.opt.angle.value  configure  -state disabled

          global angleGetDispConfig
          set    angleGetDispConfig "SEXAGESIMAL"

          global changeGetDispConfig
          set    changeGetDispConfig "y"
         }

     .getdispconfig.opt.angle.but.menu  add command \
        -label Radians  \
        -command {
          .getdispconfig.opt.angle.value  configure  -state normal
          .getdispconfig.opt.angle.value  delete 0 end
          .getdispconfig.opt.angle.value  insert 0 "Radians"
          .getdispconfig.opt.angle.value  configure  -state disabled

          global angleGetDispConfig
          set    angleGetDispConfig "RADIANS"

          global changeGetDispConfig
          set    changeGetDispConfig "y"
         }

#
#   Define the options for the 'screen width' menu.

     menu .getdispconfig.opt.width.but.menu

     .getdispconfig.opt.width.but.menu  add command \
        -label "80"  \
        -command {
          .getdispconfig.opt.width.value  configure  -state normal
          .getdispconfig.opt.width.value  delete 0 end
          .getdispconfig.opt.width.value  insert 0 "80"
          .getdispconfig.opt.width.value  configure  -state disabled

          global widthGetDispConfig
          set    widthGetDispConfig 79

          global changeGetDispConfig
          set    changeGetDispConfig "y"
         }

     .getdispconfig.opt.width.but.menu  add command \
        -label "100"  \
        -command {
          .getdispconfig.opt.width.value  configure  -state normal
          .getdispconfig.opt.width.value  delete 0 end
          .getdispconfig.opt.width.value  insert 0 "100"
          .getdispconfig.opt.width.value  configure  -state disabled

          global widthGetDispConfig
          set    widthGetDispConfig 99

          global changeGetDispConfig
          set    changeGetDispConfig "y"
         }

     .getdispconfig.opt.width.but.menu  add command \
        -label "120"  \
        -command {
          .getdispconfig.opt.width.value  configure  -state normal
          .getdispconfig.opt.width.value  delete 0 end
          .getdispconfig.opt.width.value  insert 0 "120"
          .getdispconfig.opt.width.value  configure  -state disabled

          global widthGetDispConfig
          set    widthGetDispConfig 119

          global changeGetDispConfig
          set    changeGetDispConfig "y"
         }

     .getdispconfig.opt.width.but.menu  add command \
        -label "140"  \
        -command {
          .getdispconfig.opt.width.value  configure  -state normal
          .getdispconfig.opt.width.value  delete 0 end
          .getdispconfig.opt.width.value  insert 0 "140"
          .getdispconfig.opt.width.value  configure  -state disabled

          global widthGetDispConfig
          set    widthGetDispConfig 139

          global changeGetDispConfig
          set    changeGetDispConfig "y"
         }

     .getdispconfig.opt.width.but.menu  add command \
        -label "160"  \
        -command {
          .getdispconfig.opt.width.value  configure  -state normal
          .getdispconfig.opt.width.value  delete 0 end
          .getdispconfig.opt.width.value  insert 0 "160"
          .getdispconfig.opt.width.value  configure  -state disabled

          global widthGetDispConfig
          set    widthGetDispConfig 159

          global changeGetDispConfig
          set    changeGetDispConfig "y"
         }

#
#   Pack the frames enclosing the rows of menu buttons and entry boxes into
#   their frame.

     pack .getdispconfig.opt    -side left

#
#   Create a frame to hold the row of control buttons.

     frame .getdispconfig.button

#
#   Create each of the buttons.
#
#   OK.

     button .getdispconfig.button.ok \
       -text OK \
       -width 6 \
       -command {set getDispConfigButton "ok"}

#
#   Cancel.

     button .getdispconfig.button.can \
       -text Cancel \
       -width 6 \
       -command {set getDispConfigButton "can"}

#
#   Help.

     button .getdispconfig.button.help \
       -text Help \
       -width 6 \
       -command {HelpText GetDispConfig_help}

#
#   Pack the buttons into their enclosing frame with a default border around
#   the OK button.

     frame .getdispconfig.button.default -relief sunken -bd 1
     raise .getdispconfig.button.ok .getdispconfig.button.default

     pack .getdispconfig.button.ok -in .getdispconfig.button.default \
       -side top  -padx 1m  -pady 1m  -ipadx 1m

     pack .getdispconfig.button.default  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getdispconfig.button.can   \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getdispconfig.button.help  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

#
#   Pack the frame of buttons into the window.

     pack .getdispconfig.button  -side left

#
#   Withdraw the window, then update all the geometry information
#   to determine how big the window wants to be, then centre the window
#   in parent and de-iconify it.

     wm withdraw .getdispconfig
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .getdispconfig]/2 + \
       [winfo x .]]
     set y [expr [winfo height .]/2 - [winfo reqheight .getdispconfig]/2 + \
       [winfo y .]]
     wm geom .getdispconfig +$x+$y
     wm deiconify .getdispconfig

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab  .getdispconfig
     focus .getdispconfig

#
#   Set the defaults for the return values for the options.  Note that
#   current values of the global variables are used as the defaults.

     if {$sequenceNumber == "y"} then {
        .getdispconfig.opt.seqno.value  configure  -state normal
        .getdispconfig.opt.seqno.value  delete 0 end
        .getdispconfig.opt.seqno.value  insert 0 "Yes"
        .getdispconfig.opt.seqno.value  configure  -state disabled

        set seqnoGetDispConfig "y"
     } else {
        .getdispconfig.opt.seqno.value  configure  -state normal
        .getdispconfig.opt.seqno.value  delete 0 end
        .getdispconfig.opt.seqno.value  insert 0 "No"
        .getdispconfig.opt.seqno.value  configure  -state disabled

        set seqnoGetDispConfig  "n"
     }


     if {$angleRepn == "SEXAGESIMAL"} then {
        .getdispconfig.opt.angle.value  configure  -state normal
        .getdispconfig.opt.angle.value  delete 0 end
        .getdispconfig.opt.angle.value  insert 0 "Sexagesimal"
        .getdispconfig.opt.angle.value  configure  -state disabled

        set angleGetDispConfig "SEXAGESIMAL"
     } else {
        .getdispconfig.opt.angle.value  configure  -state normal
        .getdispconfig.opt.angle.value  delete 0 end
        .getdispconfig.opt.angle.value  insert 0 "Radians"
        .getdispconfig.opt.angle.value  configure  -state disabled

        set angleGetDispConfig "RADIANS"
     }

     set widthGetDispConfig [expr $screenWidth + 1]

     .getdispconfig.opt.width.value  configure  -state normal
     .getdispconfig.opt.width.value  delete 0 end
     .getdispconfig.opt.width.value  insert 0 $widthGetDispConfig
     .getdispconfig.opt.width.value  configure  -state disabled

#
#   Wait for the "OK" button to be pushed.

     tkwait variable getDispConfigButton

     if {$getDispConfigButton == "ok"} then {
        set sequenceNumber $seqnoGetDispConfig
        set angleRepn      $angleGetDispConfig

        set screenWidth    $widthGetDispConfig
        set widthChange    $changeGetDispConfig
     }

     if {$getDispConfigButton == "can"} then { }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .getdispconfig
     focus    $oldFocus

}
