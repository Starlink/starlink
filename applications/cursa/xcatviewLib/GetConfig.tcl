proc GetConfig { } {

#+ GetConfig
#
#  Get the configuration options.  Currently the only configuration
#  option is whether or not to echo commands sent to the A-task to
#  the error and message window.  Note that the current value is used
#  as the default.
#
#  Given
#    none.
#
#  Author:
#   ACD: A C Davenhall (Leicester).

#  History:
#   23/10/94 (ACD): Original version.
#   15/3/95  (ACD): First stable version.
#   26/2/96  (ACD): Added options to set global variable angleReformat.
#   20/10/96 (ACD): Modified for Tcl/Tk v4 and expect v5.
#   19/11/96 (ACD): Completed modifications for Tcl/Tk v4.
#   17/4/01  (ACD): Added option to set the quiet mode.
#-

#
#   Create the top level window.

     toplevel     .getconfig   -class Dialog   -bd 10
     wm title     .getconfig   "Set the Configuration"
     wm iconname  .getconfig   Configuration
     wm transient .getconfig   .

#
#   Declare the global variables.
#   ----------------------------

#
#   The state returned by clicking on the "OK" or "Cancel" buttons.

     global getConfigButton

#
#   Flag indicating whether commands sent to the ADAM A-task are to be
#   echoed to the the error window.

     global echoCommand

#
#   Flag indicating whether the UNITS attribute of angles is to be
#   reformatted.

     global angleReformat

#
#  Flag indicating the quiet mode.

    global quietMode

#
#   GetConfig's copies of these flags.

     global echoGetConfig
     global angleGetConfig
     global quietGetConfig

#   --------------------

#
#   Bind the return key to set global variable button to "ok".  That is,
#   hitting return will have the same effect as clicking on the "ok"
#   button.

     bind  .getconfig  <Return> {set getConfigButton "ok" }

#
#   Create the frame to hold the vertical row of option buttons and
#   their defaults.

     frame .getconfig.opt

#
#   Create the menu buttons and value boxes and pack them into their frame:

#   ... echo command,

     frame .getconfig.opt.echo

     menubutton .getconfig.opt.echo.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Echo Command" \
       -menu .getconfig.opt.echo.but.menu
     pack .getconfig.opt.echo.but  -side left

     entry .getconfig.opt.echo.value  -width 7  -relief flat  \
       -highlightthickness 0
     pack  .getconfig.opt.echo.value  -side left  -padx 2m
     .getconfig.opt.echo.value  configure  -state disabled

     pack  .getconfig.opt.echo  -side top -pady 5m

#   ... reformat angle units,

     frame .getconfig.opt.angleref

     menubutton .getconfig.opt.angleref.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Reformat Angle Units" \
       -menu .getconfig.opt.angleref.but.menu
     pack .getconfig.opt.angleref.but  -side left

     entry .getconfig.opt.angleref.value  -width 7  -relief flat  \
       -highlightthickness 0
     pack  .getconfig.opt.angleref.value  -side left  -padx 2m
     .getconfig.opt.angleref.value  configure  -state disabled

     pack  .getconfig.opt.angleref  -side top -pady 5m

#   ... quiet mode.

     frame .getconfig.opt.quiet

     menubutton .getconfig.opt.quiet.but  \
       -relief sunken  -bd 2 -width 20  \
       -text "Quiet Mode" \
       -menu .getconfig.opt.quiet.but.menu
     pack .getconfig.opt.quiet.but  -side left

     entry .getconfig.opt.quiet.value  -width 7  -relief flat  \
       -highlightthickness 0
     pack  .getconfig.opt.quiet.value  -side left  -padx 2m
     .getconfig.opt.quiet.value  configure  -state disabled

     pack  .getconfig.opt.quiet  -side top -pady 5m

#
#   Define the options for the 'echo command' menu.

     menu .getconfig.opt.echo.but.menu

     .getconfig.opt.echo.but.menu  add command \
        -label Yes  \
        -command {
          .getconfig.opt.echo.value  configure  -state normal
          .getconfig.opt.echo.value  delete 0 end
          .getconfig.opt.echo.value  insert 0 "Yes"
          .getconfig.opt.echo.value  configure  -state disabled

          global echoGetConfig
          set    echoGetConfig 1
         }

     .getconfig.opt.echo.but.menu  add command \
        -label No  \
        -command {
          .getconfig.opt.echo.value  configure  -state normal
          .getconfig.opt.echo.value  delete 0 end
          .getconfig.opt.echo.value  insert 0 "No"
          .getconfig.opt.echo.value  configure  -state disabled

          global echoGetConfig
          set    echoGetConfig 0
         }

#
#   Define the options for the 'Reformat Angle Units' menu.

     menu .getconfig.opt.angleref.but.menu

     .getconfig.opt.angleref.but.menu  add command \
        -label Yes  \
        -command {
          .getconfig.opt.angleref.value  configure  -state normal
          .getconfig.opt.angleref.value  delete 0 end
          .getconfig.opt.angleref.value  insert 0 "Yes"
          .getconfig.opt.angleref.value  configure  -state disabled

          global angleGetConfig
          set    angleGetConfig  "y"
         }

     .getconfig.opt.angleref.but.menu  add command \
        -label No  \
        -command {
          .getconfig.opt.angleref.value  configure  -state normal
          .getconfig.opt.angleref.value  delete 0 end
          .getconfig.opt.angleref.value  insert 0 "No"
          .getconfig.opt.angleref.value  configure  -state disabled

          global angleGetConfig
          set    angleGetConfig  "n"
         }

#
#   Define the options for the 'Quiet Mode' menu.

     menu .getconfig.opt.quiet.but.menu

     set quietGetConfig "false"

     .getconfig.opt.quiet.but.menu  add command \
        -label "Verbose (normal)"  \
        -command {
          .getconfig.opt.quiet.value  configure  -state normal
          .getconfig.opt.quiet.value  delete 0 end
          .getconfig.opt.quiet.value  insert 0 "Verbose"
          .getconfig.opt.quiet.value  configure  -state disabled

          global quietGetConfig
          set    quietGetConfig  "false"
         }

     .getconfig.opt.quiet.but.menu  add command \
        -label Quiet  \
        -command {
          .getconfig.opt.quiet.value  configure  -state normal
          .getconfig.opt.quiet.value  delete 0 end
          .getconfig.opt.quiet.value  insert 0 "Quiet"
          .getconfig.opt.quiet.value  configure  -state disabled

          global quietGetConfig
          set    quietGetConfig  "true"
         }

#
#   Pack the frames enclosing the rows of menu buttons and entry boxes into
#   their frame.

     pack .getconfig.opt    -side left

#
#   Create a frame to hold the row of control buttons.

     frame .getconfig.button

#
#   Create each of the buttons.
#
#   OK.

     button .getconfig.button.ok \
       -text OK \
       -width 6 \
       -command {set getConfigButton "ok"}

#
#   Cancel.

     button .getconfig.button.can \
       -text Cancel \
       -width 6 \
       -command {set getConfigButton "can"}

#
#   Help.

     button .getconfig.button.help \
       -text Help \
       -width 6 \
       -command {HelpText GetConfig_help}

#
#   Pack the buttons into their enclosing frame with a default border around
#   the OK button.

     frame .getconfig.button.default -relief sunken -bd 1
     raise .getconfig.button.ok .getconfig.button.default

     pack  .getconfig.button.ok -in .getconfig.button.default \
       -side top  -padx 1m -pady 1m -ipadx 1m

     pack .getconfig.button.default  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getconfig.button.can   \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getconfig.button.help  \
       -side top  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

#
#   Pack the frame of buttons into the window.

     pack .getconfig.button  -side left

#
#   Withdraw the window, then update all the geometry information
#   to determine how big the window wants to be, then centre the window
#   in parent and de-iconify it.

     wm withdraw .getconfig
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .getconfig]/2 + \
       [winfo x .]]
     set y [expr [winfo height .]/2 - [winfo reqheight .getconfig]/2 + \
       [winfo y .]]
     wm geom .getconfig +$x+$y
     wm deiconify .getconfig

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab  .getconfig
     focus .getconfig

#
#   Set the default for the return value for the options.  Note that
#   current value of the global variable is used as the default.

     if {$echoCommand == 0} then {
        .getconfig.opt.echo.value  configure  -state normal
        .getconfig.opt.echo.value  delete 0 end
        .getconfig.opt.echo.value  insert 0 "No"
        .getconfig.opt.echo.value  configure  -state disabled

        set echoGetConfig 0
     } else {
        .getconfig.opt.echo.value  configure  -state normal
        .getconfig.opt.echo.value  delete 0 end
        .getconfig.opt.echo.value  insert 0 "Yes"
        .getconfig.opt.echo.value  configure  -state disabled

        set echoGetConfig 1
     }

     if {$angleReformat == "n"} then {
        .getconfig.opt.angleref.value  configure  -state normal
        .getconfig.opt.angleref.value  delete 0 end
        .getconfig.opt.angleref.value  insert 0 "No"
        .getconfig.opt.angleref.value  configure  -state disabled

        set angleGetConfig "n"
     } else {
        .getconfig.opt.angleref.value  configure  -state normal
        .getconfig.opt.angleref.value  delete 0 end
        .getconfig.opt.angleref.value  insert 0 "Yes"
        .getconfig.opt.angleref.value  configure  -state disabled

        set angleGetConfig "y"
     }

     if {$quietMode == "false"} then {
        .getconfig.opt.quiet.value  configure  -state normal
        .getconfig.opt.quiet.value  delete 0 end
        .getconfig.opt.quiet.value  insert 0 "Verbose"
        .getconfig.opt.quiet.value  configure  -state disabled

        set quietGetConfig "false"
     } else {
        .getconfig.opt.quiet.value  configure  -state normal
        .getconfig.opt.quiet.value  delete 0 end
        .getconfig.opt.quiet.value  insert 0 "Quiet"
        .getconfig.opt.quiet.value  configure  -state disabled

        set quietGetConfig "true"
     }

#
#   Wait for the "OK" button to be pushed.

     tkwait variable getConfigButton

     if {$getConfigButton == "ok"} then {
        set echoCommand    $echoGetConfig
        set angleReformat  $angleGetConfig
        set quietMode      $quietGetConfig
     }

     if {$getConfigButton == "can"} then { }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .getconfig
     focus    $oldFocus

}
