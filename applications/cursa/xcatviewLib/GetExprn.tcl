proc GetExprn { } {

#+ GetExprn
#
#  Get an expression.  The syntax of the returned expression is:
#
#    name{expression}units
#
#  'name' and 'expression' are mandatory; 'units' are optional.
#
#  Given
#    none.
#
#  Author:
#   ACD: A C Davenhall (Leicester).
#
#  History:
#   31/3/95 (ACD):  Original version.
#   31/3/95 (ACD):  First stable version.
#   20/10/96 (ACD): Modified for Tcl/Tk v4 and expect v5.
#   19/11/96 (ACD): Completed modifications for Tcl/Tk v4.
#-

#
#   Create the top level window.

     toplevel     .getexprn   -class Dialog   -bd 10
     wm title     .getexprn   "Define an expression"
     wm iconname  .getexprn   Expression
     wm transient .getexprn   .

#
#   Declare the global variables.
#   ----------------------------

#
#   The state returned by clicking on the "OK" or "Cancel" buttons.

     global getExprnButton

#
#   The returned expression.

     global exprnDefn


#
#   Bind the return key to set global variable button to "ok".  That is,
#   hitting return will have the same effect as clicking on the "ok"
#   button.

     bind  .getexprn  <Return> {set getExprnButton "ok" }

#
#   Create a frame to hold the entry boxes and titles for the catalogue
#   name and any comments to be added to it.

     frame .getexprn.inbox

#
#   Create the entry boxes and their associated titles.

     text .getexprn.inbox.title1  -height 1 -width 60 -relief flat  \
       -highlightthickness 0
     pack .getexprn.inbox.title1  -side top
     .getexprn.inbox.title1  insert 1.0 "Name:"

     entry .getexprn.inbox.name  -relief sunken -bd 2 -width 20
     pack  .getexprn.inbox.name  -side top -anchor w

     bind  .getexprn.inbox.name  <Return> {set getExprnButton "ok" }

     text .getexprn.inbox.title2  -height 1 -width 60 -relief flat  \
       -highlightthickness 0
     pack .getexprn.inbox.title2  -side top
     .getexprn.inbox.title2  insert 1.0 "Expression:"

     entry .getexprn.inbox.exprn  -relief sunken -bd 2 -width 60
     pack  .getexprn.inbox.exprn  -side top -anchor w
     .getexprn.inbox.exprn insert 0 ""

     bind  .getexprn.inbox.exprn  <Return> {set getExprnButton "ok" }

     text .getexprn.inbox.title3  -height 1 -width 60 -relief flat  \
       -highlightthickness 0
     pack .getexprn.inbox.title3  -side top
     .getexprn.inbox.title3  insert 1.0 "Units:"

     entry .getexprn.inbox.units  -relief sunken -bd 2 -width 30
     pack  .getexprn.inbox.units -side top -anchor w
     .getexprn.inbox.units insert 0 ""

     bind  .getexprn.inbox.units  <Return> {set getExprnButton "ok" }

#
#   Pack the frame into the window.

     pack  .getexprn.inbox  -side top

#
#   Create a frame to hold the row of control buttons.

     frame .getexprn.button

#
#   Create each of the buttons.
#
#   OK.

     button .getexprn.button.ok \
       -text OK \
       -width 6 \
       -command {set getExprnButton "ok"}

#
#   Cancel.

     button .getexprn.button.can \
       -text Cancel \
       -width 6 \
       -command {set getExprnButton "can"}

#
#   Help.

     menubutton .getexprn.button.help \
       -relief sunken  -bd 2 \
       -text Help \
       -width 6 \
       -menu .getexprn.button.help.menu

#
#   Define the options for the help menu.

     menu .getexprn.button.help.menu

     .getexprn.button.help.menu add command \
       -label "About this window" \
       -command {HelpText GetExprn_help}

     .getexprn.button.help.menu add command \
       -label "Expression syntax" \
       -command {HelpText expressions_help}

#
#   Pack the buttons into their enclosing frame with a default border around
#   the OK button.

     frame .getexprn.button.default -relief sunken -bd 1
     raise .getexprn.button.ok .getexprn.button.default

     pack  .getexprn.button.ok -in .getexprn.button.default \
       -side left  -padx 1m -pady 1m -ipadx 1m

     pack .getexprn.button.default  \
       -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getexprn.button.can   \
       -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

     pack .getexprn.button.help  \
       -side left  -expand 1  -padx 3m  -pady 2m  -ipadx 1m

#
#   Pack the frame of buttons into its frame.

     pack .getexprn.button  -side top

#
#   Withdraw the window, then update all the geometry information
#   to determine how big the window wants to be, then centre the window
#   in parent and de-iconify it.

     wm withdraw .getexprn
     update idletasks
     set x [expr [winfo width .]/2 - [winfo reqwidth .getexprn]/2 + \
       [winfo x .]]
     set y [expr [winfo height .]/2 - [winfo reqheight .getexprn]/2 + \
       [winfo y .]]
     wm geom .getexprn +$x+$y
     wm deiconify .getexprn

#
#   Set a grab and claim the focus.

     set oldFocus [focus]
     grab  .getexprn
     focus .getexprn

#
#   Wait for the "OK" button to be pushed.

     tkwait variable getExprnButton

     if {$getExprnButton == "ok"} then {
        set exprnName  [.getexprn.inbox.name   get]
        set exprnExprn [.getexprn.inbox.exprn   get]
        set exprnUnits [.getexprn.inbox.units   get]

        if {($exprnName != "")  && ($exprnExprn != "")} then {
           set    exprnDefn $exprnName
           append exprnDefn "{"
           append exprnDefn $exprnExprn
           append exprnDefn "}"
           append exprnDefn $exprnUnits
        } else {
           set exprnDefn ""
           Error "Error: either the expression or the name was not defined."
        }
     }

     if {$getExprnButton == "can"} then {
        set exprnDefn ""
     }

#
#   Destroy the dialogue box and restore the focus.

     destroy  .getexprn
     focus    $oldFocus

}
