proc GetParam {twidth prompt helptext} {

#+ GetParam
#
#  Procedure to get a value for a named parameter from the user.
#
#  A transient window is created into which the user types the required
#  value.
#
#  Given
#   twidth  (Given)
#      Width of the box in which the user supplies his reply (in
#      characters).
#   prompt  (Given)
#      Prompt string.
#   helptext (Given)
#      Name of the help text entry in procedure HelpText.

#  Author:
#   ACD.

#  History:
#   19/5/94  (ACD): Original version.
#   20/10/94 (ACD): First stable version.
#-

# Create the top level window.
    toplevel     .getparam   -class Dialog   -bd 10
    wm title     .getparam   "Input Value"
    wm iconname  .getparam   Input
    wm transient .getparam   .

    label .getparam.label  -text   $prompt
    entry .getparam.name   -width  $twidth   -relief  sunken   -bd 2
    pack  .getparam.label  .getparam.name

#
# Bind the return key to set global variable button to "ok".  That is,
# hitting return will have the same effect as clicking on the "ok"
# button.

    bind  .getparam.name   <Return> {global button; set button "ok" }

# Create "OK", "Cancel" and "Help" buttons.
    button .getparam.ok -text OK -width 6 -command {set button "ok"}
    button .getparam.can -text Cancel -width 6 \
        -command {set button "can"}
    button .getparam.help -text Help -width 6 \
        -command "HelpText {$helptext}"

# Pack them into the bottom frame with a default border around the OK
# button.
    frame .getparam.default -relief sunken -bd 1
    raise .getparam.ok .getparam.default
    pack .getparam.default -side left -expand 1 -padx 3m -pady 2m
    pack .getparam.ok -in .getparam.default -padx 1m -pady 1m -ipadx 1m

    pack .getparam.can  -side left -expand 1 -padx 3m -pady 2m -ipadx 1m
    pack .getparam.help -side left -expand 1 -padx 3m -pady 2m -ipadx 1m


# Withdraw the window, then update all the geometry information
# so we know how big it wants to be, then center the window in
# parent and de-iconify it.
    wm withdraw .getparam
    update idletasks
    set x [expr [winfo width .]/2 - [winfo reqwidth .getparam]/2 + [winfo x .]]
    set y [expr [winfo height .]/2 - [winfo reqheight .getparam]/2 + [winfo y .]]
    wm geom .getparam +$x+$y
    wm deiconify .getparam

#  Set a grab and claim the focus.
    set oldFocus [focus]
    grab .getparam
    focus .getparam.name

    global button
    tkwait variable button

    if {$button == "ok"} {
        global GetParamValue
	set    GetParamValue    [.getparam.name get]
    }

    if {$button == "can"} {
        global GetParamValue
	set    GetParamValue    ""
    }

# Destroy the dialogue box and restore the focus.
    destroy  .getparam
    focus    $oldFocus

}
