proc buildP4Widgets2 w {
#+
# This procedure builds the p4 plotting window widget tree. The names of any
# "active" widgets are stored in the global array "P4Widgets" so that
# the names of the widgets can be changed without effecting the rest of
# the application.
#
# The return value is the name of the frame widget that contains the
# widget tree.
#-
    global env
    global P4Widgets
    global cgs4drHtml

# Create some widget options just for the plotting window
    set optRoot *[string range $w 1 end]
    option add $optRoot*Radiobutton.width 15

# Create the gwm widget with scroll bars
    global gwm
    set gwm [gwm_gwmWithScroll $w.frame -gwmname $env(PID)xwin]
    $gwm configure -crosscolour #FF00FF
    bind $gwm <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4GwmBox1.html"

# Create and pack a frame for the control buttons
    pack [frame $w.bottom] -side bottom -fill x
    pack [frame $w.buttons -relief sunken -border 2] -padx 3 -pady 3 -side right -in $w.bottom

# Pack a label along side the button frame to match the appearance of the frame
    pack [label $w.bottom.fill -text "Portable-CGS4DR" -relief sunken -border 2] -fill both -padx 3 -pady 3 -side left -expand y

# Create the command buttons
    button $w.buttons.colours -text Colours -padx 10 -command "gwm_colourDialog $w.col $gwm $w.buttons.colours"
    button $w.buttons.clear -text Clear -padx 10 -command "$gwm clear"
    button $w.buttons.print -text Print -padx 10 -command "gwm_printDialog $w.pr $gwm $w.buttons.print"
    checkbutton $w.buttons.crosshair -text Crosshair -padx 10 -variable P4Widgets(CROSSHAIR) -command crossHair

# Bind them to help text
    bind $w.buttons.clear <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4GwmBox2.html"
    bind $w.buttons.colours <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4GwmBox3.html"
    bind $w.buttons.print <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4GwmBox4.html"
    bind $w.buttons.crosshair <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4GwmBox5.html"

# Pack the buttons into the frame
    pack $w.buttons.clear $w.buttons.colours $w.buttons.print $w.buttons.crosshair -side left -expand y -padx 5 -pady 5

# Pack the gwm widget's frame into the top level widget. This is done last
# so that when the top level is resized it is the gwm widget that gets
# resized to fit rather than the frame containing the buttons.
    pack $w.frame.gwm -in $w.frame
    return $w.frame
}

proc crossHair {} {
  global P4Widgets
  global gwm
  global P4Task
  if { $P4Widgets(CROSSHAIR) == 1 } {
    bind $gwm <Any-Motion> {%W set crosshair %x %y}
    bind $gwm <Any-Enter> {%W configure -crosshair yes}
    bind $gwm <Any-Leave> {%W configure -crosshair no}
    bind $gwm <Button-1> {cgs4drInform $P4Task "GWM Widget Co-ords: X=%x Y=%y"}
    bind $gwm <ButtonPress-2> {showPointer %x %y}
    bind $gwm <ButtonRelease-2> {destroy .position}
  } else {
    bind $gwm <Any-Motion> {}
    bind $gwm <Any-Enter> {}
    bind $gwm <Any-Leave> {}
    bind $gwm <Button-1> {}
    bind $gwm <ButtonPress-2> {}
    bind $gwm <ButtonRelease-2> {}
    $gwm configure -crosshair no
  }
}

proc showPointer {x y} {
#+
# This procedure pops up a panel that displays values of the parameters
# x and y corrected for any scrolling of the gwm widget.
#-
    toplevel .position -bd 3 -relief raised
    wm overrideredirect .position 1
    global gwm
    set xpos [expr [winfo rootx $gwm] + $x]
    set ypos [expr [winfo rooty $gwm] + $y]
    wm geometry .position +$xpos+$ypos
    set x [expr $x - [$gwm cget -xoffset]]
    set y [expr $y - [$gwm cget -yoffset]]
    label .position.t -text "GWM Widget Co-ords: "
    label .position.c -text "X = $x   Y = $y"
    pack .position.t .position.c
}
