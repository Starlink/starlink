#
# Tcl/Tk code for the ET color chooser
#
option add *highlightThickness 0

# Title the color chooser and make it resizeable.
#
wm title . "Color Chooser"
wm iconname . "ColorChooser"
wm minsize . 1 1

# Construct the menu bar.
#
frame .menu -bd 2 -relief raised
pack .menu -side top -fill x
menubutton .menu.file -text File -menu .menu.file.menu
pack .menu.file -side left -padx 5
menu .menu.file.menu
  .menu.file.menu add command -label Exit -command {destroy .}

menubutton .menu.help -text Help -menu .menu.help.menu
pack .menu.help -side left -padx 5
menu .menu.help.menu
  .menu.help.menu add command -label {About This Program} \
     -command {Help About}
  .menu.help.menu add command -label {Introduction} \
     -command {Help Purpose}
  .menu.help.menu add command -label {Contents} \
     -command {Help Index}

# eval tk_menuBar .menu [winfo children .menu]

# Construct the color swatch
#
label .swatch -bd 2 -relief raised -height 3
pack .swatch -side top -fill x -padx 5 -pady 5
label .unmapped
proc ChangeSwatch {fg bg label} {
  .unmapped config -bg [lindex [.swatch config -bg] 4]
  .swatch config -bg $bg -foreground $fg -text $label
  update
}

# Construct the panel which shows similar named colors
#
frame .near -bd 2 -relief groove
pack .near -side right -padx 5 -pady 5 -fill y
label .near.label -text {Similar Colors}
pack .near.label -side top
foreach i {x1 x2 x3 x4 x5 x6} {
  label .near.$i -bd 2 -relief raised -text Bisque1 -width 18
  bind .near.$i <1> {ChangeColor [lindex [%W config -bg] 4]}
  pack .near.$i -side top -padx 5 -pady 2 -fill x -expand 1
}

# Construct the sliders for adjusting the color
#
frame .scales -bd 2 -relief groove
pack .scales -side right -padx 5 -pady 5 -fill y -expand 1
label .scales.label -text {Color Components}
pack .scales.label -side top
foreach i {red green blue hue saturation intensity} {
  frame .scales.$i
  pack .scales.$i -side top -padx 5 -pady 2 -anchor e
  scale .scales.$i.x -from 0 -to 999 -orient horizontal -showvalue 0 \
     -length 6c
  bind .scales.$i.x <ButtonPress-1> "%W config -command \"ChangeComponent $i\""
  bind .scales.$i.x <ButtonRelease-1> {%W config -command {}}
  pack .scales.$i.x -side right -anchor e
  label .scales.$i.l -text $i -anchor e
  pack .scales.$i.l -side right -anchor e
}

# Initialize the color selection
#
ChangeComponent red 0
ChangeColor bisque
