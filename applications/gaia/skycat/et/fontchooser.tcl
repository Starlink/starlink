# This code accompanies the "fontchooser.c" file.  It does most of the
# work of setting up and operating the font chooser.
option add *highlightThickness 0

# Title the font chooser and make it resizeable.
#
wm title . "Font Chooser"
wm iconname . "FontChooser"
wm minsize . 1 1

# Functions to tell about this program
#
proc AboutMessage {} {
  catch {destroy .about}
  toplevel .about
  wm title .about "About Fontchooser"
  wm iconname .about "AboutFontchooser"
  label .about.title -text {Font Chooser}\
    -font -adobe-times-bold-i-normal--24-240-75-75-p-128-iso8859-1
  pack .about.title -side top -pady 15
  message .about.subtitle -width 10c -justify center \
    -font -adobe-times-bold-i-normal-*-14-140-75-75-p-77-iso8859-1 \
    -text "A utility for selecting X11 fonts"
  pack .about.subtitle -side top -pady 10 -padx 15
  message .about.msg -width 10c -text "
By D. Richard Hipp
Hipp, Wyrick & Company, Inc.
6200 Maple Cove Lane
Charlotte, NC  28269
704-948-4565" \
    -font -adobe-times-medium-r-normal-*-12-120-75-75-p-64-iso8859-1
  pack .about.msg -padx 15 -anchor w
  button .about.dismiss -text {Dismiss} -command {destroy .about}
  pack .about.dismiss -pady 8
  wm withdraw .about
  update idletasks
  set x [expr [winfo rootx .] + ([winfo width .]-[winfo reqwidth .about])/2]
  set y [expr [winfo rooty .] + ([winfo height .]-[winfo reqheight .about])/2]
  wm geometry .about +$x+$y
  wm deiconify .about
}

# Construct a panel for selecting the font family.
#
frame .name -bd 0 -relief raised
frame .name.x -bd 1 -relief sunken
listbox .name.x.box -yscrollcommand ".name.x.sb set" \
  -width 46 -height 12 \
  -font fixed -exportselection 0 -selectmode single
scrollbar .name.x.sb -orient vertical -command ".name.x.box yview"
pack .name.x.box -side left -expand 1 -fill both
pack .name.x.sb -fill y -side left
pack .name.x -side bottom -padx 5 -pady 3
label .name.label -text Family
pack .name.label -side top

# Construct a panel used to select the font size
#
frame .size -bd 0 -relief raised
frame .size.x -bd 1 -relief sunken
listbox .size.x.box -yscrollcommand ".size.x.sb set" \
  -width 30 -height 12 \
  -font fixed -exportselection 0 -selectmode single
scrollbar .size.x.sb -orient vertical -command ".size.x.box yview"
pack .size.x.box -side left -expand 1 -fill both
pack .size.x.sb -fill y -side left
pack .size.x -side bottom -padx 5 -pady 3
label .size.label -text Size
pack .size.label -side top

# Construct a panel used to display an example of the choosen font.
#
canvas .c -bd 0 -relief raised -height 3c

# Construct the menu bar.
#
frame .menu -bd 2 -relief raised
menubutton .menu.file -text File -menu .menu.file.menu
pack .menu.file -side left -padx 5
menu .menu.file.menu
.menu.file.menu add cascade -label Resolution -menu .menu.file.menu.res
menu .menu.file.menu.res
.menu.file.menu.res add command -label {75 dpi} -command {LoadFontInfo 75}
.menu.file.menu.res add command -label {100 dpi} -command {LoadFontInfo 100}
.menu.file.menu.res add command -label {Both} -command {LoadFontInfo 0}
.menu.file.menu add command -label {Invoke xfd}  -command {
  if {[info exists Name] && [info exists Size]} {
    exec xfd -fn -$Name-*-$Size &
  }
}
.menu.file.menu add separator
.menu.file.menu add command -label Exit -command {destroy .}
menubutton .menu.edit -text Edit -menu .menu.edit.menu
pack .menu.edit -side left -padx 5
menu .menu.edit.menu
.menu.edit.menu add command -label Copy -command Copy
menubutton .menu.help -text Help -menu .menu.help.menu
pack .menu.help -side left -padx 5
menu .menu.help.menu
.menu.help.menu add command -label {About This Program} \
   -command AboutMessage
eval tk_menuBar .menu [winfo children .menu]

# Pack together the various panels.
#
pack .c -side bottom -fill x
pack .menu -side top -fill x
pack .name .size -side left -fill both -expand 1

# This procedure reads font information for a font of a specified resolution
# and sets up the font family listbox appropriately.
#
proc LoadFontInfo {resolution} {
  global FontCount Font
  FindFonts $resolution
  set sel [.name.x.box cursel]
  .name.x.box delete 0 end
  .size.x.box delete 0 end
  .c delete all
  if {$FontCount>0} {
    foreach font [lsort [array names Font]] {.name.x.box insert end $font}
  }
  if {[llength $sel]==1} {
    FamilySet [lindex $sel 0]
  } else {
    FamilySet 0
  }
}

# Click inside the font family box to select a font family.  This causes
# the sizes available in that font to appear in the size box.
#
bind .name.x.box <1> {FamilySet [%W nearest %y]; break}

proc FamilySet {y} {
  global Name Font
  set Name [.name.x.box get $y]
  if {[info exists Font($Name)]} {
    .name.x.box select clear 0 end
    .name.x.box select set $y
    .name.x.box see $y
    set sel [.size.x.box cursel]
    .size.x.box delete 0 end
    foreach size $Font($Name) {.size.x.box insert end $size}
    if {[llength $sel]==1} {
      SizeSet [lindex $sel 0]
    } else {
      SizeSet 0
    }
  } else {
    .name.x.box select clear
  }
}

# Click inside the size box to select a particular font size.  This causes
# example text to be displayed, and enables pasteing of the full font name.
#
bind .size.x.box <1> {SizeSet [.size.x.box nearest %y]; break}

# This procedure selects the "y"-th element in the size listbox and
# displays the corresponding font in the canvas.
#
proc SizeSet {y} {
  global Name Font Size
  set Size [.size.x.box get $y]
  .size.x.box select clear 0 end
  .size.x.box select set $y
  .size.x.box see $y
  .c delete all
  set x [expr [winfo width .c]/2]
  set y [expr [winfo height .c]/2]
  .c create text $x $y -anchor c -justify center \
    -text abcdefghijklmnopqrstuvwxyz\nABCDEFGHIJKLMNOPQRSTUVWXYZ\n0123456789 \
    -font -$Name-*-$Size 
}

# This routine "copies" the currently selected font.  This means that the
# font name is available for pasting by other applications.
#
proc Copy {} {
  global Copied Name Size
  if {[info exists Name] && [info exists Size]} {
    set Copied -$Name-*-$Size
    selection handle . HandleSelectionRequest
    selection own .
  }
}

# This procedure is called when another program attempt to take the name
# of the selected font from the clipboard.  Simply return the name of the
# selected font.
#
proc HandleSelectionRequest {offset MaxBytes} {
  global Copied
  return $Copied
}

# Begin by displaying the 75 dot-per-inch fonts
#
update
LoadFontInfo 0
