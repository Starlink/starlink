option add *highlightThickness 0
wm title . tkterm
wm iconname . tkterm

# Functions to tell about this program
#
proc AboutMessage {} {
  catch {destroy .about}
  toplevel .about
  wm title .about "About TkTerm"
  wm iconname .about "AboutTkTerm"
  label .about.title -text {TkTerm}\
    -font -adobe-times-bold-i-normal--24-240-75-75-p-128-iso8859-1
  pack .about.title -side top -pady 15
  message .about.subtitle -width 10c -justify center \
    -font -adobe-times-bold-i-normal-*-14-140-75-75-p-77-iso8859-1 \
    -text "A VT100 terminal emulator based\non the Tcl/Tk Text widget"
  pack .about.subtitle -side top -pady 10 -padx 15
  message .about.msg -width 10c -text "
By D. Richard Hipp
6200 Maple Cove Lane
Charlotte, NC 28269
704-948-4565
drh@acm.org" \
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

proc CenterWindow w {
  update
  set x [expr [winfo rootx .] + ([winfo width .]-[winfo reqwidth $w])/2]
  set y [expr [winfo rooty .] + ([winfo height .]-[winfo reqheight $w])/2]
  wm geometry $w +$x+$y
  wm deiconify $w
}

# Create the name-change dialog box.
#
proc TitleChange {} {
  set w .titlechange
  if {[winfo exists $w]} {
    wm withdraw $w
    CenterWindow $w
    focus $w.e
    return
  }
  toplevel $w
  wm withdraw $w
  wm title $w "Change Window Title"
  wm iconname $w "Change Window Title"
  frame $w.b
  pack $w.b -side bottom -fill x
  button $w.b.cancel -text Cancel -width 6 -command "TitleChangeClose $w"
  button $w.b.ok -text Ok -width 6 -command "TitleChangeApply $w"
  pack $w.b.ok $w.b.cancel -side left -expand 1 -pady 15
  entry $w.e -bd 2 -relief sunken -width 30
  bind $w.e <Return> "TitleChangeApply $w"
  $w.e insert end [wm title .]
  pack $w.e -side bottom -expand 1 -padx 20 -pady 10
  label $w.l -text "Enter New Window Title Below"
  pack $w.l -side top
  CenterWindow $w
  focus $w.e
}
proc TitleChangeClose w {
  destroy $w
  after 100 {focus .t}
}
proc TitleChangeApply w {
  wm title . [$w.e get]
  wm iconname . [$w.e get]
  TitleChangeClose $w
}

##### Construct the menu bar
#
frame .menubar -bd 2 -relief raised
pack .menubar -side top -fill x

menubutton .menubar.file -text File -menu .menubar.file.m -pady 0 \
  -underline 0
pack .menubar.file -side left -padx 10
menu .menubar.file.m
#  .menubar.file.m add command -label {New} -underline 0 \
#      -command "Paste \"$cmd_dir/$cmd_name &\n\""
#  .menubar.file.m add separator
  .menubar.file.m add command -label {Change Title...} -command TitleChange
  .menubar.file.m add separator
  .menubar.file.m add command -label {Exit} -command {destroy .} \
      -underline 1

menubutton .menubar.edit -text Edit -menu .menubar.edit.m -pady 0 \
  -underline 0
pack .menubar.edit -side left -padx 10
menu .menubar.edit.m
  .menubar.edit.m add command -label {Copy} -underline 0 \
     -command EditCopy -accelerator Alt-c
  .menubar.edit.m add command -label {Paste       } \
     -command EditPaste -accelerator Alt-p

menubutton .menubar.options -text Options -menu .menubar.options.m -pady 0 \
  -underline 0
pack .menubar.options -side left -padx 10
menu .menubar.options.m
  .menubar.options.m add cascade -label {Font Size} \
    -menu .menubar.options.m.fontsize
  menu .menubar.options.m.fontsize
  foreach i {Tiny Small Short Normal Large {Very Large} Huge} {
    .menubar.options.m.fontsize add radiobutton -label $i \
       -value $i -variable Font -command "ChangeFont \"$i\""
  }
  .menubar.options.m add cascade -label {Height} \
    -menu .menubar.options.m.height
  menu .menubar.options.m.height
  foreach i {8 12 16 24 30 36 42 48 54 60 72} {
    .menubar.options.m.height add radiobutton -label $i \
       -value $i -variable Height -command "ChangeHeight $i"
  }
  .menubar.options.m add cascade -label {Width} \
    -menu .menubar.options.m.width
  menu .menubar.options.m.width
  foreach i {64 72 80 90 110 123 140 161} {
    .menubar.options.m.width add radiobutton -label $i \
       -value $i -variable Width -command "ChangeWidth $i"
  }

menubutton .menubar.help -text Help -menu .menubar.help.menu -pady 0 \
  -underline 0
pack .menubar.help -side left -padx 5
menu .menubar.help.menu
  .menubar.help.menu add command -label {About This Program} \
     -command AboutMessage

proc EditPaste {} {
  Paste [selection get]
}

# Put a speed bar directly below the menu bar
frame .sbar -bd 1  -relief raised
pack .sbar -side top -expand 1 -fill x
set i 0
foreach btn {
  {Copy EditCopy 0}
  {Paste EditPaste 0}
  {{12} {ChangeHeight 12} 0}
  {{24} {ChangeHeight 24} 0}
  {{36} {ChangeHeight 36} 0}
  {{48} {ChangeHeight 48} 0}
  {{58} {ChangeHeight 58} 0}
  {{Title} {TitleChange} 0}
  {{Normal} {ChangeFont Normal} 0}
  {{Short} {ChangeFont Short} 4}
  {{Small} {ChangeFont Small} 1}
} {
  incr i
  button .sbar.b$i -text [lindex $btn 0] \
    -font -adobe-helvetica-medium-r-normal-*-8-80-75-75-p-46-iso8859-1 \
    -command [lindex $btn 1] -underline [lindex $btn 2]\
    -padx 1 -pady 0
  pack .sbar.b$i -side left -padx 1 -pady 1
}

####
# Default window settings
#
set Width 80
set Height 24
set Font Normal

######
# These are all the valid fonts.  FB(x) is the bold font which
# corresponds to F(x).  If there is no FB(x), then no bold text
# will be shown.
#
set F(Tiny) -schumacher-clean-medium-r-normal-*-6-60-75-75-c-40-iso8859-1
set F(Small) -schumacher-clean-medium-r-normal-*-8-80-75-75-c-50-iso8859-1
set F(Short) -schumacher-clean-medium-r-normal-*-10-100-75-75-c-60-iso8859-1
set FB(Short) -schumacher-clean-bold-r-normal-*-10-100-75-75-c-60-iso8859-1
set F(Normal) -misc-fixed-medium-r-semicondensed-*-13-120-75-75-c-60-iso8859-1
set FB(Normal) -misc-fixed-bold-r-semicondensed-*-13-120-75-75-c-60-iso8859-1
set F(Large) -misc-fixed-medium-r-normal-*-14-130-75-75-c-70-iso8859-1
set FB(Large) -misc-fixed-bold-r-normal-*-14-130-75-75-c-70-iso8859-1
set F(Very\ Large) -misc-fixed-medium-r-normal-*-15-140-75-75-c-90-iso8859-1
set FB(Very\ Large) -misc-fixed-bold-r-normal-*-15-140-75-75-c-90-iso8859-1
set F(Huge) -misc-fixed-medium-r-normal-*-20-200-75-75-c-100-iso8859-1

##### Construct the text widget with its scrollbar
#
text .t -bd 1 -relief raised -yscrollcommand {.sb set} \
  -height 24 -width 80 -exportselection 0 \
  -wrap none -padx 2 -pady 2 \
  -font $F($Font) -highlightthickness 0
pack .t -side left -fill both -expand 1
scrollbar .sb -command {.t yview} -orient vertical \
  -highlightthickness 0 -bd 1 -relief raised
pack .sb -side left -fill y

.t tag config ul -underline 1
.t tag config iv -foreground [.t cget -background]
.t tag config iv -background [.t cget -foreground]
if {[info exists FB($Font)]} {
  .t tag config bd -font $FB($Font)
} else {
  .t tag config bd -font $F($Font)
}

# Change the width of the text widget
proc ChangeWidth {newWidth} {
  .t config -width $newWidth
  WindowSizeChangeNotify
  global Width
  set Width $newWidth
  ResizeRootWindow
}
# Change the height of the text widget
proc ChangeHeight {newHeight} {
  .t config -height $newHeight
  global Height
  set Height $newHeight
  WindowSizeChangeNotify
  update
  SimulatedInput {}
  ResizeRootWindow
}
# Make sure the root window is of the correct size
proc ResizeRootWindow {} {
  set w [winfo reqwidth .]
  set h [winfo reqheight .]
  wm geometry . ${w}x$h
}
# Change the font of the text widget
proc ChangeFont {newFont} {
  global F FB Font
  .t config -font $F($newFont)
  set Font $newFont
  if {[info exists FB($newFont)]} {
    .t tag config bd -font $FB($newFont) -foreground Black
  } else {
    .t tag config bd -font $F($newFont) -foreground MidnightBlue
  }
  update
  SimulatedInput {}
}

bindtags .t .t
bind .t <KeyPress> {SendToTTY %N}
bind .t <Control-KeyPress> {SendToTTY [expr %N&0x1f]}
bind .t <Control-space> {SendZeroToTTY}
bind .t <Return> {SendToTTY 10}
bind .t <Tab> {SendToTTY 9}
bind .t <Escape> {SendToTTY 033}
bind .t <BackSpace> {SendToTTY 8}
bind .t <Delete> {SendToTTY 0177}
bind .t <Up> {Paste \033\[A}
bind .t <Down> {Paste \033\[B}
bind .t <Right> {Paste \033\[C}
bind .t <Left> {Paste \033\[D}
bind .t <Prior> {Paste \033\[5~}
bind .t <Next> {Paste \033\[6~}
bind .t <Home> {Paste \033\[; SendZeroToTTY}
bind .t <End> {Paste \033\[e}
bind .t <F1> {Paste \033\[11~}
bind .t <F2> {Paste \033\[12~}
bind .t <F3> {Paste \033\[13~}
bind .t <F4> {Paste \033\[14~}
bind .t <F5> {Paste \033\[15~}
bind .t <F6> {Paste \033\[16~}
bind .t <F7> {Paste \033\[17~}
bind .t <F8> {Paste \033\[20~}
bind .t <F9> {Paste \033\[21~}
bind .t <F10> {Paste \033\[22~}
bind .t <F11> {Paste \033\[23~}
bind .t <F12> {Paste \033\[24~}
bind .t <Alt-KeyPress> {tkTraverseToMenu %W %A}

# tkMenuFind --
# This procedure searches the entire window hierarchy under w for
# a menubutton that isn't disabled and whose underlined character
# is "char".  It returns the name of that window, if found, or an
# empty string if no matching window was found.  If "char" is an
# empty string then the procedure returns the name of the first
# menubutton found that isn't disabled.
#
# If a third argument is provided, it is used as a classname pattern
# for the window to search for.  Be default, this pattern is
# MenuButton, meaning that this routine will find only menubuttons.
# But if you change the class pattern to "*utton", the routine will
# find the first button of any type.
#
# Arguments:
# w -				Name of window where key was typed.
# char -			Underlined character to search for;
#				may be either upper or lower case, and
#				will match either upper or lower case.

proc tkMenuFind {w char {pattern Menubutton}} {
    global tkPriv
    set char [string tolower $char]
    set action [format {
        %s {
            set char2 [string index [$child cget -text] \
		[$child cget -underline]]
	    if {([string compare $char [string tolower $char2]] == 0)
		|| ($char == "")} {
  	        if {[$child cget -state] != "disabled"} {
		    return $child
		}
	    }
	}

	default {
	    set match [tkMenuFind $child $char %s]
	    if {$match != ""} {
		return $match
	    }
	}
    } $pattern $pattern]

    foreach child [winfo child $w] {
	switch -glob [winfo class $child] $action
    }
    return {}
}

# tkTraverseToMenu --
# This procedure implements keyboard traversal of menus.  Given an
# ASCII character "char", it looks for a menubutton with that character
# underlined.  If one is found, it posts the menubutton's menu.
#
# The routine will also look for buttons to invoke.  If a button is
# found that contains the given character, then that button is invoked.tkp
#
# Arguments:
# w -				Window in which the key was typed (selects
#				a toplevel window).
# char -			Character that selects a menu.  The case
#				is ignored.  If an empty string, nothing
#				happens.

proc tkTraverseToMenu {w char} {
    if ![winfo exists $w] return
    global tkPriv
    if {$char == ""} {
	return
    }
    while {[winfo class $w] == "Menu"} {
	if {$tkPriv(postedMb) == ""} {
	    return
	}
	set w [winfo parent $w]
    }
    set w [tkMenuFind [winfo toplevel $w] $char *utton]
    if {$w != ""} {
        switch [winfo class $w] {
            Menubutton {
   	        tkMbPost $w
	        tkMenuFirstEntry [$w cget -menu]
            }

            Button {
                tkButtonInvoke $w
            }

            Checkbutton {
                tkCheckRadioInvoke $w
            }

            Radiobutton {
                tkCheckRadioInvoke $w
            }
        }
    }
}

# Button bindings copied from the default Text widget bindings
#
foreach b {
  1 B1-Motion Double-1 Triple-1 Shift-1 Double-Shift-1
  Triple-Shift-1 B1-Leave B1-Enter ButtonRelease-1 Control-1
} {
  bind .t <$b> [bind Text <$b>]
  bind .t <$b> {+.t mark set insert $CurY.$CurX}
}
focus .t

# A routine for dispensing the selection.  The selection is always owned
# by the window ".".  Its value is stored in the variable "Selection"
#
set Selection {}
selection handle . RetrieveSelection
proc RetrieveSelection {offset max} {
  global Selection
  return [string range $Selection $offset [expr {$offset+$max}]]
}

# This routine is called whenever "." owns the selection but another
# window claims ownership.
#
proc LoseSelection {} {
  global Selection
  set Selection {}
}

# Copy the text selected in the text widget into the Selection variable,
# then claim ownership of the selection.
#
proc EditCopy {} {
  global Selection
  catch {
    set Selection [.t get sel.first sel.last]
    selection own . LoseSelection
  }
}

########
# The following is for debugging use only.
#
proc Dump {} {
  global Btm CurX CurY ScrollTop ScrollBtm

  return "iBtm=$Btm iCur=$CurY.$CurX iScroll=$ScrollTop-$ScrollBtm end=[.t index end] insert=[.t index insert]"
}
#
########
