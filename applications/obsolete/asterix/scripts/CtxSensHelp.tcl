#
# A number of procedures for handling context sensitive help
#
#   MakeHelpWindow 	- create context sensitive help window
#   SetHelpText		- set help text for a window

#
#
#
global CtxHlpString 
set CtxHlpString " "

#
# Make a context sensitive help window
#
proc MakeHelpWindow {name {width 0}} {

  global CtxHlpString

# Create enclosing frame
  set ctxhlpbar [frame $name]

# Label for help
  label $ctxhlpbar.lbl -text "Help:" \
      -font -adobe-times-bold-r-normal--*-140-*-*-*-*-*-*

# Text display area
  if { $width == 0 } {
    label $ctxhlpbar.txt -relief sunken \
      -font -adobe-times-bold-r-normal--*-140-*-*-*-*-*-* \
      -textvariable CtxHlpString  -foreground Blue -background LightYellow

  } else {
    label $ctxhlpbar.txt -relief sunken \
      -font -adobe-times-bold-r-normal--*-140-*-*-*-*-*-* \
      -textvariable CtxHlpString -width $width \
      -foreground Blue -background LightYellow

    }

# Create and pack text fields
  pack $ctxhlpbar.lbl $ctxhlpbar.txt -side left -anchor w -ipadx 2m

  return $ctxhlpbar
  }


proc SetHelpText {window text} {

# Set string on window entry
  set cmd "bind $window <Enter> {set CtxHlpString \"$text\"}"
  eval $cmd

# String is cleared on window exit
  bind $window <Leave> {
    set CtxHlpString ""
    }
  }
