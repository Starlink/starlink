#
# This example illustrates how to construct a viewport-like widget using
# a frame and the placer geometry manager.  Five listboxes are constructed
# and placed in a window which is too narrow to hold them all.  A horizontal
# scrollbar allows the listboxes to be moved left or right in their viewport
# window.

# Allow the main window to be resized
wm minsize . 1 1

# Construct the horizontal scrollbar used to scroll the listboxes
scrollbar .hsb -orient horizontal -command HScroll
pack .hsb -side bottom -fill x

# The frame ".f" is the "clipping window".  
frame .f -height 300 -width 600
pack .f -fill both -expand 1

# The frame ".f.f" is the window that gets clipped.  This is the window
# that is moved left and right by the horizontal scrollbar.  The five
# listboxes are all children of this window.
frame .f.f

# Construct the 5 listboxes and fill them with some arbitrary data
set i 1
while {$i<=5} {
  listbox .f.f.lb$i -yscrollcommand ".f.f.sb$i set"
  foreach j {0 1 2 3 4 5 6 7 8 9} {.f.f.lb$i insert end "$i$j"}
  scrollbar .f.f.sb$i -orient vertical -command ".f.f.lb$i yview"
  pack .f.f.lb$i .f.f.sb$i -side left -fill y
  incr i
}

# We have to reset the scrollbar whenever either the clipping window or
# the scrolling window are resized.
bind .f.f <Configure> {HScroll $FirstUnit}
bind .f <Configure> {HScroll $FirstUnit}

# The global variable "FirstUnit" stores and integer which determines where
# the left edge of .f.f is relative to the left edge of .f.  When FirstUnit==0,
# the left edges correspond.  When FirstUnit==1, the left edge of .f.f is
# 10 pixels to the left of the left edge of .f.  And so forth.
set FirstUnit 0

# This function causes the .f.f window to slide left and right within
# the clipping window .f.  It also updates the position of the scrollbar.
#
proc HScroll {x} {
  global FirstUnit

  # totalUnits is an integer proportional to the width of .f.f
  set totalUnits [expr int([winfo reqwidth .f.f]/10)]

  # windowUnits is an integer which is proportional to the amount of
  # .f.f which is actual visible.  If totalUnits is, say, 100, and exactly
  # half of .f.f is visible within .f, then windowUnits will be 50.
  set windowUnits [expr int([winfo width .f]/10)]

  # Limit the range of x so that we don't scroll too far left or right.
  if {$x+$windowUnits>$totalUnits} {
    set x [expr {$totalUnits - $windowUnits}]
  }
  if {$x<0} {
    set x 0
  }
  set FirstUnit $x

  # Adjust the position of the scrollbar thumb
  .hsb set $totalUnits $windowUnits $x [expr $x+$windowUnits]

  # Adjust the position of .f.f within the clipping window .f
  place .f.f -anchor nw -relheight 1 -x [expr -10*$x] -y 0 \
     -width [winfo reqwidth .f.f]
}
