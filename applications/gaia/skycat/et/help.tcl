###########################################################################
# Routines to implement hyper-text help
#
# Call this routine with a single argument which is the name of a help
# page, and the appropriate help page will appear.  A new help widget
# is created if necessary.
#
# This is the only function in the help package with external linkage
#
# In order to be useful, this routine needs access to help data in
# the variables __HelpScript() and __HelpButton().  This data is
# generated from an SGML specification using the "help2tk" tool.
#
proc Help {tag} {
  global __HelpData __HelpButton
  if {[winfo exists .help]} {
    raise .help
  } else {
    toplevel .help
    wm title .help "[wm title .] Help"
    wm iconname .help Help
    text .help.c -width 70 -height 20 -yscrollcommand {.help.sb set} \
      -font -adobe-times-medium-r-normal-*-14-140-75-75-p-74-iso8859-1 \
      -tabs {1c 2c 3c 4c 5c 6c 7c} -wrap word \
      -cursor top_left_arrow -padx 10 -pady 10
    set __HelpData(widget) .help.c
    scrollbar .help.sb -orient vertical -command {.help.c yview}
    frame .help.f
    button .help.f.back -text Back -pady 0 -command {__HelpMove -1}
    button .help.f.forward -text Forward -pady 0 -command {__HelpMove 1}
    pack .help.f.back .help.f.forward -side left -padx 1 -pady 2
    foreach i [array names __HelpButton] {
      button .help.f.x$i -text $__HelpButton($i) -pady 0 -command "Help $i"
      pack .help.f.x$i -side left -padx 1 -pady 2
    }
    button .help.f.dismiss -text {Dismiss} -pady 0 -command __HelpDismiss
    pack .help.f.dismiss -side left -padx 1 -pady 2
    pack .help.f -side bottom -fill x
    pack .help.c -side left -fill both -expand 1
    pack .help.sb -side left -fill y
    .help.c tag config a -foreground navyblue
    .help.c tag config em \
       -font -adobe-times-medium-i-normal-*-14-140-75-75-p-73-iso8859-1
    .help.c tag config strong \
       -font -adobe-times-bold-r-normal-*-14-140-75-75-p-77-iso8859-1
    .help.c tag config title -justify center \
       -font -adobe-times-bold-r-normal-*-18-180-75-75-p-99-iso8859-1
    .help.c tag config center -justify c
    .help.c tag config code -font fixed
    .help.c tag config pre -font fixed -wrap none
    set i 0
    while {$i<6} {
      set j [expr $i+1]
      .help.c tag config i$i -lmargin1 ${j}c -lmargin2 ${j}c
      .help.c tag config l$i -lmargin1 ${i}c -lmargin2 ${j}c -font fixed
      .help.c tag config d$i -lmargin1 ${i}c -lmargin2 ${j}c
      incr i
    }
    set __HelpData(ctr) 0
  }
  incr __HelpData(ctr)
  set x $__HelpData(ctr)
  set __HelpData(history:$x) $tag
  set y [expr $x+1]
  catch {unset __HelpData(history:$y)}
  __Help_SetText $x
}

# This routine actually inserts text into a help widget.  The argument
# is the number of the page to be inserted.  
#
# This routine is for internal use only
proc __Help_SetText {x} {
  global __HelpData __HelpScript
  set w $__HelpData(widget)
  $w config -state normal
  $w delete 1.0 end
  set tag $__HelpData(history:$x)
  eval $__HelpScript($tag)
  set __HelpData(ctr) $x
  $w config -state disabled
  incr x -1
  if {[info exists __HelpData(history:$x)]} {
    .help.f.back config -state normal
  } else {
    .help.f.back config -state disabled
  }
  incr x 2
  if {[info exists __HelpData(history:$x)]} {
    .help.f.forward config -state normal
  } else {
    .help.f.forward config -state disabled
  }
}

# This routine is called when the Dismiss button is pressed
#
proc __HelpDismiss {} {
  global __HelpData
  if {![winfo exists .help]} return
  destroy .help
  unset __HelpData
}

# This routine is called with the Back or Forward buttons are pressed
#
proc __HelpMove {amt} {
  global __HelpData
  if {![info exists __HelpData(ctr)]} return
  set x $__HelpData(ctr)
  incr x $amt
  if {[info exists __HelpData(history:$x)]} {
    __Help_SetText $x
  }
}

# End of the help routine
###########################################################################
