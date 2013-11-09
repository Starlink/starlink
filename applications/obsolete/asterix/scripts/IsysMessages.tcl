#
proc HandleOutput {txt} {
  global msg_data


  $msg_data configure -state normal
  $msg_data insert end "$txt\n"
  $msg_data yview moveto 1
  $msg_data configure -state disabled
}
#
#
#
proc Show_Error {w msg} {

# Modified to wait for the 'close' button to be clicked.
# A C Davenhall (Edinburgh) 19/5/97.

#  create display window
  toplevel $w -class Dialog
  wm title $w "Error"
  wm iconname $w Error
  wm transient $w .



  pack [frame $w.top ] \
       [frame $w.bot ] -side top

  pack [text $w.top.text -width 30 -height 10 -wrap word ]


  $w.top.text insert end $msg

  button $w.bot.close -text Close -command {set show_Error_Button "close"}

  pack  $w.bot.close -side top -pady 1m

  Centre_Window $w

#
#  Set a grab and claim the focus.

  set oldFocus [focus]
  grab  $w
  focus $w

#
#  Wait for the "close" button to be pushed.

  tkwait variable show_Error_Button

  destroy $w
  focus   $oldFocus

}
#
#
#
proc colour_warning {w} {

  toplevel $w -class Dialog
  wm title $w "Warning"
  wm iconname $w Warning
  wm transient $w .

  global  Koff Close
  set Koff 0
  
  pack [frame $w.top] [frame $w.mid] [frame $w.bot]

  set t [text $w.top.txt -width 40 -height 10 -wrap word]
  $t insert end "Isys has been unable to find enough colours in the "
  $t insert end "default colour map and has had to create a private one. "
  $t insert end "This explains why you are probably getting some freaky "
  $t insert end "colour effects on your screen.  If you can live with this "
  $t insert end "then carry on.  If not then close some other windows, eg. "
  $t insert end "Netscape, and then restart Isys"

  pack $t

  checkbutton $w.mid.check -text "Don't show me this again" -variable Koff

  pack $w.mid.check -pady 2m

  button $w.bot.close -text Close -command { 
     if {$Koff == 1} {
       UpdateSetting ColourWarn 0
     }
     set Close 1
  }
  
  pack $w.bot.close -fill x -pady 2m

  Centre_Window $w

  set oldFocus [focus]
  grab  $w
  focus $w

  tkwait variable Close

  destroy $w
  focus   $oldFocus
}
