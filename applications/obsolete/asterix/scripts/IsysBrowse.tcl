#
proc browse_data {w } {

  global nbid
  global canv Width Height
  global Top

  set Top $w

#  set starting values in noticeboard
  nbs put $nbid.flag 1
  nbs put $nbid.options "D"
  nbs put $nbid.xpmax $Width
  nbs put $nbid.ypmax $Height

#  start monitoring
  monitor_browse

#  start browsing application
  ImgExecNoWait ibrowse " "

#  create display window
  toplevel $w -class Dialog
  wm title $w "Browse"
  wm iconname $w Browse


  pack [frame $w.title] -side top 
  pack [frame $w.panel] -side bottom 

  global browseTitle
  label $w.title.text -textvariable browseTitle \
        -font -adobe-times-medium-r-normal--*-120-*-*-*-*-*-*
  pack $w.title.text

#  create cells to display monitored data
  for {set j 1} {$j<=9} {incr j} {
    pack [frame $w.row$j] -side bottom
    for {set i 1} {$i<=9} {incr i} {
      global data$i$j
      pack [label $w.row$j.col$i -textvariable data$i$j -width 8 \
           -font -adobe-courier-medium-r-normal--*-80-*-*-*-*-*-*] -side left
    }
  }

  global browseOpt
  trace variable browseOpt w browse_options
  set browseOpt "D"

#  control buttons
  radiobutton $w.panel.data -text "Data" -variable browseOpt -value "D" \
      -font -adobe-times-medium-r-normal--*-100-*-*-*-*-*-* -relief raised
  radiobutton $w.panel.var -text "Variance" -variable browseOpt -value "V" \
      -font -adobe-times-medium-r-normal--*-100-*-*-*-*-*-* -relief raised
  radiobutton $w.panel.err -text "Error" -variable browseOpt -value "E" \
      -font -adobe-times-medium-r-normal--*-100-*-*-*-*-*-* -relief raised
  radiobutton $w.panel.signif -text "Signif." -variable browseOpt -value "S" \
      -font -adobe-times-medium-r-normal--*-100-*-*-*-*-*-* -relief raised
  radiobutton $w.panel.qual -text "Quality" -variable browseOpt -value "Q" \
      -font -adobe-times-medium-r-normal--*-100-*-*-*-*-*-* -relief raised
  button $w.panel.quit -text "Quit" -command {set Quit yes} \
      -font -adobe-times-medium-r-normal--*-100-*-*-*-*-*-* 

  pack $w.panel.data $w.panel.var $w.panel.err $w.panel.signif \
          $w.panel.qual $w.panel.quit -side left 


  bind $canv <Enter> {nbs put $nbid.flag 0}
  bind $canv <Motion> {browse_display %x %y}
  bind $canv <ButtonPress-1> {raise $Top .top}
  $canv configure -cursor crosshair

  tkwait variable Quit
  bind $canv <Enter> {}
  bind $canv <Motion> {}
  bind $canv <ButtonPress-1> {}
  $canv configure -cursor arrow
  nbs put $nbid.flag 1
  trace vdelete variable w browseOpt
  destroy $w
  monitor_standard
}
#


proc browse_display {x y} {

  global nbid

  nbs put $nbid.xp $x
  nbs put $nbid.yp $y

}
#
proc browse_options {name element op} {

  global  browseOpt
  global browseTitle
  global nbid

  nbs put $nbid.options $browseOpt

  if {$browseOpt=="D"} {
    set browseTitle "Data Values"
  } elseif {$browseOpt=="V"} {
    set browseTitle "Variances"
  } elseif {$browseOpt=="E"} {
    set browseTitle "Errors"
  } elseif {$browseOpt=="S"} {
    set browseTitle "Significances"
  } elseif {$browseOpt=="Q"} {
    set browseTitle "Quality Bits"
  }
}
#