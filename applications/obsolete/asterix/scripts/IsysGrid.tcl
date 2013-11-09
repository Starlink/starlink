#
proc set_grid {w } {

  ImgExecWaitNoMsg igui "action=synchro value=twod"
  ImgExecWaitNoMsg igui "action=update" 


  global W
  set W $w

  global AutoRefresh

  global nbid

  global Grid_frame Grid_pos

  global old_frame old_pos

  set Grid_frame [nbs get $nbid.gcb.grid_frame]
  set old_frame $Grid_frame
  set Grid_pos [nbs get $nbid.gcb.grid_pos]
  set old_pos $Grid_pos

  toplevel $w -class Dialog
  wm title $w "Coordinate Grid"
  wm iconname $w Grid

  pack [frame $w.top] -side top -fill both
  pack [frame $w.bot] -side bottom -fill both

  pack [frame $w.top.a] [frame $w.top.b]  -side left

  label $w.top.a.lbl -text "Coordinates"
  radiobutton $w.top.a.ra -text "RA/DEC" -variable Grid_frame -value 1
  radiobutton $w.top.a.ecl -text "Ecliptic" -variable Grid_frame -value 2
  radiobutton $w.top.a.gal -text "Galactic" -variable Grid_frame -value 3
  pack $w.top.a.lbl -side top -anchor n
  pack $w.top.a.ra $w.top.a.ecl $w.top.a.gal -side top -anchor nw

  label $w.top.b.lbl -text "Label Position"
  radiobutton $w.top.b.cen -text "In Centre" -variable Grid_pos -value 2
  radiobutton $w.top.b.edg -text "Inside Edge" -variable Grid_pos -value 1
  radiobutton $w.top.b.out -text "Outside Edge" -variable Grid_pos -value 3

  pack $w.top.b.lbl -side top -anchor n
  pack $w.top.b.cen $w.top.b.edg $w.top.b.out -side top \
              -anchor nw 


  button $w.bot.ok -text Ok -command {
     if {$Grid_frame == $old_frame} {set Grid_frame "!"}
     if {$Grid_pos == $old_pos} {set Grid_pos "!"}
     ImgExecWait gset \
     "switch=grid frame=$Grid_frame pos=$Grid_pos \
           accept" 
     ImgExecWaitNoMsg igui "action=update" 
     ImgExecWaitNoMsg igui "action=cache value=twod"
     if {$AutoRefresh == 1} {
       ImgExecWait idisplay " "
     }
     set Ok 1}

  bind $w <Return> {$W.bot.ok flash
                    $W.bot.ok invoke
                   }

  button $w.bot.reset -text Reset -command {ImgExecWait gset \
                                     "switch=grid off=yes" 
                             ImgExecWaitNoMsg igui "action=update"
                             ImgExecWaitNoMsg igui "action=cache value=twod"
                             if {$AutoRefresh == 1} {
                               ImgExecWait idisplay " "
                             }
                             set Ok 2}

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.reset $w.bot.cancel \
              -side left -expand 1 -padx 3m -padx 4m


#  set oldFocus [focus]
#  grab set $w
#  focus $w

  Centre_Window $w

  tkwait variable Ok
  destroy $w
#  focus $oldFocus
}
#
