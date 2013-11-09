#
proc set_contour {w } {

  toplevel $w -class Dialog
  wm title $w "Contours"
  wm iconname $w Contours

  ImgExecWaitNoMsg igui "action=synchro value=twod"
  ImgExecWaitNoMsg igui "action=update" 

  global W
  set W $w
  global Min Max
  global levels
  set levels " "
  pack [frame $w.top] -side top
  pack [frame $w.bot] -side bottom
  pack [frame $w.top.row1] [frame $w.top.row2] [frame $w.top.row3] -side top

  label $w.top.row1.lbl1 -text "Levels:"
  entry $w.top.row1.lev -width 25 -textvariable levels

  pack $w.top.row1.lbl1 $w.top.row1.lev -side left -pady 2m -padx 2m

  global lolev hilev
  label $w.top.row2.lbl1 -text "         From:"
  entry $w.top.row2.lolev -width 10 -textvariable lolev
  label $w.top.row2.lbl2 -text "      To:"
  entry $w.top.row2.hilev -width 10 -textvariable hilev
  set lolev $Min
  set hilev $Max

  pack $w.top.row2.lbl1 $w.top.row2.lolev \
     $w.top.row2.lbl2 $w.top.row2.hilev -side left -pady 2m -padx 2m

  global nlev levint
  set nlev " "
  set levint " "
  label $w.top.row3.lbl1 -text "No. of Levels:"
  entry $w.top.row3.nlev -width 10 -textvariable nlev
  label $w.top.row3.lbl2 -text "Interval:"
  entry $w.top.row3.levint -width 10 -textvariable levint

  proc set_contour_nlev {} {
    global levels nlev levint hilev lolev
    global Max Min

# exclude extremes of data range
    if {($hilev==$Max) && ($lolev==$Min)} {
      set levint [expr ($hilev-$lolev)/($nlev+1)]
      set first [expr $lolev+$levint]
    } elseif {($hilev==$Max) && ($lolev!=$Min)} {
      set levint [expr ($hilev-$lolev)/($nlev)]
      set first $lolev
    } elseif {($hilev!=$Max) && ($lolev==$Min)} {
      set levint [expr ($hilev-$lolev)/($nlev)]
      set first [expr $hilev-($nlev-1)*$levint]
    } elseif {($hilev!=$Max) && ($lolev!=$Min)} {
      set levint [expr ($hilev-$lolev)/($nlev-1)]
      set first $lolev
    }
    set levels ""
    for {set ilev 1} {$ilev<=$nlev} {incr ilev 1} {
      set lev [expr $first + ($ilev-1)*$levint]
      append levels "$lev "
    }
  }

  proc set_contour_int {} {
    global levels nlev levint hilev lolev
    global Max Min

# exclude extremes of data range
    if {($hilev==$Max) && ($lolev==$Min)} {
      set first [expr $lolev+$levint]
      set last [expr $hilev-$levint]
    } elseif {($hilev==$Max) && ($lolev!=$Min)} {
      set first $lolev
      set last [expr $hilev-$lolev]
    } elseif {($hilev!=$Max) && ($lolev==$Min)} {
      set first [expr $lolev+$levint]
      set last $hilev
    } elseif {($hilev!=$Max) && ($lolev!=$Min)} {
      set first $lolev
      set last $hilev
    }
    set lev $first
    set levels ""
    while {$lev<=$last} {
      append levels "$lev "
      set lev [expr $lev+$levint]
    }
  }



  bind $w.top.row3.nlev <Return> {set_contour_nlev}
  bind $w.top.row3.levint <Return> {set_contour_int}
  bind $w.top.row3.nlev <Double-Button-1> {set_contour_nlev}
  bind $w.top.row3.levint <Double-Button-1> {set_contour_int}

  pack $w.top.row3.lbl1 $w.top.row3.nlev $w.top.row3.lbl2 $w.top.row3.levint \
                                        -side left -pady 2m -padx 2m

  button $w.bot.ok -text Ok -command {
             if {$levels == " "} {
               if {$nlev != ""} {
                 set_contour_nlev
               }
             }
             ImgExecWait icontour "levels=\[$levels\]"
             set Ok 1
            }

  bind $w.top.row1.lev <Return> {$W.bot.ok flash
                                 $W.bot.ok invoke
                   }

  button $w.bot.reset -text Reset -command {
                            ImgExecWait gset "switch=cont off=yes"
                            ImgExecWaitNoMsg igui "action=update" 
                            ImgExecWaitNoMsg igui "action=cache value=twod"
                            if {$AutoRefresh == 1} {
                              ImgExecWait idisplay " "
                            }
                            set Ok 2
                          }

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.reset $w.bot.cancel \
                     -side left -padx 3m -pady 2m

  Centre_Window $w

  tkwait variable Ok
  destroy $w
}
#
