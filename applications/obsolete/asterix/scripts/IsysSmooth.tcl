#
proc smooth_data {w } {

  toplevel $w -class Dialog
  wm title $w "Smooth"
  wm iconname $w Smooth

  global Top
  set Top $w

  global AutoRefresh

  pack [frame $w.top] -side top
  pack [frame $w.bot] -side bottom

  pack [frame $w.top.lbl1] [frame $w.top.filter] \
                        [frame $w.top.box] -side top

  pack [label $w.top.lbl1.text -text Filter]

  global filter
  set filter "Y"
  radiobutton $w.top.filter.gauss -text "Gaussian" \
                    -variable filter -value "Y" 
  radiobutton $w.top.filter.box -text "Box" \
                    -variable filter -value "N" 

  pack $w.top.filter.gauss $w.top.filter.box -side left

  global boxsize
  set boxsize 1
  scale $w.top.box.size -label "Smoothing level" -showvalue yes \
       -orient horizontal -to 10 -from 1 -length 40m 
       
  $w.top.box.size set $boxsize

  pack $w.top.box.size



  button $w.bot.ok -text Ok -command {
           set boxsize [$Top.top.box.size get]  
           ImgExecWait iblur "gauss=$filter xwid=$boxsize ywid=$boxsize"
           ImgExecWaitNoMsg istats " "
           if {$AutoRefresh == 1} {
             ImgExecWait idisplay " "
           }
           set Ok 1
          }
  bind $w <Return> {$Top.bot.ok flash
                    $Top.bot.ok invoke
                   }

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.cancel \
                  -side left -padx 3m -pady 2m

  Centre_Window $w

  tkwait variable Ok
  destroy $w
}
#
