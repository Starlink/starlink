#
proc add_noise {w } {

  toplevel $w -class Dialog
  wm title $w "Noise"
  wm iconname $w Noise

  global Top
  set Top $w

  global AutoRefresh

  pack [frame $w.top] -side top
  pack [frame $w.bot] -side bottom

  pack [label $w.top.lbl -text "Noise level"] -side top


  global noise
  set noise 1
  scale $w.top.lev -showvalue yes \
       -orient horizontal -to 10 -from 1 -length 40m 
       
  $w.top.lev set $noise

  pack $w.top.lev -side top



  button $w.bot.ok -text Ok -command {
           set noise [$Top.top.lev get]  
           ImgExecWait inoise "sdev=$noise"
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
