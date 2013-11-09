#
proc set_scale {w } {

  toplevel $w -class Dialog
  wm title $w "Scaling"
  wm iconname $w Scaling

  global W
  set W $w
  global Min Max
  global nbid
  global slo sho
  global srange scale
  global Pix_scaling Pix_min Pix_max

# get current settings
  set Pix_scaling [nbs get $nbid.gcb.pix_scaling]
  set Pix_scaling [string tolower $Pix_scaling]
  if {$Pix_scaling == ""} {
    set Pix_scaling lin
  } else {
    set Pix_scaling [string range $Pix_scaling 0 2]
    set Pix_scaling [string tolower $Pix_scaling]
  }
  set scale $Pix_scaling
  set Pix_min [nbs get $nbid.gcb.pix_min]
  if {$Pix_min == ""} {
    set Pix_min $Min
  }
  set Pix_max [nbs get $nbid.gcb.pix_max]
  if {$Pix_max == ""} {
    set Pix_max $Max
  }

  pack [frame $w.top] -side top
  pack [frame $w.bot] -side bottom
  pack [frame $w.top.left] -side left
  pack [frame $w.top.right] -side right

  radiobutton $w.top.left.lin -text Linear -variable scale -value lin
  radiobutton $w.top.left.log -text Logarithmic -variable scale -value log
  radiobutton $w.top.left.sqr -text SquareRoot -variable scale -value sqr
  radiobutton $w.top.left.par -text Parabolic -variable scale -value par
  radiobutton $w.top.left.sin -text Sine -variable scale -value sin
  radiobutton $w.top.left.his -text Histogram -variable scale -value his

  pack $w.top.left.lin $w.top.left.log $w.top.left.sqr \
       $w.top.left.par $w.top.left.sin $w.top.left.his \
           -side top -anchor w -expand yes

  pack [frame $w.top.right.lo] [frame $w.top.right.hi] -side top -anchor n

  label $w.top.right.lo.lbl -text "Lo-limit"
  entry $w.top.right.lo.val -width 20 -textvariable Pix_min
  pack $w.top.right.lo.lbl $w.top.right.lo.val -side top

  label $w.top.right.hi.lbl -text "Hi-limit"
  entry $w.top.right.hi.val -width 20 -textvariable Pix_max
  pack $w.top.right.hi.lbl $w.top.right.hi.val -side top


  button $w.bot.ok -text Ok -command {
                             set smax $Pix_max
                             set smin $Pix_min
                             ImgExecWait iscale \
                               "scaling=$scale min=$smin max=$smax"
                             if {$AutoRefresh == 1} {
                               ImgExecWait idisplay " "
                             }
                             ImgExecWaitNoMsg igui "action=update"
                             set Ok 1
                            }

  bind $w <Return> {$W.bot.ok flash
                    $W.bot.ok invoke
                   }

  button $w.bot.reset -text Reset -command {
                              ImgExecWait iscale \
                                "scaling=lin min=$Min max=$Max"
                              if {$AutoRefresh == 1} {
                                ImgExecWait idisplay " "
                              }
                              ImgExecWaitNoMsg igui "action=update"
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
