#
proc circle_form {w } {

  toplevel $w -class Dialog
  wm title $w "Circle"
  wm iconname $w Circle

  global nbid
  global Xworld Yworld

  global Top
  set Top $w

  global Xc Yc Rad

  set Xc $Xworld
  set Yc $Yworld

  nbs put $nbid.flag 3

  pack [frame $w.xc] [frame $w.yc] [frame $w.rad] -side top
  pack [frame $w.bot] -side bottom

  label $w.xc.lbl -text "X (centre):"
  entry $w.xc.val -width 12 -textvariable Xc
  pack $w.xc.lbl $w.xc.val -side left
  label $w.yc.lbl -text "Y (centre):"
  entry $w.yc.val -width 12 -textvariable Yc
  pack $w.yc.lbl $w.yc.val -side left
  label $w.rad.lbl -text "    Radius:"
  entry $w.rad.val -width 12 -textvariable Rad
  pack $w.rad.lbl $w.rad.val -side left

  focus $Top.xc.val
  bind $w.xc.val <Return> {focus $Top.yc.val}
  bind $w.yc.val <Return> {focus $Top.rad.val}
  bind $w.rad.val <Return> {$Top.bot.ok flash
                            $Top.bot.ok invoke
                           }

  button $w.bot.ok -text Ok -command {
                                      nbs put $nbid.par_r1 $Xc
                                      nbs put $nbid.par_r2 $Yc
                                      nbs put $nbid.par_r3 $Rad
                                      nbs put $nbid.flag 1
                                      destroy $Top
                                     }
  button $w.bot.cancel -text Cancel -command {
                                              nbs put $nbid.flag -1
                                              destroy $Top
                                             }
  pack $w.bot.ok $w.bot.cancel -side left

  Centre_Window $w

}
#
#
#
proc annulus_form {w } {

  toplevel $w -class Dialog
  wm title $w "Annulus"
  wm iconname $w Annulus

  global nbid
  global Xworld Yworld

  global Top
  set Top $w

  global Xc Yc Irad Orad

  set Xc $Xworld
  set Yc $Yworld

  nbs put $nbid.flag 3

  pack [frame $w.xc] [frame $w.yc] [frame $w.irad] [frame $w.orad] -side top
  pack [frame $w.bot] -side bottom

  label $w.xc.lbl -text "  X (centre):"
  entry $w.xc.val -width 12 -textvariable Xc
  pack $w.xc.lbl $w.xc.val -side left
  label $w.yc.lbl -text "  Y (centre):"
  entry $w.yc.val -width 12 -textvariable Yc
  pack $w.yc.lbl $w.yc.val -side left
  label $w.irad.lbl -text "Inner radius:"
  entry $w.irad.val -width 12 -textvariable Irad
  pack $w.irad.lbl $w.irad.val -side left
  label $w.orad.lbl -text "Outer radius:"
  entry $w.orad.val -width 12 -textvariable Orad
  pack $w.orad.lbl $w.orad.val -side left

  focus $Top.xc.val
  bind $w.xc.val <Return> {focus $Top.yc.val}
  bind $w.yc.val <Return> {focus $Top.irad.val}
  bind $w.irad.val <Return> {focus $Top.orad.val}
  bind $w.orad.val <Return> {$Top.bot.ok flash
                            $Top.bot.ok invoke
                           }

  button $w.bot.ok -text Ok -command {
                                      nbs put $nbid.par_r1 $Xc
                                      nbs put $nbid.par_r2 $Yc
                                      nbs put $nbid.par_r3 $Irad
                                      nbs put $nbid.par_r4 $Orad
                                      nbs put $nbid.flag 1
                                      destroy $Top
                                     }
  button $w.bot.cancel -text Cancel -command {
                                              nbs put $nbid.flag -1
                                              destroy $Top
                                             }
  pack $w.bot.ok $w.bot.cancel -side left

  Centre_Window $w

}
#
#
#
proc box_form {w } {

  toplevel $w -class Dialog
  wm title $w "Box"
  wm iconname $w Box

  global nbid
  global Xworld Yworld

  global Top
  set Top $w

  global Xc Yc Wwid Ywid

  set Xc $Xworld
  set Yc $Yworld

  nbs put $nbid.flag 3

  pack [frame $w.xc] [frame $w.yc] [frame $w.xwid] [frame $w.ywid] -side top
  pack [frame $w.bot] -side bottom

  label $w.xc.lbl -text "   X (centre):"
  entry $w.xc.val -width 12 -textvariable Xc
  pack $w.xc.lbl $w.xc.val -side left
  label $w.yc.lbl -text "   Y (centre):"
  entry $w.yc.val -width 12 -textvariable Yc
  pack $w.yc.lbl $w.yc.val -side left
  label $w.xwid.lbl -text " Width (full):"
  entry $w.xwid.val -width 12 -textvariable Xwid
  pack $w.xwid.lbl $w.xwid.val -side left
  label $w.ywid.lbl -text "Height (full):"
  entry $w.ywid.val -width 12 -textvariable Ywid
  pack $w.ywid.lbl $w.ywid.val -side left

  focus $Top.xc.val
  bind $w.xc.val <Return> {focus $Top.yc.val}
  bind $w.yc.val <Return> {focus $Top.xwid.val}
  bind $w.xwid.val <Return> {focus $Top.ywid.val}
  bind $w.ywid.val <Return> {$Top.bot.ok flash
                            $Top.bot.ok invoke
                           }

  button $w.bot.ok -text Ok -command {
                                      nbs put $nbid.par_r1 $Xc
                                      nbs put $nbid.par_r2 $Yc
                                      nbs put $nbid.par_r3 $Xwid
                                      nbs put $nbid.par_r4 $Ywid
                                      nbs put $nbid.flag 1
                                      destroy $Top
                                     }
  button $w.bot.cancel -text Cancel -command {
                                              nbs put $nbid.flag -1
                                              destroy $Top
                                             }
  pack $w.bot.ok $w.bot.cancel -side left

  Centre_Window $w

}
#
#
#
proc slice_form {w } {

  toplevel $w -class Dialog
  wm title $w "Slice"
  wm iconname $w Slice

  global nbid
  global Xworld Yworld

  global Top
  set Top $w

  global Xc Yc Length Breadth Angle

  set Xc $Xworld
  set Yc $Yworld

  nbs put $nbid.flag 3

  pack [frame $w.xc] [frame $w.yc] [frame $w.len] [frame $w.wid] \
                [frame $w.ang]  -side top
  pack [frame $w.bot] -side bottom

  label $w.xc.lbl -text  "X (centre):"
  entry $w.xc.val -width 12 -textvariable Xc
  pack $w.xc.lbl $w.xc.val -side left
  label $w.yc.lbl -text  "Y (centre):"
  entry $w.yc.val -width 12 -textvariable Yc
  pack $w.yc.lbl $w.yc.val -side left
  label $w.len.lbl -text "    Length:"
  entry $w.len.val -width 12 -textvariable Length
  pack $w.len.lbl $w.len.val -side left
  label $w.wid.lbl -text "     Width:"
  entry $w.wid.val -width 12 -textvariable Breadth
  pack $w.wid.lbl $w.wid.val -side left
  label $w.ang.lbl -text "     Angle:"
  entry $w.ang.val -width 12 -textvariable Angle
  pack $w.ang.lbl $w.ang.val -side left

  focus $Top.xc.val
  bind $w.xc.val <Return> {focus $Top.yc.val}
  bind $w.yc.val <Return> {focus $Top.len.val}
  bind $w.len.val <Return> {focus $Top.wid.val}
  bind $w.wid.val <Return> {focus $Top.ang.val}
  bind $w.ang.val <Return> {$Top.bot.ok flash
                            $Top.bot.ok invoke
                           }

  button $w.bot.ok -text Ok -command {
                                      nbs put $nbid.par_r1 $Xc
                                      nbs put $nbid.par_r2 $Yc
                                      nbs put $nbid.par_r3 $Angle
                                      nbs put $nbid.par_r4 $Length
                                      nbs put $nbid.par_r5 $Breadth
                                      nbs put $nbid.flag 1
                                      destroy $Top
                                     }
  button $w.bot.cancel -text Cancel -command {
                                              nbs put $nbid.flag -1
                                              destroy $Top
                                             }
  pack $w.bot.ok $w.bot.cancel -side left

  Centre_Window $w

}
#
#
#
proc posit_form {w } {

  toplevel $w -class Dialog
  wm title $w "Position"
  wm iconname $w Position

  global nbid

  global Top
  set Top $w

  global FilePos

  global Xcoord Ycoord
  set Xcoord ""
  set Ycoord ""

  global Frame
  global Xlbl Ylbl
  trace variable Frame w ChangeFrame

  global Record
  trace variable Record w ListChoice

  proc ChangeFrame {name element op} {
    global Frame Xlbl Ylbl
    if {$Frame==1} {
      set Xlbl "X"
      set Ylbl "Y"
    } elseif {$Frame==2} {
      set Xlbl "Ra"
      set Ylbl "Dec"
    } elseif {$Frame==3} {
      set Xlbl "Ecl.Long."
      set Ylbl "Ecl.Lat."
    } elseif {$Frame==4} {
      set Xlbl "L"
      set Ylbl "B"
    }
  }


  proc ListChoice {name element op} {
    global Record Frame Xcoord Ycoord
    set Frame 2
    set Xcoord [string trim [string range $Record 16 27] ]
    set Ycoord [string trim [string range $Record 28 39] ]
  }

  proc read_pos_list {list} {
    global FilePos

    set n [$list size]
    $list delete 0 $n
    if {[file exists $FilePos] == 1} {
      set f [open $FilePos]
      while {![eof $f]} {
        $list insert end [gets $f]
      }
    }
  }


  nbs put $nbid.flag 3

  pack [frame $w.xc] [frame $w.yc] [frame $w.rad] -side top
  pack [frame $w.top] -side top
  pack [frame $w.mid] -side top
  pack [frame $w.bot] -side bottom
  pack [frame $w.top.left] [frame $w.top.right] -side left

  set Frame 1
  radiobutton $w.top.left.xy -text "x/y" -variable Frame -value 1
  radiobutton $w.top.left.radec -text "Ra/Dec" -variable Frame -value 2
  radiobutton $w.top.left.ecl -text "Ecliptic" -variable Frame -value 3
  radiobutton $w.top.left.gal -text "Galactic" -variable Frame -value 4
  pack $w.top.left.xy $w.top.left.radec $w.top.left.ecl $w.top.left.gal \
                             -side top -anchor nw

  pack [frame $w.top.right.x] [frame $w.top.right.y] -side top

  label $w.top.right.x.lbl -width 8 -textvariable Xlbl
  entry $w.top.right.x.val -width 12 -textvariable Xcoord
  pack $w.top.right.x.lbl $w.top.right.x.val -side left
  label $w.top.right.y.lbl -width 8 -textvariable Ylbl
  entry $w.top.right.y.val -width 12 -textvariable Ycoord
  pack $w.top.right.y.lbl $w.top.right.y.val -side left

  listbox $w.mid.list -width 40 -yscrollcommand "$w.mid.scroll set"
  scrollbar $w.mid.scroll -command "$w.mid.list yview"

  read_pos_list $w.mid.list


  pack $w.mid.list $w.mid.scroll -side left -fill y

  bind $w.mid.list <ButtonRelease-1> {set Record [selection get]}



  focus $Top.top.right.x.val
  bind $w.top.right.x.val <Return> {focus $Top.top.right.y.val}
  bind $w.top.right.y.val <Return> {$Top.bot.ok flash
                                    $Top.bot.ok invoke
                                   }

  button $w.bot.ok -text Ok -command {
                                      if {$Frame==1} {
                                        nbs put $nbid.par_r1 $Xcoord
                                        nbs put $nbid.par_r2 $Ycoord
                                      } elseif {$Frame==2} {
                                        nbs put $nbid.par_c1 $Xcoord
                                        nbs put $nbid.par_c2 $Ycoord
                                      } elseif {$Frame==3} {
                                        nbs put $nbid.par_d1 $Xcoord
                                        nbs put $nbid.par_d2 $Ycoord
                                      } elseif {$Frame==4} {
                                        nbs put $nbid.par_d1 $Xcoord
                                        nbs put $nbid.par_d2 $Ycoord
                                      }
                                      nbs put $nbid.par_i1 $Frame
                                      nbs put $nbid.flag 1
                                      destroy $Top
                                     }

  button $w.bot.import -text Import -command {
                                              nbs put $nbid.flag -1
                                              load_pos_list .loadpos_dialogue
                                              read_pos_list $Top.mid.list
                                              nbs put $nbid.options "FORM"
                                              nbs put $nbid.flag 0
                                              update idletasks
                                              ImgExecWait iposit "mode=point"
                                             }
  button $w.bot.clear -text Clear -command {
                                              nbs put $nbid.flag -1
                                              ImgExecWaitNoMsg iposit \
                                                              "mode=clear"
                                              if {[file exists $FilePos]} {
                                                exec rm $FilePos
                                              }
                                              set n [$Top.mid.list size]
                                              $Top.mid.list delete 0 $n
                                              nbs put $nbid.options "FORM"
                                              nbs put $nbid.flag 0
                                              update idletasks
                                              ImgExecWait iposit "mode=point"
                                             }
  button $w.bot.cancel -text Cancel -command {
                                              nbs put $nbid.flag -1
                                              destroy $Top
                                             }
  pack $w.bot.ok $w.bot.import $w.bot.clear $w.bot.cancel -side left

  Centre_Window $w

}
#
