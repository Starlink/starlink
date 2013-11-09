#
proc set_note {w } {

  ImgExecWaitNoMsg igui "action=synchro value=twod"
  ImgExecWaitNoMsg igui "action=update" 

  global W
  set W $w

  global AutoRefresh
  global nbid
  global set_note_pos_DONE
  global Text_font Text_size Text_bold Text_colour Text_just
  global Text_format_done
  global note_n note_text note_x note_y note_angle note_font note_bold \
                                 note_size note_colour note_from note_to

  set note_n [nbs get $nbid.gcb.note_n]
  set note_n [string trim $note_n]
  if {$note_n==""} {
    set note_n 0
  }
  nbs put $nbid.flag -1
  ImgExecNoWait igui "action=getstruct value=note" 
  update idletasks
  for {set i 1} {$i<=$note_n} {incr i} {
    while {[nbs get $nbid.flag]==-1} {}
    set note_text($i) [nbs get $nbid.par_c1]
    set note_font($i) [nbs get $nbid.par_i2]
    set note_bold($i) [nbs get $nbid.par_i3]
    set note_colour($i) [nbs get $nbid.par_i4]
    set note_size($i) [nbs get $nbid.par_r1]
    set note_x($i) [nbs get $nbid.par_r2]
    set note_y($i) [nbs get $nbid.par_r3]
    set note_angle($i) [nbs get $nbid.par_r4]
    update idletasks
    nbs put $nbid.flag -1
  }
  update idletasks

  proc set_note_update {box} {
  global note_n note_text note_x note_y note_angle note_font note_bold \
                                 note_size note_colour note_from note_to
    set n [$box size]
    $box delete 0 $n
    for {set i 1} {$i<=$note_n} {incr i}  {
       $box insert end $note_text($i)
    }
  }
#
#
  proc set_note_delete {box} {
  global note_n note_text note_x note_y note_angle note_font note_bold \
                                 note_size note_colour note_from note_to
    set line [$box curselection]
    if {$line!=""} {
      incr line
      for {set i $line} {$i<$note_n} {incr i}  {
         set j [expr $i+1]
         set note_text($i) $note_text($j)
         set note_font($i) $note_font($j)
         set note_bold($i) $note_bold($j)
         set note_size($i) $note_size($j)
         set note_colour($i) $note_colour($j)
         set note_just($i) $note_just($j)
      }
      set note_n [expr $note_n-1]
    }
    
  }
#

  proc set_note_format {a b c} {
    global Text_font Text_size Text_bold Text_colour Text_just
    global note_n note_text note_x note_y note_angle note_font note_bold \
                                 note_size note_colour note_from note_to

    for {set i $note_from} {$i<=$note_to} {incr i} {
      set note_font($i) $Text_font
      set note_size($i) $Text_size
      set note_bold($i) $Text_bold
      set note_colour($i) $Text_colour
    }
  }

  proc set_note_pos {text size angle} {

    global nbid
    global Height Width
    global Xmin Xmax Ymin Ymax
    global Wxmin Wxmax Wymin Wymax
    ImgExecWaitNoMsg igui "action=transform"
    set Xmin [nbs get $nbid.xmin]
    set Xmax [nbs get $nbid.xmax]
    set Ymin [nbs get $nbid.ymin]
    set Ymax [nbs get $nbid.ymax]
    set Wxmin [nbs get $nbid.wxmin]
    set Wxmax [nbs get $nbid.wxmax]
    set Wymin [nbs get $nbid.wymin]
    set Wymax [nbs get $nbid.wymax]

    global W
    global Text_x Text_y
    global canv
    global X1 Y1 X2 Y2 X3 Y3 X4 Y4

    global set_note_pos_DONE
    raise . $W

    set ch [expr $size*$Height/60.0]
    set cw [expr $ch*0.8]
    set text [string trim $text]
    set nc [string length $text]
    set h $ch
    set l [expr $nc*$cw]
    set X1 [expr $Width/2]
    set Y1 [expr $Height/2]
    if {($angle<0.1) && ($angle>-0.1)} {
      set X2 [expr $X1+$l]
      set Y2 $Y1
      set X3 $X2
      set Y3 [expr $Y1-$h]
      set X4 $X1
      set Y4 $Y3

    } else {
      set ang [expr $angle*0.017453]
      set X2 [expr $X1+$l*cos($ang)]
      set Y2 [expr $Y1-$l*sin($ang)]
      set X3 [expr $X2-$h*sin($ang)]
      set Y3 [expr $Y2-$h*cos($ang)]
      set X4 [expr $X1-$h*sin($ang)]
      set Y4 [expr $Y1-$h*cos($ang)]
    }

    $canv configure -cursor crosshair

    $canv create polygon $X1 $Y1 $X2 $Y2 $X3 $Y3 $X4 $Y4 -tags slice \
                                    -outline white -smooth no

    bind $canv <Motion> {set_note_pos_move $canv slice %x %y}

    proc set_note_pos_move {c s x y} {
      global X1 Y1 X2 Y2 X3 Y3 X4 Y4
      set dx [expr $x-$X1]
      set dy [expr $y-$Y1]
      set X1 [expr $X1+$dx]
      set Y1 [expr $Y1+$dy]
      lappend vert $X1 $Y1
      set X2 [expr $X2+$dx]
      set Y2 [expr $Y2+$dy]
      lappend vert $X2 $Y2
      set X3 [expr $X3+$dx]
      set Y3 [expr $Y3+$dy]
      lappend vert $X3 $Y3
      set X4 [expr $X4+$dx]
      set Y4 [expr $Y4+$dy]
      lappend vert $X4 $Y4
      return [eval "$c coords $s $vert"]
    }

    bind $canv <ButtonPress-1> {
                                raise $W .
                                $canv delete slice
                                Gui2World %x %y x y
                                set Text_x $x
                                set Text_y $y
                                bind $canv <Motion> {}
                                bind $canv <ButtonPress-1> {}
                                $canv configure -cursor arrow
                                set set_note_pos_DONE 1
                               }

  }

  trace variable set_note_pos_DONE w set_note_pos_set

  proc set_note_pos_set {a b c} {

    global W
    global note_x note_y
    global Text_x Text_y

    set i [$W.choose.box curselection]
    if {$i!=""} {
      incr i
      set note_x($i) $Text_x
      set Text_x ""
      set note_y($i) $Text_y
      set Text_y ""
    }

  }

  toplevel $w -class Dialog
  wm title $w "Annotate"
  wm iconname $w Annotate

  pack [frame $w.top] -side top
  pack [frame $w.entry] -side top
  pack [frame $w.pos] -side top
  pack [frame $w.choose] -side top
  pack [frame $w.bot] -side bottom


  button $w.top.format -text Format -command {
                                set i [$W.choose.box curselection]
                                if {$i!=""} {
                                  incr i
                                  set Text_font $note_font($i)
                                  set Text_size $note_size($i)
                                  set Text_bold $note_bold($i)
                                  set Text_colour $note_colour($i)
                                  set note_from $i
                                  set note_to $i
                                  Format_Text .txtfmt_dialogue
                                }
                               }
  button $w.top.formatall -text "Format All" -command {
                                          set Text_font 1
                                          set Text_size 1.0
                                          set Text_bold 1
                                          set Text_colour 1
                                          set Text_just "L"
                                          set note_from 1
                                          set note_to $note_n
                                          Format_Text .txtfmt_dialogue
                                         }

  trace variable Text_format_done w set_note_format

  button $w.top.clear -text Clear -command {
                                            set Text_text ""
                                            set Text_x ""
                                            set Text_y ""
                                            set Text_angle 0.0
                                           }
  button $w.top.delete -text Delete -command {
                                              set_note_delete $W.choose.box
                                              set_note_update $W.choose.box
                                             }
  button $w.top.add -text Add -command {
                                  if {$note_n==""} {
                                    set note_n 1
                                  } else {
                                    incr note_n
                                  }
                                  set note_text($note_n) $Text_text
                                  set note_font($note_n) 1
                                  set note_bold($note_n) 1
                                  set note_size($note_n) 1.0
                                  set note_just($note_n) "L"
                                  set note_colour($note_n) 1
                                  if {$Text_x==""} {
                                    set note_x($note_n) 0.0
                                  } else {
                                    set note_x($note_n) $Text_x
                                  }
                                  if {$Text_y==""} {
                                    set note_y($note_n) 0.0
                                  } else {
                                    set note_y($note_n) $Text_y
                                  }
                                  if {$Text_angle==""} {
                                    set note_angle($note_n) 0.0
                                  } else {
                                    set note_angle($note_n) $Text_angle
                                  }
                                  set_note_update $W.choose.box
                                  $W.top.clear invoke
                                 }

  button $w.top.position -text Position -command {
                                set i [$W.choose.box curselection]
                                if {$i!=""} {
                                  incr i
                                  set text $note_text($i)
                                  set angle $note_angle($i)
                                  set size $note_size($i)
                                } else {
                                  set text $Text_text
                                  set size 1.0
                                  if {$Text_angle==""} {
                                    set angle 0.0
                                  } else {
                                    set angle $Text_angle
                                  }
                                }
                                set_note_pos $text $size $angle
                               }

  pack $w.top.format $w.top.formatall $w.top.clear $w.top.delete \
         $w.top.add $w.top.position -side left -fill both

  entry $w.entry.text -textvariable Text_text -width 60
  pack $w.entry.text -side left
  bind $w.entry.text <Return> {focus $W.pos.x}
  set Text_text ""

  label $w.pos.xlbl -text "X"
  entry $w.pos.x -textvariable Text_x -width 12
  label $w.pos.ylbl -text "   Y"
  bind $w.pos.x <Return> {focus $W.pos.y}
  set Text_x ""

  entry $w.pos.y -textvariable Text_y -width 12
  label $w.pos.albl -text "   Angle"
  bind $w.pos.y <Return> {focus $W.pos.a}
  set Text_y ""

  entry $w.pos.a -textvariable Text_angle -width 12
  pack $w.pos.xlbl $w.pos.x $w.pos.ylbl $w.pos.y \
                  $w.pos.albl $w.pos.a -side left -fill x
  bind $w.pos.a <Return> {
                          $W.top.add invoke
                          focus $W.entry.text
                         }
  set Text_angle 0.0

  listbox $w.choose.box -yscrollcommand "$w.choose.scroll set" -width 60
  scrollbar $w.choose.scroll -command "$w.choose.box yview"
  if {$note_n>0} {
    set_note_update $w.choose.box
  }
  pack $w.choose.box $w.choose.scroll -side left -fill y
  bind $w.choose.box <Double-Button-1> {
                                 set i [$W.choose.box curselection]
                                 if {$i!=""} {
                                   incr i
                                   set Text_text $note_text($i)
                                   set Text_x $note_x($i)
                                   set Text_y $note_y($i)
                                   set Text_angle $note_angle($i)
                                 }
                                }

  button $w.bot.ok -text Close -command {
                           if {$note_n==0} {
                             ImgExecWait gset "switch=note off=yes" 
                           } else {
                             nbs put $nbid.par_i1 $note_n
                             nbs put $nbid.flag -1
                             ImgExecNoWait igui \
                                  "action=putstruct value=note"
                             update idletasks
                             for {set i 1} {$i<=$note_n} {incr i} {
                               nbs put $nbid.par_c1 $note_text($i)
                               nbs put $nbid.par_i2 $note_font($i)
                               nbs put $nbid.par_i3 $note_bold($i)
                               nbs put $nbid.par_r1 $note_size($i)
                               nbs put $nbid.par_i4 $note_colour($i)
                               nbs put $nbid.par_r2 $note_x($i)
                               nbs put $nbid.par_r3 $note_y($i)
                               nbs put $nbid.par_r4 $note_angle($i)
                               nbs put $nbid.flag 0
                               while {[nbs get $nbid.flag]==0} {}
                             }
                           }
                           ImgExecWaitNoMsg igui "action=update"
                           ImgExecWaitNoMsg igui "action=cache value=twod"
                           if {$AutoRefresh == 1} {
                             ImgExecWait idisplay " "
                           }
                           set Ok 1
                          }

  button $w.bot.reset -text Reset -command {
                             ImgExecWait gset \
                               "switch=note off=yes" 
                             set note_n 0
                             set_note_update $W.choose.box
                            }

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.reset $w.bot.cancel \
                -side left -expand 1 -padx 3m -padx 4m

  Centre_Window $w
  set oldfocus [focus]
  focus $w.entry.text

  tkwait variable Ok
  trace vdelete set_note_pos_DONE w set_note_pos_set
  trace vdelete Text_format_done w set_note_format
  focus $oldfocus
  destroy $w
}
#
