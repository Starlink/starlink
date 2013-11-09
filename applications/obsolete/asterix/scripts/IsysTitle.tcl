#
proc set_title {w } {

  ImgExecWaitNoMsg igui "action=synchro value=twod"
  ImgExecWaitNoMsg igui "action=update" 

  global W
  set W $w

  global AutoRefresh
  global nbid
  global Text_font Text_size Text_bold Text_colour Text_just
  global Text_format_done
  global title_n title_text title_font title_bold \
         title_size title_colour title_just title_from title_to

  set title_n [nbs get $nbid.gcb.title_n]
  set title_n [string trim $title_n]
  if {$title_n==""} {
    set title_n 0
  }
  nbs put $nbid.flag -1
  ImgExecNoWait igui "action=getstruct value=title" 
  update idletasks
  for {set i 1} {$i<=$title_n} {incr i} {
    while {[nbs get $nbid.flag]==-1} {}
    set title_text($i) [nbs get $nbid.par_c1]
    set title_font($i) [nbs get $nbid.par_i2]
    set title_bold($i) [nbs get $nbid.par_i3]
    set title_size($i) [nbs get $nbid.par_r1]
    set title_colour($i) [nbs get $nbid.par_i4]
    set title_just($i) [nbs get $nbid.par_c2]
    set title_just($i) [string trim $title_just($i)]
    update idletasks
    nbs put $nbid.flag -1
  }
  update idletasks

  proc set_title_update {box} {
    global title_n title_text title_font title_bold \
           title_size title_colour title_just
    set n [$box size]
    $box delete 0 $n
    for {set i 1} {$i<=$title_n} {incr i}  {
       $box insert end $title_text($i)
    }
  }
#
#
  proc set_title_delete {box} {
    global title_n title_text title_font title_bold \
            title_size title_colour title_just
    set line [$box curselection]
    if {$line!=""} {
      incr line
      for {set i $line} {$i<$title_n} {incr i}  {
         set j [expr $i+1]
         set title_text($i) $title_text($j)
         set title_font($i) $title_font($j)
         set title_bold($i) $title_bold($j)
         set title_size($i) $title_size($j)
         set title_colour($i) $title_colour($j)
         set title_just($i) $title_just($j)
      }
      set title_n [expr $title_n-1]
    }
    
  }
#
  proc set_title_insert {box} {
    global title_n title_text title_font title_bold \
            title_size title_colour title_just
    global Text_text
    set line [$box curselection]
    if {$title_n==0} {
      set line 1
      set title_n 1
      set last 0
    } elseif {$line==""} {
      set last $title_n
      set line 1
      incr title_n
    } else {
      set last $title_n
      incr line
      incr title_n
    }
    for {set i $last} {$i>=$line} {incr i -1}  {
       set j [expr $i+1]
       set title_text($j) $title_text($i)
       set title_font($j) $title_font($i)
       set title_bold($j) $title_bold($i)
       set title_size($j) $title_size($i)
       set title_colour($j) $title_colour($i)
       set title_just($j) $title_just($i)
    }
    set title_text($line) $Text_text
    set title_font($line) 1
    set title_bold($line) 1
    set title_size($line) 1.0
    set title_just($line) "L"
    set title_colour($line) 1
  }

  proc set_title_format {a b c} {
    global Text_font Text_size Text_bold Text_colour Text_just
    global title_n title_text title_font title_bold \
            title_size title_colour title_just title_from title_to

    for {set i $title_from} {$i<=$title_to} {incr i} {
      set title_font($i) $Text_font
      set title_size($i) $Text_size
      set title_bold($i) $Text_bold
      set title_colour($i) $Text_colour
      set title_just($i) $Text_just
    }
  }

  toplevel $w -class Dialog
  wm title $w "Title"
  wm iconname $w Title

  pack [frame $w.top] -side top
  pack [frame $w.entry] -side top
  pack [frame $w.choose] -side top
  pack [frame $w.bot] -side bottom


  button $w.top.format -text Format -command {
                                set i [$W.choose.box curselection]
                                if {$i!=""} {
                                  incr i
                                  set Text_font $title_font($i)
                                  set Text_size $title_size($i)
                                  set Text_bold $title_bold($i)
                                  set Text_colour $title_colour($i)
                                  set Text_just $title_just($i)
                                  set title_from $i
                                  set title_to $i
                                  Format_Text .txtfmt_dialogue
                                }
                               }
  button $w.top.formatall -text "Format All" -command {
                                          set Text_font 1
                                          set Text_size 1.0
                                          set Text_bold 1
                                          set Text_colour 1
                                          set Text_just "L"
                                          set title_from 1
                                          set title_to $title_n
                                          Format_Text .txtfmt_dialogue
                                         }

  trace variable Text_format_done w set_title_format

  button $w.top.clear -text Clear -command {set Text_text ""}
  button $w.top.delete -text Delete -command {
                                              set_title_delete $W.choose.box
                                              set_title_update $W.choose.box
                                             }
  button $w.top.insert -text Insert -command {
                                              set_title_insert $W.choose.box
                                              set_title_update $W.choose.box
                                             }
  button $w.top.add -text Add -command {
                                  if {$title_n==""} {
                                    set title_n 1
                                  } else {
                                    incr title_n
                                  }
                                  set title_text($title_n) $Text_text
                                  set title_font($title_n) 1
                                  set title_bold($title_n) 1
                                  set title_size($title_n) 1.0
                                  set title_just($title_n) "L"
                                  set title_colour($title_n) 1
                                  set_title_update $W.choose.box
                                  set Text_text ""
                                 }

  pack $w.top.format $w.top.formatall $w.top.clear $w.top.delete \
          $w.top.insert $w.top.add -side left -fill both

  entry $w.entry.text -textvariable Text_text -width 60
  pack $w.entry.text -side left
  bind $w.entry.text <Return> {$W.top.add invoke}

  listbox $w.choose.box -yscrollcommand "$w.choose.scroll set" -width 60
  scrollbar $w.choose.scroll -command "$w.choose.box yview"
  if {$title_n>0} {
    set_title_update $w.choose.box
  }
  pack $w.choose.box $w.choose.scroll -side left -fill y
  bind $w.choose.box <Double-Button-1> {
                                 set i [$W.choose.box curselection]
                                 if {$i!=""} {
                                   incr i
                                   set Text_text $title_text($i)
                                 }
                                }

  button $w.bot.ok -text Close -command {
                           if {$title_n==0} {
                             ImgExecWait gset "switch=title off=yes" 
                           } else {
                             nbs put $nbid.par_i1 $title_n
                             nbs put $nbid.flag -1
                             ImgExecNoWait igui \
                                  "action=putstruct value=title"
                             update idletasks
                             for {set i 1} {$i<=$title_n} {incr i} {
                               nbs put $nbid.par_c1 $title_text($i)
                               nbs put $nbid.par_i2 $title_font($i)
                               nbs put $nbid.par_i3 $title_bold($i)
                               nbs put $nbid.par_r1 $title_size($i)
                               nbs put $nbid.par_i4 $title_colour($i)
                               nbs put $nbid.par_c2 $title_just($i)
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
                               "switch=title off=yes" 
                             set title_n 0
                             set_title_update $W.choose.box
                            }

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.reset $w.bot.cancel \
                -side left -expand 1 -padx 3m -padx 4m

  Centre_Window $w
  set oldfocus [focus]
  focus $w.entry.text

  tkwait variable Ok
  trace vdelete Text_format_done w set_title_format
  focus $oldfocus
  destroy $w
}
#
