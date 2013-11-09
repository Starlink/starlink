#
proc mark_list {w } {

  global Text_font Text_size Text_colour Text_bold Text_just
  global Number
  global SymbolFile  Symbol

  global W
  set W $w

  set Number N
  set Text_size 3.0
  set Text_colour 1
  set Text_bold 1

  toplevel $w -class Dialog
  wm title $w "Mark list"
  wm iconname $w Mark

  pack [frame $w.bot] -side bottom -fill both
  pack [frame $w.num] -side bottom -fill both -pady 2m
  pack [frame $w.size] -side bottom -fill both
  pack [frame $w.bold] -side bottom -fill both
  pack [frame $w.col] -side left -fill y
  pack [frame $w.sym] -side left -padx 1m

  label $w.sym.lbl -text Symbol
  frame $w.sym.list
  pack $w.sym.lbl -side top -anchor w
  pack $w.sym.list -side top

  listbox $w.sym.list.box -width 20 -yscrollcommand "$w.sym.list.scroll set"
  scrollbar $w.sym.list.scroll -command "$w.sym.list.box yview"
  set f [open $SymbolFile]
  while {[gets $f line]>=0} {
    $W.sym.list.box insert end $line
  }
  close $f
  pack $w.sym.list.box $w.sym.list.scroll -side left -fill y


  label $w.col.lbl -text Colour
  radiobutton $w.col.c0 -text "Black (bg)" -variable Text_colour -value 0
  radiobutton $w.col.c1 -text "White (fg)" -variable Text_colour -value 1
  radiobutton $w.col.c2 -text "Red" -variable Text_colour -value 2
  radiobutton $w.col.c3 -text "Green" -variable Text_colour -value 3
  radiobutton $w.col.c4 -text "Blue" -variable Text_colour -value 4
  radiobutton $w.col.c5 -text "Cyan" -variable Text_colour -value 5
  radiobutton $w.col.c6 -text "Magenta" -variable Text_colour -value 6
  radiobutton $w.col.c7 -text "Yellow" -variable Text_colour -value 7

  pack $w.col.lbl $w.col.c0 $w.col.c1 $w.col.c2 $w.col.c3 $w.col.c4 \
              $w.col.c5 $w.col.c6 $w.col.c7 -side top -anchor nw -padx 2m


  scale $w.size.ch -label "% Symbol Size" -showvalue yes \
       -orient horizontal -to 500 -from 1 -length 45m \
       -command "get_ch $w.size.ch Text_size"
  scale $w.bold.cb -label "Symbol Boldness" -showvalue yes \
       -orient horizontal -to 20 -from 1 -length 45m \
       -command "get_lw $w.bold.cb Text_bold"
  $w.size.ch set [expr $Text_size*100]
  $w.bold.cb set $Text_bold

  pack $w.size.ch 
  pack $w.bold.cb


  checkbutton $w.num.yn -text "Number positions" -variable Number \
                                            -onvalue "Y" -offvalue "N"
  pack $w.num.yn

  button $w.bot.ok -text Ok -command {
                                  set i [$W.sym.list.box curselection]
                                  if {$i==""} {
                                    set i 2
                                  }
                                  ImgExecWait imark "all=y number=$Number \
                                     colour=$Text_colour size=$Text_size \
                                     bold=$Text_bold symbol=$i accept"
                                  ImgExecWaitNoMsg igui "action=update"
                                  set Ok 1
                                 }

  button $w.bot.cancel -text Cancel -command {
                                              set Ok 999
                                             }

  pack $w.bot.ok $w.bot.cancel \
                -side left -expand 1 -padx 3m -padx 4m

  Centre_Window $w
  tkwait variable Ok
  destroy $w

}
#
