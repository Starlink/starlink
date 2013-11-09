#
proc set_def {w } {

  ImgExecWaitNoMsg igui "action=synchro value=twod"
  ImgExecWaitNoMsg igui "action=update" 

  global W
  set W $w

  global AutoRefresh
  global nbid
  global Def_font Def_size Def_width Def_style Def_colour

set Def_font [nbs get $nbid.gcb.default_font]
if {$Def_font==""} {
  set Def_font 1}
set Def_size [nbs get $nbid.gcb.default_size]
if {$Def_size==""} {
  set Def_size 1.0 }
set Def_width [nbs get $nbid.gcb.default_width]
if {$Def_width==""} {
  set Def_width 1}
set Def_style [nbs get $nbid.gcb.default_style]
if {$Def_style==""} {
  set Def_style 1}
set Def_colour [nbs get $nbid.gcb.default_colour]
set Def_colour [string trim $Def_colour]
if {$Def_colour==""} {
  set Def_colour 1}

  toplevel $w -class Dialog
  wm title $w "Plotting defaults"
  wm iconname $w Defaults

  pack [frame $w.bot] -side bottom -fill both
  pack [frame $w.col] -side left -fill y
  pack [frame $w.size] -side bottom -fill both
  pack [frame $w.style] -side right
  pack [frame $w.font] -side left

  label $w.font.lbl -text Font
  radiobutton $w.font.f1 -text Normal -variable Def_font -value 1
  radiobutton $w.font.f2 -text Roman -variable Def_font -value 2
  radiobutton $w.font.f3 -text Italic -variable Def_font -value 3
  radiobutton $w.font.f4 -text Script -variable Def_font -value 4

  pack $w.font.lbl $w.font.f1 $w.font.f2 $w.font.f3 $w.font.f4 -side top \
              -anchor nw -padx 2m

  label $w.col.lbl -text Colour
  radiobutton $w.col.c0 -text "Black (bg)" -variable Def_colour -value 0
  radiobutton $w.col.c1 -text "White (fg)" -variable Def_colour -value 1
  radiobutton $w.col.c2 -text "Red" -variable Def_colour -value 2
  radiobutton $w.col.c3 -text "Green" -variable Def_colour -value 3
  radiobutton $w.col.c4 -text "Blue" -variable Def_colour -value 4
  radiobutton $w.col.c5 -text "Cyan" -variable Def_colour -value 5
  radiobutton $w.col.c6 -text "Magenta" -variable Def_colour -value 6
  radiobutton $w.col.c7 -text "Yellow" -variable Def_colour -value 7

  pack $w.col.lbl $w.col.c0 $w.col.c1 $w.col.c2 $w.col.c3 $w.col.c4 \
                  $w.col.c5 $w.col.c6 $w.col.c7 -side top -anchor nw -padx 2m

  label $w.style.lbl -text LineStyle
  radiobutton $w.style.s1 -text "______" -variable Def_style -value 1
  radiobutton $w.style.s2 -text "------" -variable Def_style -value 2
  radiobutton $w.style.s3 -text "_._._._" -variable Def_style -value 3
  radiobutton $w.style.s4 -text "........." -variable Def_style -value 4
  radiobutton $w.style.s5 -text "_..._..." -variable Def_style -value 5

  pack $w.style.lbl $w.style.s1 $w.style.s2 $w.style.s3 $w.style.s4 \
               $w.style.s5 -side top -anchor nw -padx 2m

  scale $w.size.ch -label "% Character Size" -showvalue yes \
       -orient horizontal -to 500 -from 1 -length 40m \
       -command {get_ch $w.size.ch Def_size}
  scale $w.size.lw -label "Line Width/Boldness" -showvalue yes \
       -orient horizontal -to 20 -from 1 -length 40m \
       -command {get_lw $w.size.lw Def_width}
  $w.size.ch set [expr $Def_size*100]
  $w.size.lw set $Def_width

  pack $w.size.ch $w.size.lw -side top -padx 2m -pady 2m

  button $w.bot.ok -text Ok -command {ImgExecWait gset \
     "switch=def font=$Def_font size=$Def_size width=$Def_width \
     style=$Def_style colour=$Def_colour" 
     ImgExecWaitNoMsg igui "action=update"
     ImgExecWaitNoMsg igui "action=cache value=twod"
     if {$AutoRefresh == 1} {
       ImgExecWait idisplay " "
     }
     set Ok 1
    }



  button $w.bot.reset -text Reset -command {ImgExecWait gset \
                                     "switch=def off=yes" 
                             ImgExecWaitNoMsg igui "action=update"
                             ImgExecWaitNoMsg igui "action=cache value=twod"
                             if {$AutoRefresh == 1} {
                               ImgExecWait idisplay " "
                             }
                             set Ok 2}

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.ok $w.bot.reset $w.bot.cancel \
                -side left -expand 1 -padx 3m -padx 4m


  tkwait variable Ok
  destroy $w
}
#
