proc set_axes {w } {

  ImgExecWaitNoMsg igui "action=transform" 
  ImgExecWaitNoMsg igui "action=synchro value=twod"
  ImgExecWaitNoMsg igui "action=update" 

  global W
  set W $w

  global AutoRefresh

  global nbid
  global Axes_radec Axes_scaling  
  global Axes_bold Axes_width Axes_colour Axes_font Axes_size
  global Xaxis_opt Yaxis_opt Xaxis_lo Xaxis_hi Yaxis_lo Yaxis_hi
  global Xaxis_log Yaxis_log Xaxis_tick Yaxis_tick Xaxis_div Yaxis_div

  global axes_form axes_cross axes_grid
  global xopt_tick xopt_subtick xopt_num 
  global yopt_tick yopt_subtick yopt_num 

  global old_size old_bold old_width
  global old_xlo old_xhi old_ylo old_yhi
  global old_xtick old_xdiv old_ytick old_ydiv
  global old_xopt old_yopt
  global xinc yinc

#  get current values from noticeboard
  set Axes_radec [nbs get $nbid.gcb.axes_radec]
  set Axes_scaling [nbs get $nbid.gcb.axes_scaling]
  set Axes_font [nbs get $nbid.gcb.axes_font]
  set Axes_size [nbs get $nbid.gcb.axes_size]
  if {$Axes_size==""} {
    set old_size 1.0
    set Axes_size 1.0
  } else {
    set old_size $Axes_size
  }
  set Axes_bold [nbs get $nbid.gcb.axes_bold]
  set Axes_bold [string trim $Axes_bold]
  if {$Axes_bold==""} {
    set old_bold 1
    set Axes_bold 1
  } else {
    set old_bold $Axes_bold
  }
  set Axes_width [nbs get $nbid.gcb.axes_width]
  if {$Axes_width==""} {
    set old_width 1.0
    set Axes_width 1.0
  } else {
    set old_width $Axes_width
  }
  set Axes_colour [nbs get $nbid.gcb.axes_colour]
  set Axes_colour [string trim $Axes_colour]

#  get option codes
  set Xaxis_opt [nbs get $nbid.gcb.xaxis_opt]
  set Xaxis_opt [string toupper $Xaxis_opt]
  set Yaxis_opt [nbs get $nbid.gcb.yaxis_opt]
  set Yaxis_opt [string toupper $Yaxis_opt]
#  use default if not present
  if {$Xaxis_opt==""} {
    set Xaxis_opt "BCNST"
  }
  if {$Yaxis_opt==""} {
    set Yaxis_opt "BCNST"
  }
  set old_xopt $Xaxis_opt
  set old_yopt $Yaxis_opt

#  decode into different components

#  form of frame
  if {($Xaxis_opt==" ") && ($Yaxis_opt==" ")} {
    set axes_form "0"
  } elseif {([string first BC $Xaxis_opt]>=0) && \
                ([string first BC $Yaxis_opt]>=0)} {
    set axes_form "BC"
  } elseif {([string first B $Xaxis_opt]>=0) && \
                        ([string first B $Yaxis_opt]>=0)} {
    set axes_form "B"
  }

  if {([string first A $Xaxis_opt]>=0) && \
                        ([string first A $Yaxis_opt]>=0)} {
    set axes_cross "A"
  } else {
    set axes_cross ""
  }

  if {([string first G $Xaxis_opt]>=0) && \
                        ([string first G $Yaxis_opt]>=0)} {
    set axes_grid "G"
  } else {
    set axes_grid ""
  }

#  tick marks
  if {([string first T $Xaxis_opt]>=0) && \
                        ([string first I $Xaxis_opt]>=0)} {
    set xopt_tick "TI"
  } elseif {([string first T $Xaxis_opt]>=0) && \
                        ([string first P $Xaxis_opt]>=0)} {
    set xopt_tick "TP"
  } elseif {[string first T $Xaxis_opt]>=0} {
    set xopt_tick "T"
  } else {
    set xopt_tick ""
  }
  if {[string first S $Xaxis_opt]>=0} {
    set xopt_subtick "S"
  } else {
    set xopt_subtick ""
  }
  if {([string first T $Yaxis_opt]>=0) && \
                        ([string first I $Yaxis_opt]>=0)} {
    set yopt_tick "TI"
  } elseif {([string first T $Yaxis_opt]>=0) && \
                        ([string first P $Yaxis_opt]>=0)} {
    set yopt_tick "TP"
  } elseif {[string first T $Yaxis_opt]>=0} {
    set yopt_tick "T"
  } else {
    set yopt_tick ""
  }
  if {[string first S $Yaxis_opt]>=0} {
    set yopt_subtick "S"
  } else {
    set yopt_subtick ""
  }

#  numeric labels
  if {([string first N $Xaxis_opt]>=0) && \
                        ([string first M $Xaxis_opt]>=0)} {
    set xopt_num "NM"
  } elseif {[string first N $Xaxis_opt]>=0} {
    set xopt_num "N"
  } elseif {[string first M $Xaxis_opt]>=0} {
    set xopt_num "M"
  } else {
    set xopt_num ""
  }
  if {([string first N $Yaxis_opt]>=0) && \
                        ([string first M $Yaxis_opt]>=0)} {
    set yopt_num "NM"
  } elseif {[string first N $Yaxis_opt]>=0} {
    set yopt_num "N"
  } elseif {[string first M $Yaxis_opt]>=0} {
    set yopt_num "M"
  } else {
    set yopt_num ""
  }

  set Xaxis_lo [nbs get $nbid.gcb.xaxis_lo]    
  set Xaxis_hi [nbs get $nbid.gcb.xaxis_hi]    
  if {$Xaxis_lo == ""} {
    set Xaxis_lo [nbs get $nbid.xmin]}
  set old_xlo $Xaxis_lo
  if {$Xaxis_hi == ""} {
    set Xaxis_hi [nbs get $nbid.xmax]}
  set old_xhi $Xaxis_hi
  set Yaxis_lo [nbs get $nbid.gcb.yaxis_lo]    
  set Yaxis_hi [nbs get $nbid.gcb.yaxis_hi]    
  if {$Yaxis_lo == ""} {
    set Yaxis_lo [nbs get $nbid.ymin]}
  set old_ylo $Yaxis_lo
  if {$Yaxis_hi == ""} {
    set Yaxis_hi [nbs get $nbid.ymax]}
  set old_yhi $Yaxis_hi
  set xinc [expr ($Xaxis_hi - $Xaxis_lo) / 100.0]
  set yinc [expr ($Yaxis_hi - $Yaxis_lo) / 100.0]

  set Xaxis_log [nbs get $nbid.gcb.xaxis_log]
  set Yaxis_log [nbs get $nbid.gcb.yaxis_log]
  if {$Xaxis_log == ""} {
    set Xaxis_log F}
  if {$Yaxis_log == ""} {
    set Yaxis_log F}

  set Xaxis_tick [nbs get $nbid.gcb.xaxis_tick]
  set Xaxis_div [nbs get $nbid.gcb.xaxis_div]
  set Yaxis_tick [nbs get $nbid.gcb.yaxis_tick]
  set Yaxis_div [nbs get $nbid.gcb.yaxis_div]
  set old_xtick $Xaxis_tick;set old_xdiv $Xaxis_div
  set old_ytick $Yaxis_tick;set old_ydiv $Yaxis_div


#  create dialogue box
  toplevel $w -class Dialog
  wm title $w "Axes Attributes"
  wm iconname $w Axes

  pack [frame $w.xy -relief groove -bd 2] -side top -padx 1m -pady 1m
  pack [frame $w.exit] -side bottom -padx 1m -pady 1
  pack [frame $w.x -relief groove -bd 2] -side left -padx 1m -pady 1
  pack [frame $w.y -relief groove -bd 2] -side right -padx 1m -pady 1

  label $w.xy.title -text "General" \
           -font -adobe-times-bold-i-normal--*-180-*-*-*-*-*-*
  pack $w.xy.title -side top -pady 3m

  pack [frame $w.xy.a] [frame $w.xy.b] [frame $w.xy.c] \
                   [frame $w.xy.d] -side left -padx 2m -expand yes -anchor n

  pack [frame $w.xy.a.form ] \
         [frame $w.xy.a.coord ] \
            -side top -ipady 3m -expand yes -anchor n

  pack [frame $w.xy.b.opt ]\
           [frame $w.xy.b.font ] \
            -side top -ipady 3m -expand yes -anchor n

  pack [frame $w.xy.c.col ] 
  pack [frame $w.xy.d.var ]

  label $w.xy.a.form.lbl -text Form
  radiobutton $w.xy.a.form.box -text Box -variable axes_form -value "BC"
  radiobutton $w.xy.a.form.l -text "L-shape" -variable axes_form \
                                                                 -value "B"
  radiobutton $w.xy.a.form.none -text None -variable axes_form -value "0"
  pack $w.xy.a.form.lbl -side top -anchor n
  pack $w.xy.a.form.box $w.xy.a.form.l \
              $w.xy.a.form.none -side top -anchor nw

  label $w.xy.a.coord.lbl -text Coordinates
  radiobutton $w.xy.a.coord.world -text World -variable Axes_radec -value F
  radiobutton $w.xy.a.coord.radec -text "RA/DEC" -variable Axes_radec \
                                                                     -value T
  pack $w.xy.a.coord.lbl -side top -anchor n
  pack  $w.xy.a.coord.world $w.xy.a.coord.radec \
                                          -side top -anchor nw
  
  label $w.xy.b.opt.lbl -text Options
  checkbutton $w.xy.b.opt.cross -text Crosswire -variable axes_cross \
                               -onvalue "A" -offvalue ""
  checkbutton $w.xy.b.opt.grid -text Gridlines -variable axes_grid \
                               -onvalue "G" -offvalue ""
  checkbutton $w.xy.b.opt.scale -text "Equal Scales" \
                         -variable Axes_scaling -onvalue T -offvalue F
  pack $w.xy.b.opt.lbl -side top -anchor n
  pack $w.xy.b.opt.cross $w.xy.b.opt.grid \
             $w.xy.b.opt.scale -side top -anchor nw


  label $w.xy.b.font.lbl -text Font
  radiobutton $w.xy.b.font.norm -text Normal -variable Axes_font -value 1
  radiobutton $w.xy.b.font.roman -text Roman -variable Axes_font -value 2
  radiobutton $w.xy.b.font.italic -text Italic -variable Axes_font -value 3
  radiobutton $w.xy.b.font.script -text Script -variable Axes_font -value 4
  pack $w.xy.b.font.lbl -side top -anchor n
  pack  $w.xy.b.font.norm $w.xy.b.font.roman \
            $w.xy.b.font.italic $w.xy.b.font.script -side top -anchor nw


  label $w.xy.c.col.lbl -text Colour
  radiobutton $w.xy.c.col.c0 -text "Black (bg)" -variable Axes_colour -value 0
  radiobutton $w.xy.c.col.c1 -text "White (fg)" -variable Axes_colour -value 1
  radiobutton $w.xy.c.col.c2 -text "Red" -variable Axes_colour -value 2
  radiobutton $w.xy.c.col.c3 -text "Green" -variable Axes_colour -value 3
  radiobutton $w.xy.c.col.c4 -text "Blue" -variable Axes_colour -value 4
  radiobutton $w.xy.c.col.c5 -text "Cyan" -variable Axes_colour -value 5
  radiobutton $w.xy.c.col.c6 -text "Magenta" -variable Axes_colour -value 6
  radiobutton $w.xy.c.col.c7 -text "Yellow" -variable Axes_colour -value 7

  pack $w.xy.c.col.lbl $w.xy.c.col.c0 $w.xy.c.col.c1 $w.xy.c.col.c2 \
        $w.xy.c.col.c3 $w.xy.c.col.c4 $w.xy.c.col.c5 $w.xy.c.col.c6 \
         $w.xy.c.col.c7 -side top -anchor nw -padx 2m


  scale $w.xy.d.var.size -label "% Character Size" -showvalue yes \
       -orient horizontal -to 500 -from 1 -length 40m \
       -command {get_ch $w.xy.d.var.size Axes_size}
  $w.xy.d.var.size set [expr $Axes_size*100]

  scale $w.xy.d.var.bold -label "Boldness" -showvalue yes \
       -orient horizontal -to 10 -from 1 -length 40m \
       -command {get_lw $w.xy.d.var.bold Axes_bold}
  $w.xy.d.var.bold set $Axes_bold

  scale $w.xy.d.var.width -label "LineWidth" -showvalue yes \
       -orient horizontal -to 10 -from 1 -length 40m \
       -command {get_lw $w.xy.d.var.width Axes_width}
  $w.xy.d.var.width set $Axes_width

  pack  $w.xy.d.var.size $w.xy.d.var.bold $w.xy.d.var.width -side top 


  pack [frame $w.x.top] [frame $w.x.bot] -side top

  pack [frame $w.x.top.title] [frame $w.x.top.scale] \
          [frame $w.x.top.lo] [frame $w.x.top.hi] \
            [frame $w.x.top.tick] [frame $w.x.top.div] -side top

  label $w.x.top.title.text -text "X-axis" \
           -font -adobe-times-bold-i-normal--*-180-*-*-*-*-*-*
  pack $w.x.top.title.text -pady 3m

  label $w.x.top.scale.lbl -text Scale
  radiobutton $w.x.top.scale.lin -text "Lin." -variable Xaxis_log -value F
  radiobutton $w.x.top.scale.log -text "Log." -variable Xaxis_log -value T
  pack $w.x.top.scale.lbl $w.x.top.scale.lin $w.x.top.scale.log -side left

  label $w.x.top.lo.lbl -text "Lo edge"
  button $w.x.top.lo.down -text "<<" -command \
                         {set Xaxis_lo [expr $Xaxis_lo - $xinc]}
  entry $w.x.top.lo.val -textvariable Xaxis_lo -width 8 -relief raised
  button $w.x.top.lo.up -text ">>" -command \
                         {set Xaxis_lo [expr $Xaxis_lo + $xinc]}
  pack $w.x.top.lo.lbl $w.x.top.lo.down $w.x.top.lo.val $w.x.top.lo.up \
                                      -side left

  label $w.x.top.hi.lbl -text "Hi edge"
  button $w.x.top.hi.down -text "<<" -command \
                         {set Xaxis_hi [expr $Xaxis_hi - $xinc]}
  entry $w.x.top.hi.val -textvariable Xaxis_hi -width 8 -relief raised
  button $w.x.top.hi.up -text ">>" -command \
                         {set Xaxis_hi [expr $Xaxis_hi + $xinc]}

  pack $w.x.top.hi.lbl $w.x.top.hi.down $w.x.top.hi.val $w.x.top.hi.up \
                                      -side left

  pack [frame $w.x.bot.tick] [frame $w.x.bot.num] -side top
  pack [frame $w.x.bot.tick.lbl] -side top
  pack [frame $w.x.bot.tick.where] [frame $w.x.bot.tick.spacing] -side left
  pack [frame $w.x.bot.num.lbl] [frame $w.x.bot.num.where] -side top

  pack [label $w.x.bot.tick.lbl.text -text "Tick Marks"] -pady 2m

  radiobutton $w.x.bot.tick.where.in -text Inside -variable xopt_tick \
                                                     -value "T"
  radiobutton $w.x.bot.tick.where.out -text Outside -variable xopt_tick \
                                                     -value "TI"
  radiobutton $w.x.bot.tick.where.both -text Both -variable xopt_tick \
                                                     -value "TP"
  radiobutton $w.x.bot.tick.where.none -text None -variable xopt_tick \
                                                     -value ""
  pack $w.x.bot.tick.where.in $w.x.bot.tick.where.out \
               $w.x.bot.tick.where.both $w.x.bot.tick.where.none -side top \
                                      -anchor nw

  label $w.x.bot.tick.spacing.lbl1 -text "Major Interval"
  entry $w.x.bot.tick.spacing.int -textvariable Xaxis_tick -width 6 \
                                                 -relief raised
  checkbutton $w.x.bot.tick.spacing.min -text "Show minor ticks" \
                -variable xopt_subtick -onvalue "S" -offvalue ""
  label $w.x.bot.tick.spacing.lbl2 -text "Minor Divisions"
  entry $w.x.bot.tick.spacing.div -textvariable Xaxis_div -width 6 \
                                                 -relief raised

  pack $w.x.bot.tick.spacing.lbl1 $w.x.bot.tick.spacing.int \
       $w.x.bot.tick.spacing.min $w.x.bot.tick.spacing.lbl2 \
              $w.x.bot.tick.spacing.div -side top

  label $w.x.bot.num.lbl.text -text "Position of Numbers"
  pack $w.x.bot.num.lbl.text

  radiobutton $w.x.bot.num.where.b1 -text "Bottom" -variable xopt_num \
                                                               -value "N"
  radiobutton $w.x.bot.num.where.b2 -text "Top" -variable xopt_num \
                                                                -value "M"
  radiobutton $w.x.bot.num.where.b3 -text "Both" -variable xopt_num \
                                                                -value "NM"
  radiobutton $w.x.bot.num.where.b4 -text "None" -variable xopt_num \
                                                                -value ""
  pack $w.x.bot.num.where.b1 $w.x.bot.num.where.b2 $w.x.bot.num.where.b3 \
            $w.x.bot.num.where.b4 -side left


  pack [frame $w.y.top] [frame $w.y.bot] -side top

  pack [frame $w.y.top.title] [frame $w.y.top.scale] \
          [frame $w.y.top.lo] [frame $w.y.top.hi] \
            [frame $w.y.top.tick] [frame $w.y.top.div] -side top

  label $w.y.top.title.text -text "Y-axis" \
           -font -adobe-times-bold-i-normal--*-180-*-*-*-*-*-*
  pack $w.y.top.title.text -pady 3m

  label $w.y.top.scale.lbl -text Scale
  radiobutton $w.y.top.scale.lin -text "Lin." -variable Yaxis_log -value F
  radiobutton $w.y.top.scale.log -text "Log." -variable Yaxis_log -value T
  pack $w.y.top.scale.lbl $w.y.top.scale.lin $w.y.top.scale.log -side left

  label $w.y.top.lo.lbl -text "Lo edge"
  button $w.y.top.lo.down -text "<<" -command \
                         {set Yaxis_lo [expr $Yaxis_lo - $yinc]}
  entry $w.y.top.lo.val -textvariable Yaxis_lo -width 8 -relief raised
  button $w.y.top.lo.up -text ">>" -command \
                         {set Yaxis_lo [expr $Yaxis_lo + $yinc]}
  pack $w.y.top.lo.lbl $w.y.top.lo.down $w.y.top.lo.val $w.y.top.lo.up \
                                      -side left
  label $w.y.top.hi.lbl -text "Hi edge"
  button $w.y.top.hi.down -text "<<" -command \
                         {set Yaxis_hi [expr $Yaxis_hi - $yinc]}
  entry $w.y.top.hi.val -textvariable Yaxis_hi -width 8 -relief raised
  button $w.y.top.hi.up -text ">>" -command \
                         {set Yaxis_hi [expr $Yaxis_hi + $yinc]}

  pack $w.y.top.hi.lbl $w.y.top.hi.down $w.y.top.hi.val $w.y.top.hi.up \
                                      -side left

  pack [frame $w.y.bot.tick] [frame $w.y.bot.num] -side top
  pack [frame $w.y.bot.tick.lbl] -side top
  pack [frame $w.y.bot.tick.where] [frame $w.y.bot.tick.spacing] -side left
  pack [frame $w.y.bot.num.lbl] [frame $w.y.bot.num.where] -side top

  pack [label $w.y.bot.tick.lbl.text -text "Tick Marks"] -pady 2m

  radiobutton $w.y.bot.tick.where.in -text Inside -variable yopt_tick \
                                                     -value "T"
  radiobutton $w.y.bot.tick.where.out -text Outside -variable yopt_tick \
                                                     -value "TI"
  radiobutton $w.y.bot.tick.where.both -text Both -variable yopt_tick \
                                                     -value "TP"
  radiobutton $w.y.bot.tick.where.none -text None -variable yopt_tick \
                                                     -value ""
  pack $w.y.bot.tick.where.in $w.y.bot.tick.where.out \
               $w.y.bot.tick.where.both $w.y.bot.tick.where.none -side top \
                                      -anchor nw

  label $w.y.bot.tick.spacing.lbl1 -text "Major Interval"
  entry $w.y.bot.tick.spacing.int -textvariable Yaxis_tick -width 6 \
                                                 -relief raised
  checkbutton $w.y.bot.tick.spacing.min -text "Show minor ticks" \
                    -variable yopt_subtick -onvalue "S" -offvalue ""
  label $w.y.bot.tick.spacing.lbl2 -text "Minor Divisions"
  entry $w.y.bot.tick.spacing.div -textvariable Yaxis_div -width 6 \
                                                 -relief raised

  pack $w.y.bot.tick.spacing.lbl1 $w.y.bot.tick.spacing.int \
       $w.y.bot.tick.spacing.min $w.y.bot.tick.spacing.lbl2 \
              $w.y.bot.tick.spacing.div -side top


  label $w.y.bot.num.lbl.text -text "Position of Numbers"
  pack $w.y.bot.num.lbl.text

  radiobutton $w.y.bot.num.where.b1 -text "Left" -variable yopt_num \
                                                                 -value "N"
  radiobutton $w.y.bot.num.where.b2 -text "Right" -variable yopt_num \
                                                                 -value "M"
  radiobutton $w.y.bot.num.where.b3 -text "Both" -variable yopt_num \
                                                                 -value "NM"
  radiobutton $w.y.bot.num.where.b4 -text "None" -variable yopt_num \
                                                                 -value ""
  pack $w.y.bot.num.where.b1 $w.y.bot.num.where.b2 $w.y.bot.num.where.b3 \
            $w.y.bot.num.where.b4 -side left

  button $w.exit.ok -text Ok -command \
     { if {$Axes_size == $old_size} {set Axes_size "!"}
       if {$Axes_bold == $old_bold} {set Axes_bold "!"}
       if {$Axes_width == $old_width} {set Axes_width "!"}
       ImgExecWait gset "switch=axes width=$Axes_width size=$Axes_size \
                            bold=$Axes_bold accept" 

       set Xaxis_opt \
           "$axes_form$axes_cross$axes_grid$xopt_num$xopt_subtick$xopt_tick"
       set Yaxis_opt \
           "$axes_form$axes_cross$axes_grid$yopt_num$yopt_subtick$yopt_tick"

       if {$Xaxis_opt == $old_xopt} {set Xaxis_opt "!"}
       if {$Xaxis_lo == $old_xlo} {set Xaxis_lo "!"}
       if {$Xaxis_hi == $old_xhi} {set Xaxis_hi "!"}
       if {$Xaxis_tick == $old_xtick} {set Xaxis_tick "!"}
       if {$Xaxis_div == $old_xdiv} {set Xaxis_div "!"}
       ImgExecWait gset "switch=xaxis lo=$Xaxis_lo hi=$Xaxis_hi \
                 tick=$Xaxis_tick div=$Xaxis_div \
                   opt=$Xaxis_opt accept" 
       if {$Yaxis_opt == $old_yopt} {set Yaxis_opt "!"}
       if {$Yaxis_lo == $old_ylo} {set Yaxis_lo "!"}
       if {$Yaxis_hi == $old_yhi} {set Yaxis_hi "!"}
       if {$Yaxis_tick == $old_ytick} {set Yaxis_tick "!"}
       if {$Yaxis_div == $old_ydiv} {set Yaxis_div "!"}
       ImgExecWait gset "switch=yaxis lo=$Yaxis_lo hi=$Yaxis_hi \
                  tick=$Yaxis_tick div=$Yaxis_div \
                    opt=$Yaxis_opt accept" 

       if {$axes_form == "0"} {
         ImgExecWait gset "switch=axes suppress=yes accept" 
       }

       ImgExecWaitNoMsg igui "action=update" 
       ImgExecWaitNoMsg igui "action=cache value=twod"

       if {$AutoRefresh == 1} {
         ImgExecWait idisplay " "
       }
       set Ok 1
     }

  button $w.exit.reset -text Reset -command \
     {
       ImgExecWait gset "switch=axes cancel=yes"
       ImgExecWaitNoMsg igui "action=update"
       ImgExecWaitNoMsg igui "action=cache value=twod"
       if {$AutoRefresh == 1} {
         ImgExecWait idisplay " "
       }
       set Ok 2
     }

  button $w.exit.cancel -text Cancel -command {set Ok 999}

  pack $w.exit.ok $w.exit.reset $w.exit.cancel \
                 -side left -expand 1 -padx 3m -pady 4m

  trace variable Axes_radec w set_axes_coord
  trace variable Axes_scaling w set_axes_scaling
  trace variable Axes_font w set_axes_font
  trace variable Axes_colour w set_axes_colour
  trace variable axes_form w set_axes_form

  tkwait variable Ok
  trace vdelete Axes_radec w set_axes_coord
  trace vdelete Axes_scaling w set_axes_scaling
  trace vdelete Axes_font w set_axes_font
  trace vdelete Axes_colour w set_axes_colour
  trace vdelete axes_form w set_axes_form
  destroy $w
}
#
#
#
#
#
proc set_axes_coord {name element op} {
  global  Axes_radec
  ImgExecWait gset "switch=axes radec=$Axes_radec accept" 
}
#
#
#
proc set_axes_font {name element op} {
  global  Axes_font
  ImgExecWait gset "switch=axes font=$Axes_font accept" 
}
#
#
proc set_axes_colour {name element op} {
  global  Axes_colour
  ImgExecWait gset "switch=axes colour=$Axes_colour accept" 
}
#
#
proc set_axes_scaling {name element op} {
  global  Axes_scaling
  ImgExecWait gset "switch=axes scale=$Axes_scaling accept" 
}
#
#
proc set_axes_form {name element op} {

  global axes_form axes_cross axes_grid
  global xopt_tick xopt_subtick xopt_num 
  global yopt_tick yopt_subtick yopt_num 

  if {[string first B $axes_form]>=0} {
    set xopt_tick "T"
    set yopt_tick "T"
    set xopt_subtick "S"
    set yopt_subtick "S"
    set xopt_num "N"
    set yopt_num "N"
  } elseif {$axes_form == "0"} {
    set xopt_tick ""
    set yopt_tick ""
    set xopt_subtick ""
    set yopt_subtick ""
    set xopt_num ""
    set yopt_num ""
  }
}
#
