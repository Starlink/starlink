#
proc set_labels {w } {

  ImgExecWaitNoMsg igui "action=synchro value=twod"
  ImgExecWaitNoMsg igui "action=update" 

  global nbid

  global Key_flag Key_opt Key_font Key_size Key_bold

  global old_font old_size old_bold old_opt
  global opt_pix opt_cont

  set Key_opt [nbs get $nbid.gcb.key_opt]  
  set Key_opt [string toupper $Key_opt]
  set old_opt $Key_opt
  if {[string first P $Key_opt]>=0} {
    set opt_pix "P"
  } else {
    set opt_pix ""
  }
  if {[string first C $Key_opt]>=0} {
    set opt_cont "C"
  } else {
    set opt_cont ""
  }
  set Key_font [nbs get $nbid.gcb.key_font]
  if {$Key_font==""} {
    set Key_font 1}
  set old_font $Key_font
  set Key_size [nbs get $nbid.gcb.key_size]
  if {$Key_size==""} {
    set Key_size 1.0 }
  set old_size $Key_size
  set Key_bold [nbs get $nbid.gcb.key_bold]
  if {$Key_bold==""} {
    set Key_bold 1 }
  set old_bold $Key_bold

  toplevel $w -class Dialog
  wm title $w "Key"
  wm iconname $w Key

  pack [frame $w.top] -side top -fill both
  pack [frame $w.bot] -side bottom -fill both

  pack [frame $w.top.a] [frame $w.top.b] [frame $w.top.c] -side left

  label $w.top.a.lbl -text "Key Options"
  checkbutton $w.top.a.pix -text "Colour Bar" -variable opt_pix \
                -onvalue "P" -offvalue ""
  checkbutton $w.top.a.cont -text "Contour Levels" -variable opt_cont \
                -onvalue "C" -offvalue ""
  pack $w.top.a.lbl -side top -anchor n
  pack $w.top.a.pix $w.top.a.cont -side top -anchor nw

  label $w.top.b.lbl -text Font
  radiobutton $w.top.b.f1 -text Normal -variable Key_font -value 1
  radiobutton $w.top.b.f2 -text Roman -variable Key_font -value 2
  radiobutton $w.top.b.f3 -text Italic -variable Key_font -value 3
  radiobutton $w.top.b.f4 -text Script -variable Key_font -value 4

  pack $w.top.b.lbl -side top -anchor n
  pack $w.top.b.f1 $w.top.b.f2 $w.top.b.f3 $w.top.b.f4 -side top \
              -anchor nw 


  scale $w.top.c.ch -label "% Character Size" -showvalue yes \
       -orient horizontal -to 500 -from 1 -length 40m \
       -command {get_ch $w.top.c.ch Key_size}
  scale $w.top.c.bold -label "Boldness" -showvalue yes \
       -orient horizontal -to 20 -from 1 -length 40m \
       -command {get_lw $w.top.c.bold Key_bold}
  $w.top.c.ch set [expr $Key_size*100]
  $w.top.c.bold set $Key_bold

  pack $w.top.c.ch $w.top.c.bold -side top 

  button $w.bot.ok -text Ok -command {
     set Key_opt "$opt_pix$opt_cont"
     if {$Key_opt == $old_opt} {set Key_opt "!"}
     if {$Key_font == $old_font} {set Key_font "!"}
     if {$Key_size == $old_size} {set Key_size "!"}
     if {$Key_bold == $old_bold} {set Key_bold "!"}
     ImgExecWait gset \
     "switch=key opt=$Key_opt font=$Key_font size=$Key_size bold=$Key_bold \
           accept" 
     ImgExecWaitNoMsg igui "action=update" 
     ImgExecWaitNoMsg igui "action=cache value=twod"
     if {$AutoRefresh == 1} {
       ImgExecWait idisplay " "
     }
     set Ok yes}

  button $w.bot.reset -text Reset -command {ImgExecWait gset \
                                     "switch=key off=yes" 
                             ImgExecWaitNoMsg igui "action=update"
                             ImgExecWaitNoMsg igui "action=cache value=twod"
                             if {$AutoRefresh == 1} {
                               ImgExecWait idisplay " "
                             }
                             set Ok yes}

  pack $w.bot.ok $w.bot.reset -side left -expand 1 -padx 3m -padx 4m


#  set oldFocus [focus]
#  grab set $w
#  focus $w
  tkwait variable Ok
  destroy $w
#  focus $oldFocus
}
#
#
#
#
#
proc set_labels {w } {

  ImgExecWaitNoMsg igui "action=synchro value=twod"
  ImgExecWaitNoMsg igui "action=update" 

  global W
  set W $w

  global AutoRefresh
  global nbid
  global Text_font Text_size Text_bold Text_colour
  global X_label Y_label

  set Text_font [nbs get $nbid.gcb.xlabel_font]
  if {$Text_font==""} {
    set Text_font 1}
  set Text_size [nbs get $nbid.gcb.xlabel_size]
  if {$Text_size==""} {
    set Text_size 1.0 }
  set Text_bold [nbs get $nbid.gcb.xlabel_bold]
  if {$Text_bold==""} {
    set Text_bold 1}
  set Text_colour [nbs get $nbid.gcb.xlabel_colour]
  set Text_colour [string trim $Text_colour]
  if {$Text_colour==""} {
    set Text_colour 1}
  set X_label [nbs get $nbid.gcb.xlabel_text]
  if {$X_label==""} {
    set X_label " "}
  set Y_label [nbs get $nbid.gcb.ylabel_text]
  if {$Y_label==""} {
    set Y_label " "}

  toplevel $w -class Dialog
  wm title $w "Axis Labels"
  wm iconname $w Labels

  pack [frame $w.bot] -side bottom -fill both
  pack [frame $w.x] -side top
  pack [frame $w.y] -side top

  label $w.x.lbl -text X
  entry $w.x.val -textvariable X_label -width 40
  pack $w.x.lbl $w.x.val -side left

  label $w.y.lbl -text Y
  entry $w.y.val -textvariable Y_label -width 40
  pack $w.y.lbl $w.y.val -side left

  button $w.bot.fmt -text Format -command {Format_Text .txtfmt_dialogue}

  button $w.bot.ok -text Ok -command {
     ImgExecWait gset \
       "switch=xlabel font=$Text_font size=$Text_size bold=$Text_bold \
        colour=$Text_colour text=\"$X_label\" accept" 
     ImgExecWait gset \
       "switch=ylabel font=$Text_font size=$Text_size bold=$Text_bold \
        colour=$Text_colour text=\"$Y_label\" accept" 
     ImgExecWaitNoMsg igui "action=update"
     ImgExecWaitNoMsg igui "action=cache value=twod"
     if {$AutoRefresh == 1} {
       ImgExecWait idisplay " "
     }
     set Ok 1
    }



  button $w.bot.reset -text Reset -command {
                             ImgExecWait gset \
                               "switch=xlabel off=yes" 
                             ImgExecWait gset \
                               "switch=ylabel off=yes" 
                             ImgExecWaitNoMsg igui "action=update"
                             ImgExecWaitNoMsg igui "action=cache value=twod"
                             if {$AutoRefresh == 1} {
                               ImgExecWait idisplay " "
                             }
                             set Ok 2}

  button $w.bot.cancel -text Cancel -command {set Ok 999}

  pack $w.bot.fmt $w.bot.ok $w.bot.reset $w.bot.cancel \
                -side left -expand 1 -padx 3m -padx 4m

  Centre_Window $w
  set oldfocus [focus]
  focus $w.x.val
  bind $w.x.val <Return> {focus $W.y.val}

  tkwait variable Ok
  focus $oldfocus
  destroy $w
}
#
