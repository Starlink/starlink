proc cgs3drSetpar {} {

# Get some global values
  global env
  global Cgs3drWidgets
  global Cgs3drTask

# Create dialog box
  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Cgs3 Set Parameters" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

# Create panel layout
  set top  [frame $frame.top  -relief sunken -bd 2]
  set bottom [frame $frame.bottom -relief sunken -bd 2]
  pack $top $bottom -in $frame -side top -fill both -expand yes

  set left  [frame $top.left  -relief sunken -bd 2]
  set right [frame $top.right -relief sunken -bd 2]
  pack $left $right -in $top -side left -fill both -expand yes

# Pack the bottom with a graphics device
  set Cgs3drWidgets(bl1) [label $bottom.l1 -text "Graphics Device"]
  set Cgs3drWidgets(be1) [entry $bottom.e1 -width 50 -relief sunken -bd 2 -textvariable Cgs3drWidgets(PAR_GDEVICE)]
  pack $Cgs3drWidgets(bl1) $Cgs3drWidgets(be1) -in $bottom -side left
  bind $Cgs3drWidgets(bl1) <Button-2> {$Cgs3drWidgets(be1) delete 0 end; $Cgs3drWidgets(be1) insert end xwindows}
  bind $Cgs3drWidgets(be1) <Button-2> {$Cgs3drWidgets(be1) delete 0 end; $Cgs3drWidgets(be1) insert end xwindows}
  bind $Cgs3drWidgets(be1) <Double-Button-2> "$Cgs3drWidgets(be1) delete 0 end"

# Create lhs checkbuttons
  set lf1 [frame $left.l1 -relief flat]
  set lf2 [frame $left.l2 -relief flat]
  set lf3 [frame $left.l3 -relief flat]
  set lf4 [frame $left.l4 -relief flat]
  set lf5 [frame $left.l5 -relief flat]
  set lf6 [frame $left.l6 -relief flat]
  pack $lf1 $lf2 $lf3 $lf4 $lf5 $lf6 -in $left -side top -fill both -expand yes

  set ll1 [label $lf1.ll1 -text "Plotting"]
  set lc1 [checkbutton $lf1.l1 -variable Cgs3drWidgets(PAR_PLOTTING)]
  pack $lc1 $ll1 -in $lf1 -side left
  bind $ll1 <Button-2> "set Cgs3drWidgets(PAR_PLOTTING) 1"
  bind $lc1 <Button-2> "set Cgs3drWidgets(PAR_PLOTTING) 1"
  set ll2 [label $lf2.ll2 -text "Divide by Sky"]
  set lc2 [checkbutton $lf2.l2 -variable Cgs3drWidgets(PAR_DIVBYSKY)]
  pack $lc2 $ll2 -in $lf2 -side left
  bind $ll2 <Button-2> "set Cgs3drWidgets(PAR_DIVBYSKY) 1"
  bind $lc2 <Button-2> "set Cgs3drWidgets(PAR_DIVBYSKY) 1"
  set ll3 [label $lf3.ll3 -text "Divide by Std"]
  set lc3 [checkbutton $lf3.l3 -variable Cgs3drWidgets(PAR_DIVBYSTD)]
  pack $lc3 $ll3 -in $lf3 -side left
  bind $ll3 <Button-2> "set Cgs3drWidgets(PAR_DIVBYSTD) 0"
  bind $lc3 <Button-2> "set Cgs3drWidgets(PAR_DIVBYSTD) 0"
  set ll4 [label $lf4.ll4 -text "Cycle by Cycle"]
  set lc4 [checkbutton $lf4.l4 -variable Cgs3drWidgets(PAR_CYCBYCYC)]
  pack $lc4 $ll4 -in $lf4 -side left
  bind $ll4 <Button-2> "set Cgs3drWidgets(PAR_CYCBYCYC) 0"
  bind $lc4 <Button-2> "set Cgs3drWidgets(PAR_CYCBYCYC) 0"
  set ll5 [label $lf5.ll5 -text "Verbose"]
  set lc5 [checkbutton $lf5.l5 -variable Cgs3drWidgets(PAR_VERBOSE)]
  pack $lc5 $ll5 -in $lf5 -side left
  bind $ll5 <Button-2> "set Cgs3drWidgets(PAR_VERBOSE) 1"
  bind $lc5 <Button-2> "set Cgs3drWidgets(PAR_VERBOSE) 1"
  set ll6 [label $lf6.ll6 -text "Verbose_ph"]
  set lc6 [checkbutton $lf6.l6 -variable Cgs3drWidgets(PAR_VERBOSE_PH)]
  pack $lc6 $ll6 -in $lf6 -side left
  bind $ll6 <Button-2> "set Cgs3drWidgets(PAR_VERBOSE_PH) 0"
  bind $lc6 <Button-2> "set Cgs3drWidgets(PAR_VERBOSE_PH) 0"

# Create rhs widgets
  set rf1 [frame $right.l1 -relief flat]
  set rf2 [frame $right.l2 -relief flat]
  set rf3 [frame $right.l3 -relief flat]
  set rf4 [frame $right.l4 -relief flat]
  set rf5 [frame $right.l5 -relief flat]
  set rf6 [frame $right.l6 -relief flat]
  pack $rf1 $rf2 $rf3 $rf4 $rf5 $rf6 -in $right -side top -fill both -expand yes

  set rl1 [label $rf1.l1 -text "Cycle Start"]
  set re1 [entry $rf1.e1 -width 12 -relief sunken -bd 2 -textvariable Cgs3drWidgets(PAR_CYCBEG)]
  pack $rl1 -in $rf1 -side left
  pack $re1 -in $rf1 -side right
  bind $rl1 <Button-2> "$re1 delete 0 end; $re1 insert end 1"
  bind $re1 <Button-2> "$re1 delete 0 end; $re1 insert end 1"
  bind $re1 <Double-Button-2> "$re1 delete 0 end"
  set rl2 [label $rf2.l2 -text "Cycle End"]
  set re2 [entry $rf2.e2 -width 12 -relief sunken -bd 2 -textvariable Cgs3drWidgets(PAR_CYCEND)]
  pack $rl2 -in $rf2 -side left
  pack $re2 -in $rf2 -side right
  bind $rl2 <Button-2> "$re2 delete 0 end; $re2 insert end 0"
  bind $re2 <Button-2> "$re2 delete 0 end; $re2 insert end 0"
  bind $re2 <Double-Button-2> "$re2 delete 0 end"
  set rl3 [label $rf3.l3 -text "Ichannel Start"]
  set re3 [entry $rf3.e3 -width 12 -relief sunken -bd 2 -textvariable Cgs3drWidgets(PAR_ICHANBEG)]
  pack $rl3 -in $rf3 -side left
  pack $re3 -in $rf3 -side right
  bind $rl3 <Button-2> "$re3 delete 0 end; $re3 insert end 1"
  bind $re3 <Button-2> "$re3 delete 0 end; $re3 insert end 1"
  bind $re3 <Double-Button-2> "$re3 delete 0 end"
  set rl4 [label $rf4.l4 -text "Ichannel End"]
  set re4 [entry $rf4.e4 -width 12 -relief sunken -bd 2 -textvariable Cgs3drWidgets(PAR_ICHANEND)]
  pack $rl4 -in $rf4 -side left
  pack $re4 -in $rf4 -side right
  bind $rl4 <Button-2> "$re4 delete 0 end; $re4 insert end 32"
  bind $re4 <Button-2> "$re4 delete 0 end; $re4 insert end 32"
  bind $re4 <Double-Button-2> "$re4 delete 0 end"
  set rl5 [label $rf5.l5 -text "Nsigma"]
  set re5 [entry $rf5.e5 -width 12 -relief sunken -bd 2 -textvariable Cgs3drWidgets(PAR_NSIGMA)]
  pack $rl5 -in $rf5 -side left
  pack $re5 -in $rf5 -side right
  bind $rl5 <Button-2> "$re5 delete 0 end; $re5 insert end 3.0"
  bind $re5 <Button-2> "$re5 delete 0 end; $re5 insert end 3.0"
  bind $re5 <Double-Button-2> "$re5 delete 0 end"
  set rl6 [label $rf6.l6 -text "Standard"]
  set re6 [entry $rf6.e6 -width 12 -relief sunken -bd 2 -textvariable Cgs3drWidgets(PAR_STANDARD)]
  pack $rl6 -in $rf6 -side left
  pack $re6 -in $rf6 -side right
  bind $rl6 <Button-2> "$re6 delete 0 end"
  bind $re6 <Button-2> "$re6 delete 0 end"
  bind $re6 <Double-Button-2> "$re6 delete 0 end"

# Show the dialog box and set paramets
  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  cgs3drCursor watch red white
  cgs3drClear

# OK pressed
  if {$bv==0} {
    set cbeg [string trim [$re1 get]]
    set cend [string trim [$re2 get]]
    set nsig [string trim [$re5 get]]
    set strd [string trim [$re6 get]]
    set ibeg [string trim [$re3 get]]
    set iend [string trim [$re4 get]]
    set gdev [string trim [$Cgs3drWidgets(be1) get]]

    if {$Cgs3drWidgets(PAR_PLOTTING)==1} {
      set message "PLOTTING=T"
    } else {
      set message "PLOTTING=F"
    }
    if {$Cgs3drWidgets(PAR_VERBOSE)==1} {
      set message "${message} VERBOSE=T"
    } else {
      set message "${message} VERBOSE=F"
    }
    if {$Cgs3drWidgets(PAR_DIVBYSKY)==1} {
      set message "${message} DIVBYSKY=T"
    } else {
      set message "${message} DIVBYSKY=F"
    }
    if {$Cgs3drWidgets(PAR_DIVBYSTD)==1} {
      set message "${message} DIVBYSTD=T"
    } else {
      set message "${message} DIVBYSTD=F"
    }
    if {$Cgs3drWidgets(PAR_CYCBYCYC)==1} {
      set message "${message} CYCBYCYC=T"
    } else {
      set message "${message} CYCBYCYC=F"
    }
    set message "${message} CYCBEG=${cbeg} CYCEND=${cend} NSIGMA=${nsig} STANDARD=\"${strd}\" ICHANBEG=${ibeg} ICHANEND=${iend}"
    if {$Cgs3drWidgets(PAR_VERBOSE_PH)==1} {
      set message "${message} VERBOSE_PH=T GDEVICE=\"${gdev}\""
    } else {
      set message "${message} VERBOSE_PH=F GDEVICE=\"${gdev}\""
    }

    $Cgs3drTask obey setpar "$message" -inform "cgs3drInform %V"
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}
