proc cgs4drGetArgs {directory date format} {
#+
# Dialogue box for args
#-
    global env
    global dataRoot
    global utRoot
    global fmtRoot

# Populate the variables
    set confirmDirectory 0
    set confirmDate      0
    set confirmFormat    0

    set dataRoot [string trim $directory]
    if {$dataRoot==""} {set confirmDirectory 1}
    if {![file exists $dataRoot]} {set confirmDirectory 1}
    set utRoot [string trim $date]
    if {$utRoot==""} {set confirmDate 1}
    set fmtRoot [string trim [string toupper $format]]
    if {$fmtRoot==""} {set confirmFormat 1}
    if {$fmtRoot!="NDF" && $fmtRoot!="DST"} {set confirmFormat 1}

# Create confirmation dialogue box
    if {$confirmDirectory==1 || $confirmDate==1 || $confirmFormat==1} {
      if {[winfo exists .cgs4drDialogue]} {destroy .cgs4drDialogue}
      set frame [dialogStart .cgs4drDialogue "Cgs4dr Argument Confirmaton" 0 OK]
      set top [frame $frame.top]
      set bot [frame $frame.bot]
      pack $top $bot -in $frame -side top -fill both -expand yes

#   Confirm directory as necessary
      if {$confirmDirectory==1} {
        set l1 [label $top.l1 -text "Confirm Directory" -fg RED]
      } else {
        set l1 [label $top.l1 -text "Directory"]
      }
      set e1 [entry $top.e1 -relief sunken -width 60]
      pack $l1 -in $top -side left
      pack $e1 -in $top -side left -expand yes -fill x
      $e1 insert end $dataRoot

#   Confirm date as necessary
      if {$confirmDate==1} {
        set l2 [label $bot.l2 -text "Confirm UT Date" -fg RED]
      } else {
        set l2 [label $bot.l2 -text "UT Date"]
      }
      set e2 [entry $bot.e2 -relief sunken]
      pack $l2 -in $bot -side left
      pack $e2 -in $bot -side left -expand yes -fill x
      $e2 insert end $utRoot

#   Confirm format as necessary
      if {$confirmFormat==1} {
        set l3 [label $bot.l3 -text "     Confirm Format" -fg RED]
      } else {
        set l3 [label $bot.l3 -text "     Format"]
      }
      set r1 [radiobutton $bot.r1 -text "NDF" -value "NDF" -variable fmtRoot]
      set r2 [radiobutton $bot.r2 -text "DST" -value "DST" -variable fmtRoot]
      pack $r2 $r1 $l3 -in $bot -side right

# Get the values and set the environmental variables
      set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
      cgs4drCursor pirate orange black
      .cgs4drDialogue config -cursor {arrow green black}
      if {$bv==0} {
        cgs4drCursor watch red white
        set dataRoot [string trim [$e1 get]]
        if {$dataRoot==""} {set dataRoot $env(HOME)}
        set utRoot [string trim [$e2 get]]
        if {$utRoot==""} {set utRoot [exec /usr/bin/date -u +%y%m%d]}
        set fmtRoot  [string trim [string toupper $fmtRoot]]
        if {$fmtRoot==""} {set fmtRoot NDF}
      }
      destroy .cgs4drDialogue
    }
    cgs4drSetEnv $dataRoot $utRoot $fmtRoot
    cgs4drCursor arrow green black
}
