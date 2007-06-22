proc cgs3drGetArgs {directory date} {
#+
# Dialogue box for args
#-
    global env
    global dataRoot
    global utRoot

# Populate the variables
    set confirmDirectory 0
    set confirmDate      0
    set confirmFormat    0

    set dataRoot [string trim $directory]
    if {$dataRoot==""} {set confirmDirectory 1}
    if {![file exists $dataRoot]} {set confirmDirectory 1}
    set utRoot [string trim $date]
    if {$utRoot==""} {set confirmDate 1}

# Create confirmation dialogue box
    if {$confirmDirectory==1 || $confirmDate==1} {
      if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
      set frame [dialogStart .cgs3drDialogue "Cgs3dr Argument Confirmaton" 0 OK]
      set top [frame $frame.top]
      pack $top -in $frame -side top -fill both -expand yes

#   Confirm directory as necessary
      if {$confirmDirectory==1} {
        set l1 [label $top.l1 -text "Confirm Directory" -fg RED]
      } else {
        set l1 [label $top.l1 -text "Directory"]
      }
      set e1 [entry $top.e1 -relief sunken -width 40]
      pack $l1 -in $top -side left
      pack $e1 -in $top -side left -expand yes -fill x
      $e1 insert end $dataRoot

#   Confirm date as necessary
      if {$confirmDate==1} {
        set l2 [label $top.l2 -text "Confirm UT Date" -fg RED]
      } else {
        set l2 [label $top.l2 -text "UT Date"]
      }
      set e2 [entry $top.e2 -relief sunken]
      pack $e2 -in $top -side right -expand yes -fill x
      pack $l2 -in $top -side right
      $e2 insert end $utRoot

#   Get the values and set the environmental variables
      set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
      cgs3drCursor pirate orange black
      .cgs3drDialogue config -cursor {arrow green black}
      if {$bv==0} {
        cgs3drCursor watch red white
        set dataRoot [string trim [$e1 get]]
        if {$dataRoot==""} {set dataRoot $env(HOME)}
        set utRoot [string trim [$e2 get]]
        if {$utRoot==""} {set utRoot [exec /usr/bin/date -u +%Y%m%d]}
      }
      destroy .cgs3drDialogue
    }
    cgs3drSetEnv $dataRoot $utRoot
    cgs3drCursor arrow green black
}
