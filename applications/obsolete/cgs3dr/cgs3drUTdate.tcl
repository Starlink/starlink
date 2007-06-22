proc cgs3drUTdate {} {
  global env
  cgs3drClear
  cgs3drInform "Re-initialising CGS3DR ... please wait"

# Create confirmation dialogue box
  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Cgs3dr Initialisation Confirmaton" 0 OK Cancel]
  set top [frame $frame.top]
  pack $top -fill both -expand yes

# Confirm directory as necessary
  set l1 [label $top.l1 -text "Directory"]
  set e1 [entry $top.e1 -relief sunken -width 40]
  pack $l1 -side left
  pack $e1 -side left -expand yes -fill x
  $e1 insert end $env(DATADIR)

# Confirm date as necessary
  set l2 [label $top.l2 -text "UT Date"]
  set e2 [entry $top.e2 -relief sunken]
  pack $e2 -side right -expand yes -fill x
  pack $l2 -side right
  $e2 insert end $env(TODAY2)

# Get the values and set the environmental variables
  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}
  if {$bv==0} {
    cgs3drCursor watch red white
    set dataRoot [string trim [$e1 get]]
    if {$dataRoot==""} {set dataRoot $env(HOME)}
    set utRoot [string trim [$e2 get]]
    if {$utRoot==""} {set utRoot [exec /usr/bin/date -u +%Y%m%d]}
    cgs3drSetEnv $dataRoot $utRoot
    cgs3drInit
  } else {
    cgs3drInform "Re-initialisation aborted by user request!"
  }
  destroy .cgs3drDialogue
  cgs3drCursor arrow green black
}
