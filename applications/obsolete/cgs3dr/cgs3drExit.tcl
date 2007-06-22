proc cgs3drExit {} {

# Define global variables
  global env
  global FigTask
  global Red3Task
  global Cgs3drTask

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "CGS3DR Exit Confirmation" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}
  set label [label $frame.lab -text "Confirm Exit from Portable-CGS3DR?" -width 40]
  pack $label -in $frame -side left

# If exit is OK, do it
  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    $FigTask kill
    $Red3Task kill
    $Cgs3drTask kill
    adamtask.exit
    exit 0
  } else {
    cgs3drCursor arrow green black
  }
  destroy .cgs3drDialogue
}
