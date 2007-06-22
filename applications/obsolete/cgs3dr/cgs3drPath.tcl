proc cgs3drPath {taskname} {
  set count 0
  while {[$taskname path]==0} {
    after 100
    incr count
    if {$count>100} {
      if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
      set frame [dialogStart .cgs3drDialogue "Task Load Error" error Dismiss]
      cgs3drCursor pirate orange black
      .cgs3drDialogue config -cursor {arrow green black}

      set label [label $frame.lab -text "Time out waiting for $taskname to load" -width 40]
      pack $label -in $frame -side left

      set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
      if {$bv==0} {
        cgs3drCursor watch red white
        $taskname kill
      }
      cgs3drCursor arrow green black
      destroy .cgs3drDialogue
    }
  }
}
