proc cgs4drPath {taskname} {
#+
# Make sure we have a path to a task
#-

  set count 0
  while {[$taskname path]==0} {
    after 100
    incr count
    if {$count>100} {
      if {[winfo exists .cgs4drDialogue]} {destroy .cgs4drDialogue}
      set frame [dialogStart .cgs4drDialogue "Task Load Error" error Dismiss]
      cgs4drCursor pirate orange black
      .cgs4drDialogue config -cursor {arrow green black}

      set label [label $frame.lab -text "Time out waiting for $taskname to load" -width 40]
      pack $label -in $frame -side left

      set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
      if {$bv==0} {
        cgs4drCursor watch red white
        $taskname kill
      }
      cgs4drCursor arrow green black
      destroy .cgs4drDialogue
    }
  }
}
