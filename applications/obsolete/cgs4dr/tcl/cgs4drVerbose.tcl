proc cgs4drVerbose {taskname name op el} {

# Set local task name
  global QmanTask
  global QmanWidgets
  global P4Task
  global P4Widgets
  global Cred4Task
  global Cred4Widgets
  global Red4Task
  global Red4Widgets
  set local_task [string tolower [string trim $taskname]]
  cgs4drCursor pirate orange black

# qman task
  if {[string match $QmanTask $local_task]} {
    global QmanAccess
    if {$QmanWidgets(VERBOSE) == 1} {
      $taskname obey verbose $QmanAccess -inform "cgs4drInform $taskname %V"
    } else {
      $taskname obey noverbose $QmanAccess -inform "cgs4drInform $taskname %V"
    }

# p4 task
  } elseif {[string match $P4Task $local_task]} {
    if {$P4Widgets(VERBOSE) == 1} {
      $taskname obey verbose "" -inform "cgs4drInform $taskname %V"
    } else {
      $taskname obey noverbose "" -inform "cgs4drInform $taskname %V"
    }

# cred4 task
  } elseif {[string match $Cred4Task $local_task]} {
    if {$Cred4Widgets(VERBOSE) == 1} {
      $taskname obey set_verbose "verbose=TRUE" -inform "cgs4drInform $taskname %V"
    } else {
      $taskname obey set_verbose "verbose=FALSE" -inform "cgs4drInform $taskname %V"
    }

# red4 task
  } elseif {[string match $Red4Task $local_task]} {
    if {$Red4Widgets(VERBOSE) == 1} {
      $taskname obey set_verbose "verbose=TRUE" -inform "cgs4drInform $taskname %V"
    } else {
      $taskname obey set_verbose "verbose=FALSE" -inform "cgs4drInform $taskname %V"
    }
  }
  cgs4drCursor arrow green black
}
