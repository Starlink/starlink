proc cgs4drClear {taskname} {
  global QmanTask
  global P4Task
  global Cred4Task
  global Red4Task
  if {[string match $QmanTask $taskname]} {
    global QmanWidgets
    $QmanWidgets(OUTPUT) configure -state normal
    $QmanWidgets(OUTPUT) delete 1.0 end
    $QmanWidgets(OUTPUT) configure -state disabled
  } elseif {[string match $P4Task $taskname]} {
    global P4Widgets
    $P4Widgets(OUTPUT) configure -state normal
    $P4Widgets(OUTPUT) delete 1.0 end
    $P4Widgets(OUTPUT) configure -state disabled
  } elseif {[string match $Cred4Task $taskname]} {
    global Cred4Widgets
    $Cred4Widgets(OUTPUT) configure -state normal
    $Cred4Widgets(OUTPUT) delete 1.0 end
    $Cred4Widgets(OUTPUT) configure -state disabled
  } elseif {[string match $Red4Task $taskname]} {
    global Red4Widgets
    $Red4Widgets(OUTPUT) configure -state normal
    $Red4Widgets(OUTPUT) delete 1.0 end
    $Red4Widgets(OUTPUT) configure -state disabled
  }
}
