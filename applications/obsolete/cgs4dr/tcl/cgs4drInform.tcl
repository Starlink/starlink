proc cgs4drInform {taskname message} {

# Set a local task name
  global QmanTask
  global P4Task
  global Cred4Task
  global Red4Task
  set local_task [string tolower [string trim $taskname]]

# qman task
  if {[string match $QmanTask $local_task]} {
    global QmanAbort
    if {[string index $message 0] == "!"} {set QmanAbort 1}
    global QmanWidgets
    $QmanWidgets(OUTPUT) configure -state normal
    $QmanWidgets(OUTPUT) insert end "$message\n"
    $QmanWidgets(OUTPUT) yview -pickplace end
    $QmanWidgets(OUTPUT) configure -state disabled

# p4 task
  } elseif {[string match $P4Task $local_task]} {
    global P4Widgets
    $P4Widgets(OUTPUT) configure -state normal
    $P4Widgets(OUTPUT) insert end "$message\n"
    $P4Widgets(OUTPUT) yview -pickplace end
    $P4Widgets(OUTPUT) configure -state disabled

# cred4 task
  } elseif {[string match $Cred4Task $local_task]} {
    global Cred4Widgets
    $Cred4Widgets(OUTPUT) configure -state normal
    $Cred4Widgets(OUTPUT) insert end "$message\n"
    $Cred4Widgets(OUTPUT) yview -pickplace end
    $Cred4Widgets(OUTPUT) configure -state disabled

# red4 task
  } elseif {[string match $Red4Task $local_task]} {
    global Red4Widgets
    $Red4Widgets(OUTPUT) configure -state normal
    $Red4Widgets(OUTPUT) insert end "$message\n"
    $Red4Widgets(OUTPUT) yview -pickplace end
    $Red4Widgets(OUTPUT) configure -state disabled
  }

# update tasks
  update idletasks
}





