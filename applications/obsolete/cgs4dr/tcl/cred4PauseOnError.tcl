proc cred4PauseOnError {taskname name el op} {
  global Cred4Widgets
  if {$Cred4Widgets(POE) == 1} {
    $taskname set pause_on_error TRUE -setresponse "cgs4drInform $taskname {Pause on error set to %V}"
  } else {
    $taskname set pause_on_error FALSE -setresponse "cgs4drInform $taskname {Pause on error set to %V}"
  }
}
