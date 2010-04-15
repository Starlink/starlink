proc cred4Drstart {taskname} {
  global Cred4NoticeBoard
  global Cred4Widgets

  $taskname obey reduce "" -inform "cgs4drInform $taskname %V"
  cgs4drClear $taskname
  if {$Cred4Widgets(PAUSE)==1} {
    cgs4drInform $taskname "Starting data reduction in PAUSED state ..."
    $Cred4Widgets(RSTAT) configure -fg black -bg orange
    set Cred4Widgets(REDUCTION_STATE) PAUSED
  } else {
    cgs4drInform $taskname "Starting data reduction ..."
    $Cred4Widgets(RSTAT) configure -fg black -bg green
    set Cred4Widgets(REDUCTION_STATE) RUNNING
  }
  pack forget $Cred4Widgets(DRSTART)
  pack $Cred4Widgets(DRSTOP) -side left -padx 4m -pady 2m
}
