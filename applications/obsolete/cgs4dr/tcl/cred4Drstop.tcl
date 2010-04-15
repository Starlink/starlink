proc cred4Drstop {taskname} {
  global Cred4NoticeBoard
  global Cred4Widgets

  $taskname cancel reduce "" -inform "cgs4drInform $taskname %V"
  cgs4drInform $taskname "Stopping data reduction ..."
  $Cred4Widgets(RSTAT) configure -fg black -bg red
  set Cred4Widgets(REDUCTION_STATE) STOPPED
  pack forget $Cred4Widgets(DRSTOP)
  pack $Cred4Widgets(DRSTART) -side left -padx 4m -pady 2m
}
