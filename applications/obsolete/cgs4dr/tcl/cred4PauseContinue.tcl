proc cred4PauseContinue {taskname name el op} {
  global env
  global Cred4Widgets
  global Cred4NoticeBoard
  nbs put ${Cred4NoticeBoard}.flags.pause_reduction $Cred4Widgets(PAUSE)

  if {$Cred4Widgets(PAUSE)==0} {
    if {$Cred4Widgets(REDUCTION_STATE)=="RUNNING"} {
      $Cred4Widgets(RSTAT) configure -fg black -bg green
      set Cred4Widgets(REDUCTION_STATE) RUNNING
    } elseif {$Cred4Widgets(REDUCTION_STATE)=="STOPPED"} {
      $Cred4Widgets(RSTAT) configure -fg black -bg red
      set Cred4Widgets(REDUCTION_STATE) STOPPED
    } elseif {$Cred4Widgets(REDUCTION_STATE)=="PAUSED"} {
      cgs4drInform $taskname "Resuming data reduction ..."
      $Cred4Widgets(RSTAT) configure -fg black -bg green
      set Cred4Widgets(REDUCTION_STATE) RUNNING
    }
  } else {
    if {$Cred4Widgets(REDUCTION_STATE)=="RUNNING"} {
      cgs4drInform $taskname "Data reduction will pause after next item ..."
      $Cred4Widgets(RSTAT) configure -fg black -bg orange
      set Cred4Widgets(REDUCTION_STATE) PAUSED
    } elseif {$Cred4Widgets(REDUCTION_STATE)=="STOPPED"} {
      $Cred4Widgets(RSTAT) configure -fg black -bg red
      set Cred4Widgets(REDUCTION_STATE) STOPPED
    } elseif {$Cred4Widgets(REDUCTION_STATE)=="PAUSED"} {
      $Cred4Widgets(RSTAT) configure -fg black -bg orange
      set Cred4Widgets(REDUCTION_STATE) PAUSED
      if {$env(DOMAIN) == "ukirt.jach.hawaii.edu."} {puts "\a\nALARM: Portable-CGS4DR is PAUSED! ([exec /usr/bin/date])\n"}
    }
  }
}
