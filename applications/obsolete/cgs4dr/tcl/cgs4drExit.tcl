proc cgs4drExit {taskname cmd} {

# Define global variables
  global env
  global QmanTask
  global P4Task
  global Cred4Task
  global Red4Task
  global QmanAccess

# Pop-up a dialogue box to confirm exit for all tasks
  if {[string first cgs4dr $cmd] != -1} {
    if {[winfo exists .cgs4drDialogue]} {destroy .cgs4drDialogue}
    set frame [dialogStart .cgs4drDialogue "CGS4DR Exit Confirmation" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cgs4drDialogue config -cursor {arrow green black}
    set label [label $frame.lab -text "Confirm Exit from Portable-CGS4DR?" -width 40]
    pack $label -in $frame -side left

# If exit is OK, do it
    set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
    if {$bv==0} {
      cgs4drCursor watch red white

      cgs4drKillPack $taskname
      global QmanTask
      global P4Task
      global Red4Task
      global Cred4Task
      global Cred4Widgets

      if {$Cred4Widgets(REDUCTION_STATE)=="RUNNING"} {cred4Drstop $Cred4Task}

      if {[file exists $env(HOME)/cgs4dr_configs/default.qman]==1} {exec /usr/bin/rm -rf $env(HOME)/cgs4dr_configs/default.qman}
      $QmanTask obey save "$QmanAccess file=$env(HOME)/cgs4dr_configs/default.qman" -inform "cgs4drInform $QmanTask %V"

      if {[file exists $env(HOME)/cgs4dr_configs/default.p4]==1} {exec /usr/bin/rm -rf $env(HOME)/cgs4dr_configs/default.p4}
      $P4Task obey save "file=$env(HOME)/cgs4dr_configs/default.p4 port=-1" -inform "cgs4drInform $P4Task %V"
      $P4Task obey close_nb "" -inform "cgs4drInform $P4Task %V"

      if {[file exists $env(HOME)/cgs4dr_configs/default.cred4]==1} {exec /usr/bin/rm -rf $env(HOME)/cgs4dr_configs/default.cred4}
      $Cred4Task obey save_config "config_file=$env(HOME)/cgs4dr_configs/default.cred4" -inform "cgs4drInform $Cred4Task %V"
      $Cred4Task obey close_qfile "" -inform "cgs4drInform $Cred4Task %V"
      $Cred4Task obey close_nb "" -inform "cgs4drInform $Cred4Task %V"

      nbs stop
      $QmanTask kill
      $P4Task kill
      $Red4Task kill
      $Cred4Task kill
      cgs4drAdamnet stop
      adamtask.exit
      exit 0

    } else {
      cgs4drCursor arrow green black
    }
    destroy .cgs4drDialogue

# Pop-up a dialogue box to confirm exit from specific task
  } else {
    if {[winfo exists .cgs4drDialogue]} {destroy .cgs4drDialogue}
    set frame [dialogStart .cgs4drDialogue "$taskname Exit Confirmation" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cgs4drDialogue config -cursor {arrow green black}
    set label [label $frame.lab -text "Confirm Exit from ${taskname}?" -width 40]
    pack $label -in $frame -side left

# Set local task name
    set local_task [string tolower [string trim $taskname]]

# If exit is OK, do it
    set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
    if {$bv==0} {
      cgs4drCursor watch red white

      cgs4drKillPack $taskname
      if {[string match $QmanTask $local_task]} {
        if {[file exists $env(HOME)/cgs4dr_configs/default.qman]==1} {exec /usr/bin/rm -rf $env(HOME)/cgs4dr_configs/default.qman}
        $taskname obey save "$QmanAccess file=$env(HOME)/cgs4dr_configs/default.qman" -inform "cgs4drInform $taskname %V"
        $taskname kill

      } elseif {[string match $P4Task $local_task]} {
        if {[file exists $env(HOME)/cgs4dr_configs/default.p4]==1} {exec /usr/bin/rm -rf $env(HOME)/cgs4dr_configs/default.p4}
        $taskname obey save "file=$env(HOME)/cgs4dr_configs/default.p4 port=-1" -inform "cgs4drInform $taskname %V"
        $taskname obey close_nb "" -inform "cgs4drInform $taskname %V"
        $taskname kill

      } elseif {[string match $Cred4Task $local_task]} {
        global Cred4Widgets
        nbs stop
        if {$Cred4Widgets(REDUCTION_STATE)=="RUNNING"} {cred4Drstop $Cred4Task}
        if {[file exists $env(HOME)/cgs4dr_configs/default.cred4]==1} {exec /usr/bin/rm -rf $env(HOME)/cgs4dr_configs/default.cred4}
        $taskname obey save_config "config_file=$env(HOME)/cgs4dr_configs/default.cred4" -inform "cgs4drInform $taskname %V"
        $taskname obey close_qfile "" -inform "cgs4drInform $taskname %V"
        $taskname obey close_nb "" -inform "cgs4drInform $taskname %V"
        $taskname kill

      } elseif {[string match $Red4Task $local_task]} {
        $taskname kill
      }
      cgs4drAdamnet stop
      adamtask.exit
      exit 0
    } else {
      cgs4drCursor arrow green black
    }
    destroy .cgs4drDialogue
  }
}
