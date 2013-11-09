#
proc ImgExecWait {task taskargs} {
  global Waiting


  set done 0
  . configure -cursor watch
  set Waiting 1
  img obey $task $taskargs -inform {HandleOutput %V} -endmsg {set done 1}
  tkwait variable done
  set Waiting 0
  . configure -cursor arrow
  }

proc ImgExecNoWait {task taskargs} {
  set done 0
  img obey $task $taskargs -inform {HandleOutput %V}
  }

proc ImgExecWaitNoMsg {task taskargs} {
  global Waiting


  set done 0
  . configure -cursor watch
  set Waiting 1
  img obey $task $taskargs -inform {} -endmsg {set done 1}
  tkwait variable done
  set Waiting 0
  . configure -cursor arrow
  }
#