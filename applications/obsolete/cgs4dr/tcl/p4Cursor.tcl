proc p4Cursor {taskname} {
#+
# Gets cursor position in data axis units and values of data, error, quality arrays at point
#-
  global P4Widgets
  global act_x
  global act_y
  global dataval
  global dataerr
  global dataqual

# Check to see if NBS exists
  set status [cgs4drCheckNbs p4]
  if {$status!=0} {return}

# Issue a message saying what to do
  cgs4drCursor pirate orange black
  set old_xhair $P4Widgets(CROSSHAIR)
  set P4Widgets(CROSSHAIR) 1
  crossHair

# Have to disable reset auto-plot for this an re-display image
  set old_reset $P4Widgets(RESETPLOT)
  if {$P4Widgets(RESETPLOT) != 0} {
    set P4Widgets(RESETPLOT) 0
    p4Plot $taskname
  }

# Do the cursoring on the given data set
  set done 0
  $taskname obey cursorval "port=$P4Widgets(PORT_NO)" -inform "cgs4drInform $taskname %V" -endmsg {set done 1}
  tkwait variable done

# Get the returns (this will hang if task dies for any reason!)
  set act_x -1
  $taskname get act_x -getresponse "p4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
  tkwait variable act_x

  set act_y -1
  $taskname get act_y -getresponse "p4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
  tkwait variable act_y

  set dataval -1
  $taskname get dataval -getresponse "p4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
  tkwait variable dataval

  set dataerr -1
  $taskname get dataerr -getresponse "p4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
  tkwait variable dataerr

  set dataqual -1
  $taskname get dataqual -getresponse "p4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
  tkwait variable dataqual

# Inform the user what happened
  set message [format "Pixel ( %f , %f ) has values:   Data = %10.4f  Error = %10.4f  Quality = %1i" \
    $act_x $act_y $dataval $dataerr $dataqual]
  cgs4drInform $taskname $message

# Change the cursor and exit
  set P4Widgets(CROSSHAIR) $old_xhair
  crossHair
  set P4Widgets(RESETPLOT) $old_reset
  cgs4drCursor arrow green black
}

proc p4GetResponse {task param value status} {
  global $param
  if {[lindex $status 0]=="SAI__OK"} {
    set $param $value
    return $param
  } else {
    cgs4drClear $task
    cgs4drInform $task "p4GetResponse error : $param = $value returned with status != SAI__OK"
  }
}
