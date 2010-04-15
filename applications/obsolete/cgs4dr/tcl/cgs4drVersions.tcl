proc cgs4drVersions {taskname item} {
  set litem [string trim [string tolower $item]]
  if {$litem=="cgs4dr"} {
    set message "This is Portable-CGS4DR V1.3-0\n"
  } elseif {$litem=="tcl/tk"} {
    global tk_version
    set message "This is Tcl/tk V$tk_version\n"
  } elseif {$litem=="author"} {
    set message "Author: Phil Daly, pnd@jach.hawaii.edu\n"
  } else {
    set message ""
  }
  cgs4drInform $taskname $message
}
