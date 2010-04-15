proc cgs3drVersions {item} {
  cgs3drClear
  set litem [string trim [string tolower $item]]
  if {$litem=="cgs3dr"} {
    set message "This is Portable-CGS3DR VPKG_VERS"
  } elseif {$litem=="tcl/tk"} {
    global tk_version
    set message "This is Tcl/tk V$tk_version"
  } elseif {$litem=="author"} {
    set message "Author: Phil Daly, pnd@jach.hawaii.edu"
  } else {
    set message ""
  }
  cgs3drInform $message
}
