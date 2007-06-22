proc cgs3drCursor {type fgcol bgcol} {
  global env
  if {[catch {info exist $env(PID)}]==0 && [winfo exists .$env(PID)cgs3dr]==1} {.$env(PID)cgs3dr config -cursor [list $type $fgcol $bgcol]}
}
