proc cgs4drCursor {type fgcol bgcol} {
  if {[winfo exists .cred4] == 1}   {.cred4   config -cursor [list $type $fgcol $bgcol]}
  if {[winfo exists .red4] == 1}    {.red4    config -cursor [list $type $fgcol $bgcol]}
  if {[winfo exists .p4] == 1}      {.p4      config -cursor [list $type $fgcol $bgcol]}
  if {[winfo exists .p4_plot] == 1} {.p4_plot config -cursor [list $type $fgcol $bgcol]}
  if {[winfo exists .qman] == 1}    {.qman    config -cursor [list $type $fgcol $bgcol]}
}
