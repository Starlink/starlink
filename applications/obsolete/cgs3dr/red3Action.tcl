proc red3Action {action} {
  cgs3drClear
  set action [string trim [string tolower $action]]
  switch $action {
    black_body
      {red3_black_body}
    cgs3_41
      {red3_cgs3_41}
    cgs3_42
      {red3_cgs3_42}
    cgs3_43
      {red3_cgs3_43}
    cgs3_bad_cycle
      {red3_cgs3_bad_cycle}
    cgs3_det
      {red3_cgs3_det}
    extract3
      {red3_extract3}
    adjoin3
      {red3_adjoin3}
    scale
      {red3_scale}
    cgs3_phred
      {red3_cgs3_phred}
    cgs3pol
      {red3_cgs3pol}
    default
      {cgs3drInform "red3Action error : No such action!"}
  }
}
