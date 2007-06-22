proc cgs3drReduce {rtype} {
  global Cgs3drWidgets
  global Cgs3drTask
  set utype [string toupper [string trim $rtype]]
  set ltype [string tolower [string trim $rtype]]
  cgs3drClear
  set number [string trim [$Cgs3drWidgets(RNUM) get]]
  if {$number==""} {
    set message "cgs3drReduce $ltype error : No run number specified!"
    cgs3drInform $message
  } else {
    if {$utype=="RUN"} {
      $Cgs3drTask obey reduce_run "runnum=$number" -inform "cgs3drInform %V"
    } elseif {$utype=="GRP"} {
      $Cgs3drTask obey reduce_grp "grpnum=$number" -inform "cgs3drInform %V"
    } elseif {$utype=="PHOT"} {
      $Cgs3drTask obey reduce_phot "photnum=$number" -inform "cgs3drInform %V"
    } else {
      set message "cgs3drReduce $ltype error : Unrecognised reduction type!"
      cgs3drInform $message
    }
  }
}
