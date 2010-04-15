proc red3_cgs3_41 {} {
  global Red3Task
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 cgs3_41 Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set t1 [frame $frame.t1]
  set t2 [frame $frame.t2]
  set t3 [frame $frame.t3]
  pack $t1 $t2 $t3 -fill both -expand yes

  set l1 [label $t1.l1 -text Input]
  set e1 [entry $t1.e1 -relief sunken -bd 2 -textvariable Red3Widgets(INPUT) -width 50]
  pack $l1 -side left
  pack $e1 -side right
  set l2 [label $t2.l2 -text Output]
  set e2 [entry $t2.e2 -relief sunken -bd 2 -textvariable Red3Widgets(OUTPUT) -width 50]
  pack $l2 -side left
  pack $e2 -side right

  set l3 [label $t3.l3 -text "Scan Start"]
  set e3 [entry $t3.e3 -relief sunken -bd 2 -textvariable Red3Widgets(STARTSCAN)]
  set l4 [label $t3.l4 -text "Scan End"]
  set e4 [entry $t3.e4 -relief sunken -bd 2 -textvariable Red3Widgets(ENDSCAN)]
  set l5 [label $t3.l5 -text "Nsigma"]
  set e5 [entry $t3.e5 -relief sunken -bd 2 -textvariable Red3Widgets(NSIGMA)]
  pack $l3 $e3 $l4 $e4 $l5 $e5 -side left

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set g1 [string trim [$e1 get]]
    set g2 [string trim [$e2 get]]
    set g3 [string trim [$e3 get]]
    set g4 [string trim [$e4 get]]
    set g5 [string trim [$e5 get]]
    if {$g1==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get INPUT parameter!"
    } elseif {$g2==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get OUTPUT parameter!"
    } elseif {$g3==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get STARTSCAN parameter!"
    } elseif {$g4==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get ENDSCAN parameter!"
    } elseif {$g5==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get NSIGMA parameter!"
    } else {
      set params "INPUT=$g1 STARTSCAN=$g3 ENDSCAN=$g4 NSIGMA=$g5 OUTPUT=$g2"
      $Red3Task obey cgs3_41 "${params}" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}

proc red3_cgs3_42 {} {
  global Red3Task
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 cgs3_42 Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set t1 [frame $frame.t1]
  set t2 [frame $frame.t2]
  set t3 [frame $frame.t3]
  pack $t1 $t2 $t3 -fill both -expand yes

  set l1 [label $t1.l1 -text Input]
  set e1 [entry $t1.e1 -relief sunken -bd 2 -textvariable Red3Widgets(INPUT) -width 50]
  pack $l1 -side left
  pack $e1 -side right
  set l2 [label $t2.l2 -text Output]
  set e2 [entry $t2.e2 -relief sunken -bd 2 -textvariable Red3Widgets(OUTPUT) -width 50]
  pack $l2 -side left
  pack $e2 -side right

  set l3 [label $t3.l3 -text "Scan Start"]
  set e3 [entry $t3.e3 -relief sunken -bd 2 -textvariable Red3Widgets(STARTSCAN)]
  set l4 [label $t3.l4 -text "Scan End"]
  set e4 [entry $t3.e4 -relief sunken -bd 2 -textvariable Red3Widgets(ENDSCAN)]
  set l5 [label $t3.l5 -text "Nsigma"]
  set e5 [entry $t3.e5 -relief sunken -bd 2 -textvariable Red3Widgets(NSIGMA)]
  pack $l3 $e3 $l4 $e4 $l5 $e5 -side left

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set g1 [string trim [$e1 get]]
    set g2 [string trim [$e2 get]]
    set g3 [string trim [$e3 get]]
    set g4 [string trim [$e4 get]]
    set g5 [string trim [$e5 get]]
    if {$g1==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get INPUT parameter!"
    } elseif {$g2==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get OUTPUT parameter!"
    } elseif {$g3==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get STARTSCAN parameter!"
    } elseif {$g4==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get ENDSCAN parameter!"
    } elseif {$g5==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get NSIGMA parameter!"
    } else {
      set params "INPUT=$g1 STARTSCAN=$g3 ENDSCAN=$g4 NSIGMA=$g5 OUTPUT=$g2"
      $Red3Task obey cgs3_42 "${params}" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}

proc red3_cgs3_43 {} {
  global Red3Task
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 cgs3_43 Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set t1 [frame $frame.t1]
  set t2 [frame $frame.t2]
  pack $t1 $t2 -fill both -expand yes

  set l1 [label $t1.l1 -text Input]
  set e1 [entry $t1.e1 -relief sunken -bd 2 -textvariable Red3Widgets(INPUT) -width 50]
  pack $l1 -side left
  pack $e1 -side right
  set l2 [label $t2.l2 -text Output]
  set e2 [entry $t2.e2 -relief sunken -bd 2 -textvariable Red3Widgets(OUTPUT) -width 50]
  pack $l2 -side left
  pack $e2 -side right

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set g1 [string trim [$e1 get]]
    set g2 [string trim [$e2 get]]
    if {$g1==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get INPUT parameter!"
    } elseif {$g2==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get OUTPUT parameter!"
    } else {
      set params "INPUT=$g1 OUTPUT=$g2"
      $Red3Task obey cgs3_43 "${params}" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}

proc red3_cgs3_det {} {
  global Red3Task
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 cgs3_det Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set t1 [frame $frame.t1]
  set t2 [frame $frame.t2]
  set t3 [frame $frame.t3]
  pack $t1 $t2 $t3 -fill both -expand yes

  set l1 [label $t1.l1 -text Input]
  set e1 [entry $t1.e1 -relief sunken -bd 2 -textvariable Red3Widgets(INPUT) -width 50]
  pack $l1 -side left
  pack $e1 -side right
  set l2 [label $t2.l2 -text Output]
  set e2 [entry $t2.e2 -relief sunken -bd 2 -textvariable Red3Widgets(OUTPUT) -width 50]
  pack $l2 -side left
  pack $e2 -side right

  set l3 [label $t3.l3 -text "Scan Start"]
  set e3 [entry $t3.e3 -relief sunken -bd 2 -textvariable Red3Widgets(STARTSCAN)]
  set l4 [label $t3.l4 -text "Scan End"]
  set e4 [entry $t3.e4 -relief sunken -bd 2 -textvariable Red3Widgets(ENDSCAN)]
  set l5 [label $t3.l5 -text "Detector"]
  set e5 [entry $t3.e5 -relief sunken -bd 2 -textvariable Red3Widgets(DETECTOR)]
  pack $l3 $e3 $l4 $e4 $l5 $e5 -side left

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set g1 [string trim [$e1 get]]
    set g2 [string trim [$e2 get]]
    set g3 [string trim [$e3 get]]
    set g4 [string trim [$e4 get]]
    set g5 [string trim [$e5 get]]
    if {$g1==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get INPUT parameter!"
    } elseif {$g2==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get OUTPUT parameter!"
    } elseif {$g3==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get STARTSCAN parameter!"
    } elseif {$g4==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get ENDSCAN parameter!"
    } elseif {$g5==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get DETECTOR parameter!"
    } else {
      set params "INPUT=$g1 DETECTOR=$g5 STARTSCAN=$g3 ENDSCAN=$g4 OUTPUT=$g2"
      $Red3Task obey cgs3_det "${params}" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}

proc red3_cgs3_bad_cycle {} {
  global Red3Task
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 cgs3_bad_cycle Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set t1 [frame $frame.t1]
  pack $t1 -fill both -expand yes

  set l1 [label $t1.l1 -text Image]
  set e1 [entry $t1.e1 -relief sunken -bd 2 -textvariable Red3Widgets(IMAGE) -width 50]
  set l2 [label $t1.l2 -text "Bad Cycle"]
  set e2 [entry $t1.e2 -relief sunken -bd 2 -textvariable Red3Widgets(BAD_CYCLE)]
  pack $l1 $e1 -side left
  pack $e2 $l2 -side right

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set g1 [string trim [$e1 get]]
    set g2 [string trim [$e2 get]]
    if {$g1==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get INPUT parameter!"
    } elseif {$g2==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get BAD_CYCLE parameter!"
    } else {
      set params "INPUT=$g1 BAD_CYCLE=$g2"
      $Red3Task obey cgs3_bad_cycle "${params}" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}

proc red3_adjoin3 {} {
  global Red3Task
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 adjoin3 Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set t1 [frame $frame.t1]
  set t2 [frame $frame.t2]
  set t3 [frame $frame.t3]
  pack $t1 $t2 $t3 -fill both -expand yes

  set l1 [label $t1.l1 -text "First Spectrum"]
  set e1 [entry $t1.e1 -relief sunken -bd 2 -textvariable Red3Widgets(SPECTRUM) -width 50]
  pack $l1 -side left
  pack $e1 -side right
  set l2 [label $t2.l2 -text "Second Spectrum"]
  set e2 [entry $t2.e2 -relief sunken -bd 2 -textvariable Red3Widgets(SPECTRUM1) -width 50]
  pack $l2 -side left
  pack $e2 -side right
  set l3 [label $t3.l3 -text "Output Spectrum"]
  set e3 [entry $t3.e3 -relief sunken -bd 2 -textvariable Red3Widgets(OUTPUT) -width 50]
  pack $l3 -side left
  pack $e3 -side right

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set g1 [string trim [$e1 get]]
    set g2 [string trim [$e2 get]]
    set g3 [string trim [$e3 get]]
    if {$g1==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get SPECTRUM parameter!"
    } elseif {$g2==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get SPECTRUM1 parameter!"
    } elseif {$g3==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get OUTPUT parameter!"
    } else {
      set params "INPUT=$g1 SPECTRUM1=$g2 OUTPUT=$g3"
      $Red3Task obey adjoin3 "${params}" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}

proc red3_extract3 {} {
  global Red3Task
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 extract3 Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set t1 [frame $frame.t1]
  set t2 [frame $frame.t2]
  set t3 [frame $frame.t3]
  pack $t1 $t3 $t2 -fill both -expand yes

  set l1 [label $t1.l1 -text "Input Image"]
  set e1 [entry $t1.e1 -relief sunken -bd 2 -textvariable Red3Widgets(IMAGE) -width 50]
  pack $l1 -side left
  pack $e1 -side right
  set l2 [label $t2.l2 -text "Y-start"]
  set e2 [entry $t2.e2 -relief sunken -bd 2 -textvariable Red3Widgets(YSTART)]
  set l3 [label $t2.l3 -text "Y-end"]
  set e3 [entry $t2.e3 -relief sunken -bd 2 -textvariable Red3Widgets(YEND)]
  pack $l2 $e2 $l3 $e3 -side left
  set l4 [label $t3.l4 -text "Output Spectrum"]
  set e4 [entry $t3.e4 -relief sunken -bd 2 -textvariable Red3Widgets(SPECTRUM) -width 50]
  pack $l4 -side left
  pack $e4 -side right

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set g1 [string trim [$e1 get]]
    set g2 [string trim [$e2 get]]
    set g3 [string trim [$e3 get]]
    set g4 [string trim [$e4 get]]
    if {$g1==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get IMAGE parameter!"
    } elseif {$g2==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get YSTART parameter!"
    } elseif {$g3==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get YEND parameter!"
    } elseif {$g4==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get SPECTRUM parameter!"
    } else {
      set params "IMAGE=$g1 YSTART=$g2 YEND=$g3 SPECTRUM=$g4"
      $Red3Task obey extract3 "${params}" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}

proc red3_cgs3pol {} {
  global Cgs3drTask
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 cgs3pol Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set l1 [label $frame.l1 -text "Run Number"]
  set e1 [entry $frame.e1 -relief sunken -bd 2 -textvariable Red3Widgets(RNUM)]
  pack $l1 -side left
  pack $e1 -side right

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set rnum [string trim [$e1 get]]
    if {$rnum=="" || [expr $rnum + 0]<=0} {
      cgs3drInform "red3_cgs3pol error : run number incorrectly specified!"
      cgs3drCursor arrow green black
      return
    } else {
      set Red3Widgets(RNUM) $rnum
      $Cgs3drTask obey reduce_run "runnum=$rnum" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}

proc red3_cgs3_phred {} {
  global Red3Task
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 cgs3_phred Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set t1 [frame $frame.t1]
  set t2 [frame $frame.t2]
  set t3 [frame $frame.t3]
  set t4 [frame $frame.t4]
  pack $t1 $t2 $t3 $t4 -fill both -expand yes

  set l1 [label $t1.l1 -text Input]
  set e1 [entry $t1.e1 -relief sunken -bd 2 -textvariable Red3Widgets(SPECT) -width 50]
  pack $l1 -side left
  pack $e1 -side right
  set l2 [label $t2.l2 -text Output]
  set e2 [entry $t2.e2 -relief sunken -bd 2 -textvariable Red3Widgets(FILE) -width 50]
  pack $l2 -side left
  pack $e2 -side right

  set l3 [label $t3.l3 -text "Scan Start"]
  set e3 [entry $t3.e3 -relief sunken -bd 2 -textvariable Red3Widgets(STARTSCAN)]
  set l4 [label $t3.l4 -text "Scan End"]
  set e4 [entry $t3.e4 -relief sunken -bd 2 -textvariable Red3Widgets(ENDSCAN)]
  set l5 [label $t3.l5 -text "Sigma"]
  set e5 [entry $t3.e5 -relief sunken -bd 2 -textvariable Red3Widgets(NSIGMA)]
  pack $l3 $e3 $l4 $e4 $l5 $e5 -side left

  set l6 [label $t4.l6 -text "Det Start"]
  set e6 [entry $t4.e6 -relief sunken -bd 2 -textvariable Red3Widgets(IST)]
  set l7 [label $t4.l7 -text "Det End"]
  set e7 [entry $t4.e7 -relief sunken -bd 2 -textvariable Red3Widgets(IEN)]
  set c8 [checkbutton $t4.c8 -text "Verbose Output" -variable Red3Widgets(VERBOSE_PH)]
  pack $l6 $e6 $l7 $e7 -side left
  pack $c8 -side right

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set g1 [string trim [$e1 get]]
    set g2 [string trim [$e2 get]]
    set g3 [string trim [$e3 get]]
    set g4 [string trim [$e4 get]]
    set g5 [string trim [$e5 get]]
    set g6 [string trim [$e6 get]]
    set g7 [string trim [$e7 get]]
    if {$g1==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get SPECT parameter!"
    } elseif {$g2==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get FILE parameter!"
    } elseif {$g3==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get STARTSCAN parameter!"
    } elseif {$g4==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get ENDSCAN parameter!"
    } elseif {$g5==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get SIGMA_LIM parameter!"
    } elseif {$g6==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get IST parameter!"
    } elseif {$g7==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get IEN parameter!"
    } else {
      if {$Red3Widgets(VERBOSE_PH)==1} {
        set params "SPECT=$g1 FILE=$g2 STARTSCAN=$g3 ENDSCAN=$g4 IST=$g6 IEND=$g7 SIGMA_LIM=$g5 VERBOSE=T"
      } else {
        set params "SPECT=$g1 FILE=$g2 STARTSCAN=$g3 ENDSCAN=$g4 IST=$g6 IEND=$g7 SIGMA_LIM=$g5 VERBOSE=F"
      }
      $Red3Task obey cgs3_phred "${params}" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}

proc red3_black_body {} {
  global Red3Task
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 black_body Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set t1 [frame $frame.t1]
  set t2 [frame $frame.t2]
  set t3 [frame $frame.t3]
  pack $t1 $t2 $t3 -fill both -expand yes

  set l1 [label $t1.l1 -text Template]
  set e1 [entry $t1.e1 -relief sunken -bd 2 -textvariable Red3Widgets(TEMPLATE) -width 50]
  pack $l1 -side left
  pack $e1 -side right
  set l2 [label $t2.l2 -text Output]
  set e2 [entry $t2.e2 -relief sunken -bd 2 -textvariable Red3Widgets(OUTPUT) -width 50]
  pack $l2 -side left
  pack $e2 -side right

  set l3 [label $t3.l3 -text "BB Temp"]
  set e3 [entry $t3.e3 -relief sunken -bd 2 -textvariable Red3Widgets(BB_TEMP)]
  set l4 [label $t3.l4 -text "Ref Wave"]
  set e4 [entry $t3.e4 -relief sunken -bd 2 -textvariable Red3Widgets(REFWAVE)]
  set l5 [label $t3.l5 -text "Ref Flux"]
  set e5 [entry $t3.e5 -relief sunken -bd 2 -textvariable Red3Widgets(REFFLUX)]
  pack $l3 $e3 -side left
  pack $e5 $l5 $e4 $l4 -side right

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set g1 [string trim [$e1 get]]
    set g2 [string trim [$e2 get]]
    set g3 [string trim [$e3 get]]
    set g4 [string trim [$e4 get]]
    set g5 [string trim [$e5 get]]
    if {$g1==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get TEMPLATE parameter!"
    } elseif {$g2==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get OUTPUT parameter!"
    } elseif {$g3==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get BB_TEMP parameter!"
    } elseif {$g4==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get REFWAVE parameter!"
    } elseif {$g5==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get REFFLUX parameter!"
    } else {
      set params "INPUT=$g1 BB_TEMP=$g3 REFWAVE=$g4 REFFLUX=$g5 OUTPUT=$g2"
      $Red3Task obey black_body "${params}" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}

proc red3_scale {} {
  global Red3Task
  global Red3Widgets

  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Red3 scale Action" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set t1 [frame $frame.t1]
  set t2 [frame $frame.t2]
  set t3 [frame $frame.t3]
  pack $t1 $t2 $t3 -fill both -expand yes

  set l1 [label $t1.l1 -text Input]
  set e1 [entry $t1.e1 -relief sunken -bd 2 -textvariable Red3Widgets(INPUT) -width 50]
  pack $l1 -side left
  pack $e1 -side right
  set l2 [label $t2.l2 -text Output]
  set e2 [entry $t2.e2 -relief sunken -bd 2 -textvariable Red3Widgets(OUTPUT) -width 50]
  pack $l2 -side left
  pack $e2 -side right

  set l3 [label $t3.l3 -text "Factor"]
  set e3 [entry $t3.e3 -relief sunken -bd 2 -textvariable Red3Widgets(FACTOR)]
  set l4 [label $t3.l4 -text "Constant"]
  set e4 [entry $t3.e4 -relief sunken -bd 2 -textvariable Red3Widgets(CONSTANT)]
  set c5 [checkbutton $t3.c5 -text "Gaussian Errors" -variable Red3Widgets(ERRORS)]
  pack $l3 $e3 $l4 $e4 -side left
  pack $c5 -side right

  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drCursor watch red white
    set g1 [string trim [$e1 get]]
    set g2 [string trim [$e2 get]]
    set g3 [string trim [$e3 get]]
    set g4 [string trim [$e4 get]]
    if {$g1==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get INPUT parameter!"
    } elseif {$g2==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get OUTPUT parameter!"
    } elseif {$g3==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get FACTOR parameter!"
    } elseif {$g4==""} {
      cgs3drClear
      cgs3drInform "Red3Applics error : Failed to get CONSTANT parameter!"
    } else {
      if {$Red3Widgets(ERRORS)==1} {
        set params "INPUT=$g1 FACTOR=$g3 CONSTANT=$g4 OUTPUT=$g2 ERRORS=GAUSSIAN"
      } else {
        set params "INPUT=$g1 FACTOR=$g3 CONSTANT=$g4 OUTPUT=$g2 ERRORS=NONE"
      }
      $Red3Task obey scale "${params}" -inform "cgs3drInform %V"
    }
  }
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}
