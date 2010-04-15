proc red4Update {item value} {
  global env
  global Red4Widgets
  if {[string trim ${item}] == "red4Int"} {
    selection clear $Red4Widgets(AI_ENTRY)
    $Red4Widgets(AI_ENTRY) delete 0 end
    $Red4Widgets(AI_ENTRY) insert end $Red4Widgets(DIN)

  } elseif {[string trim ${item}] == "red4ReadObs"} {
    selection clear $Red4Widgets(ERO_ENTRY)
    $Red4Widgets(ERO_ENTRY) delete 0 end
    $Red4Widgets(ERO_ENTRY) insert end $Red4Widgets(DRO)

  } elseif {[string trim ${item}] == "red4TwoDLineFit"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(TWD_ENT01)
      $Red4Widgets(TWD_ENT01) delete 0 end
      $Red4Widgets(TWD_ENT01) insert end $Red4Widgets(DRO)
      selection clear $Red4Widgets(TWD_ENT02)
      $Red4Widgets(TWD_ENT02) delete 0 end
      $Red4Widgets(TWD_ENT02) insert end 1
      selection clear $Red4Widgets(TWD_ENT03)
      $Red4Widgets(TWD_ENT03) delete 0 end
      $Red4Widgets(TWD_ENT03) insert end 256
      selection clear $Red4Widgets(TWD_ENT04)
      $Red4Widgets(TWD_ENT04) delete 0 end
      $Red4Widgets(TWD_ENT04) insert end 1
      selection clear $Red4Widgets(TWD_ENT05)
      $Red4Widgets(TWD_ENT05) delete 0 end
      $Red4Widgets(TWD_ENT05) insert end 256
    } elseif {[string trim ${value}] == "TWD_ENT01"} {
      selection clear $Red4Widgets(TWD_ENT01)
      $Red4Widgets(TWD_ENT01) delete 0 end
      $Red4Widgets(TWD_ENT01) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "TWD_ENT02"} {
      selection clear $Red4Widgets(TWD_ENT02)
      $Red4Widgets(TWD_ENT02) delete 0 end
      $Red4Widgets(TWD_ENT02) insert end 1
    } elseif {[string trim ${value}] == "TWD_ENT03"} {
      selection clear $Red4Widgets(TWD_ENT03)
      $Red4Widgets(TWD_ENT03) delete 0 end
      $Red4Widgets(TWD_ENT03) insert end 256
    } elseif {[string trim ${value}] == "TWD_ENT04"} {
      selection clear $Red4Widgets(TWD_ENT04)
      $Red4Widgets(TWD_ENT04) delete 0 end
      $Red4Widgets(TWD_ENT04) insert end 1
    } elseif {[string trim ${value}] == "TWD_ENT05"} {
      selection clear $Red4Widgets(TWD_ENT05)
      $Red4Widgets(TWD_ENT05) delete 0 end
      $Red4Widgets(TWD_ENT05) insert end 256
    }

  } elseif {[string trim ${item}] == "red4TwoLineFit"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(TLF_ENT01)
      $Red4Widgets(TLF_ENT01) delete 0 end
      $Red4Widgets(TLF_ENT01) insert end $Red4Widgets(DRO)
      selection clear $Red4Widgets(TLF_ENT02)
      $Red4Widgets(TLF_ENT02) delete 0 end
      $Red4Widgets(TLF_ENT02) insert end 29
      selection clear $Red4Widgets(TLF_ENT03)
      $Red4Widgets(TLF_ENT03) delete 0 end
      $Red4Widgets(TLF_ENT03) insert end 29
      selection clear $Red4Widgets(TLF_ENT04)
      $Red4Widgets(TLF_ENT04) delete 0 end
      $Red4Widgets(TLF_ENT04) insert end 1
      selection clear $Red4Widgets(TLF_ENT05)
      $Red4Widgets(TLF_ENT05) delete 0 end
      $Red4Widgets(TLF_ENT05) insert end 256
      set Red4Widgets(TLF_RAD01) 1
    } elseif {[string trim ${value}] == "TLF_ENT01"} {
      selection clear $Red4Widgets(TLF_ENT01)
      $Red4Widgets(TLF_ENT01) delete 0 end
      $Red4Widgets(TLF_ENT01) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "TLF_ENT02"} {
      selection clear $Red4Widgets(TLF_ENT02)
      $Red4Widgets(TLF_ENT02) delete 0 end
      $Red4Widgets(TLF_ENT02) insert end 29
    } elseif {[string trim ${value}] == "TLF_ENT03"} {
      selection clear $Red4Widgets(TLF_ENT03)
      $Red4Widgets(TLF_ENT03) delete 0 end
      $Red4Widgets(TLF_ENT03) insert end 29
    } elseif {[string trim ${value}] == "TLF_ENT04"} {
      selection clear $Red4Widgets(TLF_ENT04)
      $Red4Widgets(TLF_ENT04) delete 0 end
      $Red4Widgets(TLF_ENT04) insert end 1
    } elseif {[string trim ${value}] == "TLF_ENT05"} {
      selection clear $Red4Widgets(TLF_ENT05)
      $Red4Widgets(TLF_ENT05) delete 0 end
      $Red4Widgets(TLF_ENT05) insert end 256
    } elseif {[string trim ${value}] == "TLF_RAD01"} {
      set Red4Widgets(TLF_RAD01) 1
    }

  } elseif {[string trim ${item}] == "red4FileStd"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(FS_ENT01)
      $Red4Widgets(FS_ENT01) delete 0 end
      $Red4Widgets(FS_ENT01) insert end $Red4Widgets(DRG)
      selection clear $Red4Widgets(FS_ENT02)
      $Red4Widgets(FS_ENT02) delete 0 end
      $Red4Widgets(FS_ENT02) insert end 5000.0
      selection clear $Red4Widgets(FS_ENT03)
      $Red4Widgets(FS_ENT03) delete 0 end
      $Red4Widgets(FS_ENT03) insert end 2.2
      selection clear $Red4Widgets(FS_ENT04)
      $Red4Widgets(FS_ENT04) delete 0 end
      $Red4Widgets(FS_ENT04) insert end 29
      selection clear $Red4Widgets(FS_ENT05)
      $Red4Widgets(FS_ENT05) delete 0 end
      $Red4Widgets(FS_ENT05) insert end 29
    } elseif {[string trim ${value}] == "FS_ENT01"} {
      selection clear $Red4Widgets(FS_ENT01)
      $Red4Widgets(FS_ENT01) delete 0 end
      $Red4Widgets(FS_ENT01) insert end $Red4Widgets(DRG)
    } elseif {[string trim ${value}] == "FS_ENT02"} {
      selection clear $Red4Widgets(FS_ENT02)
      $Red4Widgets(FS_ENT02) delete 0 end
      $Red4Widgets(FS_ENT02) insert end 5000.0
    } elseif {[string trim ${value}] == "FS_ENT03"} {
      selection clear $Red4Widgets(FS_ENT03)
      $Red4Widgets(FS_ENT03) delete 0 end
      $Red4Widgets(FS_ENT03) insert end 2.2
    } elseif {[string trim ${value}] == "FS_ENT04"} {
      selection clear $Red4Widgets(FS_ENT04)
      $Red4Widgets(FS_ENT04) delete 0 end
      $Red4Widgets(FS_ENT04) insert end 29
    } elseif {[string trim ${value}] == "FS_ENT05"} {
      selection clear $Red4Widgets(FS_ENT05)
      $Red4Widgets(FS_ENT05) delete 0 end
      $Red4Widgets(FS_ENT05) insert end 29
    }

  } elseif {[string trim ${item}] == "red4Fits"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(FI_ENT01)
      $Red4Widgets(FI_ENT01) delete 0 end
      $Red4Widgets(FI_ENT01) insert end $Red4Widgets(DRO)
      selection clear $Red4Widgets(FI_ENT02)
      $Red4Widgets(FI_ENT02) delete 0 end
      $Red4Widgets(FI_ENT02) insert end OBJECT
    } elseif {[string trim ${value}] == "FI_ENT01"} {
      selection clear $Red4Widgets(FI_ENT01)
      $Red4Widgets(FI_ENT01) delete 0 end
      $Red4Widgets(FI_ENT01) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "FI_ENT02"} {
      selection clear $Red4Widgets(FI_ENT02)
      $Red4Widgets(FI_ENT02) delete 0 end
      $Red4Widgets(FI_ENT02) insert end OBJECT
    }

  } elseif {[string trim ${item}] == "red4Polysky"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(PF_INPUT)
      $Red4Widgets(PF_INPUT) delete 0 end
      $Red4Widgets(PF_INPUT) insert end $Red4Widgets(DRG)
      set Red4Widgets(PF_POLYFIT) REDUCED_GRP
      selection clear $Red4Widgets(PF_DEGREE)
      $Red4Widgets(PF_DEGREE) delete 0 end
      $Red4Widgets(PF_DEGREE) insert end 1.0
      selection clear $Red4Widgets(PF_NREJECT)
      $Red4Widgets(PF_NREJECT) delete 0 end
      $Red4Widgets(PF_NREJECT) insert end 0
      set Red4Widgets(PF_WEIGHT) 1
      selection clear $Red4Widgets(PF_SAYS1)
      $Red4Widgets(PF_SAYS1) delete 0 end
      $Red4Widgets(PF_SAYS1) insert end 20
      selection clear $Red4Widgets(PF_SAYE1)
      $Red4Widgets(PF_SAYE1) delete 0 end
      $Red4Widgets(PF_SAYE1) insert end 25
      selection clear $Red4Widgets(PF_SAYS2)
      $Red4Widgets(PF_SAYS2) delete 0 end
      $Red4Widgets(PF_SAYS2) insert end 35
      selection clear $Red4Widgets(PF_SAYE2)
      $Red4Widgets(PF_SAYE2) delete 0 end
      $Red4Widgets(PF_SAYE2) insert end 40
      selection clear $Red4Widgets(PF_SAYS3)
      $Red4Widgets(PF_SAYS3) delete 0 end
      $Red4Widgets(PF_SAYS3) insert end -1
      selection clear $Red4Widgets(PF_SAYE3)
      $Red4Widgets(PF_SAYE3) delete 0 end
      $Red4Widgets(PF_SAYE3) insert end -1
      selection clear $Red4Widgets(PF_SAYS4)
      $Red4Widgets(PF_SAYS4) delete 0 end
      $Red4Widgets(PF_SAYS4) insert end -1
      selection clear $Red4Widgets(PF_SAYE4)
      $Red4Widgets(PF_SAYE4) delete 0 end
      $Red4Widgets(PF_SAYE4) insert end -1
    } elseif {[string trim ${value}] == "PF_INPUT"} {
      selection clear $Red4Widgets(PF_INPUT)
      $Red4Widgets(PF_INPUT) delete 0 end
      $Red4Widgets(PF_INPUT) insert end $Red4Widgets(DRG)
    } elseif {[string trim ${value}] == "PF_POLYFIT"} {
      set Red4widgets(PF_POLYFIT) REDUCED_GRP
    } elseif {[string trim ${value}] == "PF_DEGREE"} {
      selection clear $Red4Widgets(PF_DEGREE)
      $Red4Widgets(PF_DEGREE) delete 0 end
      $Red4Widgets(PF_DEGREE) insert end 1.0
    } elseif {[string trim ${value}] == "PF_NREJECT"} {
      selection clear $Red4Widgets(PF_NREJECT)
      $Red4Widgets(PF_NREJECT) delete 0 end
      $Red4Widgets(PF_NREJECT) insert end 0
    } elseif {[string trim ${value}] == "PF_WEIGHT"} {
      set Red4widgets(PF_WEIGHT) 1
    } elseif {[string trim ${value}] == "PF_SAYS1"} {
      selection clear $Red4Widgets(PF_SAYS1)
      $Red4Widgets(PF_SAYS1) delete 0 end
      $Red4Widgets(PF_SAYS1) insert end 20
    } elseif {[string trim ${value}] == "PF_SAYE1"} {
      selection clear $Red4Widgets(PF_SAYE1)
      $Red4Widgets(PF_SAYE1) delete 0 end
      $Red4Widgets(PF_SAYE1) insert end 25
    } elseif {[string trim ${value}] == "PF_SAYS2"} {
      selection clear $Red4Widgets(PF_SAYS2)
      $Red4Widgets(PF_SAYS2) delete 0 end
      $Red4Widgets(PF_SAYS2) insert end 35
    } elseif {[string trim ${value}] == "PF_SAYE2"} {
      selection clear $Red4Widgets(PF_SAYE2)
      $Red4Widgets(PF_SAYE2) delete 0 end
      $Red4Widgets(PF_SAYE2) insert end 40
    } elseif {[string trim ${value}] == "PF_SAYS3"} {
      selection clear $Red4Widgets(PF_SAYS3)
      $Red4Widgets(PF_SAYS3) delete 0 end
      $Red4Widgets(PF_SAYS3) insert end -1
    } elseif {[string trim ${value}] == "PF_SAYE3"} {
      selection clear $Red4Widgets(PF_SAYE3)
      $Red4Widgets(PF_SAYE3) delete 0 end
      $Red4Widgets(PF_SAYE3) insert end -1
    } elseif {[string trim ${value}] == "PF_SAYS4"} {
      selection clear $Red4Widgets(PF_SAYS4)
      $Red4Widgets(PF_SAYS4) delete 0 end
      $Red4Widgets(PF_SAYS4) insert end -1
    } elseif {[string trim ${value}] == "PF_SAYE4"} {
      selection clear $Red4Widgets(PF_SAYE4)
      $Red4Widgets(PF_SAYE4) delete 0 end
      $Red4Widgets(PF_SAYE4) insert end -1
    }

  } elseif {[string trim ${item}] == "red4Flux"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(FC_ENT01)
      $Red4Widgets(FC_ENT01) delete 0 end
      $Red4Widgets(FC_ENT01) insert end $Red4Widgets(DSP)
      selection clear $Red4Widgets(FC_ENT02)
      $Red4Widgets(FC_ENT02) delete 0 end
      $Red4Widgets(FC_ENT02) insert end 0.0
      set Red4Widgets(FC_IN) 0.0
      set Red4Widgets(FC_FIN) "J"
      set Red4Widgets(FC_FOUT) "W/m2/um"
    } elseif {[string trim ${value}] == "FC_ENT01"} {
      selection clear $Red4Widgets(FC_ENT01)
      $Red4Widgets(FC_ENT01) delete 0 end
      $Red4Widgets(FC_ENT01) insert end $Red4Widgets(DSP)
    } elseif {[string trim ${value}] == "FC_ENT02"} {
      selection clear $Red4Widgets(FC_ENT02)
      $Red4Widgets(FC_ENT02) delete 0 end
      $Red4Widgets(FC_ENT02) insert end 0.0
      set Red4Widgets(FC_IN) 0.0
    } elseif {[string trim ${value}] == "FC_FIN"} {
      set Red4Widgets(FC_FIN) "J"
    } elseif {[string trim ${value}] == "FC_FOUT"} {
      set Red4Widgets(FC_FOUT) "W/m2/um"
    }

  } elseif {[string trim ${item}] == "red4NormObs"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(NO_ENT01)
      $Red4Widgets(NO_ENT01) delete 0 end
      $Red4Widgets(NO_ENT01) insert end $Red4Widgets(DRO)
      selection clear $Red4Widgets(NO_ENT02)
      $Red4Widgets(NO_ENT02) delete 0 end
      $Red4Widgets(NO_ENT02) insert end 3
      selection clear $Red4Widgets(NO_ENT03)
      $Red4Widgets(NO_ENT03) delete 0 end
      $Red4Widgets(NO_ENT03) insert end 5
      set Red4Widgets(NO_METHOD) POLYFIT
    } elseif {[string trim ${value}] == "NO_ENT01"} {
      selection clear $Red4Widgets(NO_ENT01)
      $Red4Widgets(NO_ENT01) delete 0 end
      $Red4Widgets(NO_ENT01) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "NO_ENT02"} {
      selection clear $Red4Widgets(NO_ENT02)
      $Red4Widgets(NO_ENT02) delete 0 end
      $Red4Widgets(NO_ENT02) insert end 3
    } elseif {[string trim ${value}] == "NO_ENT03"} {
      selection clear $Red4Widgets(NO_ENT03)
      $Red4Widgets(NO_ENT03) delete 0 end
      $Red4Widgets(NO_ENT03) insert end 5
    } elseif {[string trim ${value}] == "NO_METHOD"} {
      set Red4Widgets(NO_METHOD) POLYFIT
    }

  } elseif {[string trim ${item}] == "red4ModelBB"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(BB_ENT01)
      $Red4Widgets(BB_ENT01) delete 0 end
      $Red4Widgets(BB_ENT01) insert end $Red4Widgets(DRO)
      selection clear $Red4Widgets(BB_ENT02)
      $Red4Widgets(BB_ENT02) delete 0 end
      $Red4Widgets(BB_ENT02) insert end 5000.0
      selection clear $Red4Widgets(BB_ENT03)
      $Red4Widgets(BB_ENT03) delete 0 end
      $Red4Widgets(BB_ENT03) insert end 2.2
      selection clear $Red4Widgets(BB_ENT04)
      $Red4Widgets(BB_ENT04) delete 0 end
      $Red4Widgets(BB_ENT04) insert end 0.0
    } elseif {[string trim ${value}] == "BB_ENT01"} {
      selection clear $Red4Widgets(BB_ENT01)
      $Red4Widgets(BB_ENT01) delete 0 end
      $Red4Widgets(BB_ENT01) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "BB_ENT02"} {
      selection clear $Red4Widgets(BB_ENT02)
      $Red4Widgets(BB_ENT02) delete 0 end
      $Red4Widgets(BB_ENT02) insert end 5000.0
    } elseif {[string trim ${value}] == "BB_ENT03"} {
      selection clear $Red4Widgets(BB_ENT03)
      $Red4Widgets(BB_ENT03) delete 0 end
      $Red4Widgets(BB_ENT03) insert end 2.2
    } elseif {[string trim ${value}] == "BB_ENT04"} {
      selection clear $Red4Widgets(BB_ENT04)
      $Red4Widgets(BB_ENT04) delete 0 end
      $Red4Widgets(BB_ENT04) insert end 0.0
    }

  } elseif {[string trim ${item}] == "red4Derip"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(DR_ENT01)
      $Red4Widgets(DR_ENT01) delete 0 end
      $Red4Widgets(DR_ENT01) insert end $Red4Widgets(DSP)
      selection clear $Red4Widgets(DR_ENT02)
      $Red4Widgets(DR_ENT02) delete 0 end
      $Red4Widgets(DR_ENT02) insert end 0.5
      selection clear $Red4Widgets(DR_ENT03)
      $Red4Widgets(DR_ENT03) delete 0 end
      $Red4Widgets(DR_ENT03) insert end 255.5
    } elseif {[string trim ${value}] == "DR_ENT01"} {
      selection clear $Red4Widgets(DR_ENT01)
      $Red4Widgets(DR_ENT01) delete 0 end
      $Red4Widgets(DR_ENT01) insert end $Red4Widgets(DSP)
    } elseif {[string trim ${value}] == "DR_ENT02"} {
      selection clear $Red4Widgets(DR_ENT02)
      $Red4Widgets(DR_ENT02) delete 0 end
      $Red4Widgets(DR_ENT02) insert end 0.5
    } elseif {[string trim ${value}] == "DR_ENT03"} {
      selection clear $Red4Widgets(DR_ENT03)
      $Red4Widgets(DR_ENT03) delete 0 end
      $Red4Widgets(DR_ENT03) insert end 255.5
    }

  } elseif {[string trim ${item}] == "red4Stats"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(GS_ENT01)
      $Red4Widgets(GS_ENT01) delete 0 end
      $Red4Widgets(GS_ENT01) insert end $Red4Widgets(DRO)
      set Red4Widgets(GS_PLANE) DATA
      set Red4Widgets(GS_WHOLE) 1
      set Red4Widgets(GS_AUTOSCALE) 1
    } elseif {[string trim ${value}] == "GS_ENT01"} {
      selection clear $Red4Widgets(GS_ENT01)
      $Red4Widgets(GS_ENT01) delete 0 end
      $Red4Widgets(GS_ENT01) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "GS_ENT02"} {
      selection clear $Red4Widgets(GS_ENT02)
      $Red4Widgets(GS_ENT02) delete 0 end
      $Red4Widgets(GS_ENT02) insert end 1
    } elseif {[string trim ${value}] == "GS_ENT03"} {
      selection clear $Red4Widgets(GS_ENT03)
      $Red4Widgets(GS_ENT03) delete 0 end
      $Red4Widgets(GS_ENT03) insert end 256
    } elseif {[string trim ${value}] == "GS_ENT04"} {
      selection clear $Red4Widgets(GS_ENT04)
      $Red4Widgets(GS_ENT04) delete 0 end
      $Red4Widgets(GS_ENT04) insert end 1
    } elseif {[string trim ${value}] == "GS_ENT05"} {
      selection clear $Red4Widgets(GS_ENT05)
      $Red4Widgets(GS_ENT05) delete 0 end
      $Red4Widgets(GS_ENT05) insert end 1
    } elseif {[string trim ${value}] == "GS_ENT06"} {
      selection clear $Red4Widgets(GS_ENT06)
      $Red4Widgets(GS_ENT06) delete 0 end
      $Red4Widgets(GS_ENT06) insert end 256
    } elseif {[string trim ${value}] == "GS_ENT07"} {
      selection clear $Red4Widgets(GS_ENT07)
      $Red4Widgets(GS_ENT07) delete 0 end
      $Red4Widgets(GS_ENT07) insert end 1
    } elseif {[string trim ${value}] == "GS_ENT08"} {
      selection clear $Red4Widgets(GS_ENT08)
      $Red4Widgets(GS_ENT08) delete 0 end
      $Red4Widgets(GS_ENT08) insert end 1000.0
    } elseif {[string trim ${value}] == "GS_ENT09"} {
      selection clear $Red4Widgets(GS_ENT09)
      $Red4Widgets(GS_ENT09) delete 0 end
      $Red4Widgets(GS_ENT09) insert end 0.0
    } elseif {[string trim ${value}] == "GS_PLANE"} {
      set Red4Widgets(GS_PLANE) DATA
    } elseif {[string trim ${value}] == "GS_WHOLE"} {
      set Red4Widgets(GS_WHOLE) 1
    } elseif {[string trim ${value}] == "GS_AUTOSCALE"} {
      set Red4Widgets(GS_AUTOSCALE) 1
    }

  } elseif {[string trim ${item}] == "red4Remove"} {
    selection clear $Red4Widgets(RM_ENTRY)
    $Red4Widgets(RM_ENTRY) delete 0 end
    if {[string trim ${value}] == "grp"} {
      $Red4Widgets(RM_ENTRY) insert end $Red4Widgets(DRG)
    } elseif {[string trim ${value}] == "obs"} {
      $Red4Widgets(RM_ENTRY) insert end $Red4Widgets(DRO)
    }

  } elseif {[string trim ${item}] == "red4Dbs"} {
    selection clear $Red4Widgets(DBS_ENTRY)
    $Red4Widgets(DBS_ENTRY) delete 0 end
    $Red4Widgets(DBS_ENTRY) insert end $Red4Widgets(DRG)

  } elseif {[string trim ${item}] == "red4Efficiency"} {
    selection clear $Red4Widgets(EF_ENTRY)
    $Red4Widgets(EF_ENTRY) delete 0 end
    $Red4Widgets(EF_ENTRY) insert end $Red4Widgets(DRO)

  } elseif {[string trim ${item}] == "red4Cgs4List"} {
    selection clear $Red4Widgets(LH_ENTRY)
    $Red4Widgets(LH_ENTRY) delete 0 end
    $Red4Widgets(LH_ENTRY) insert end $env(ODIR)

  } elseif {[string trim ${item}] == "red4FileObs"} {
    selection clear $Red4Widgets(FO_ENTRY)
    $Red4Widgets(FO_ENTRY) delete 0 end
    $Red4Widgets(FO_ENTRY) insert end $Red4Widgets(DOB)

  } elseif {[string trim ${item}] == "red4Package"} {
    selection clear $Red4Widgets(PK_ENTRY)
    $Red4Widgets(PK_ENTRY) delete 0 end
    $Red4Widgets(PK_ENTRY) insert end $Red4Widgets(DRO)

  } elseif {[string trim ${item}] == "red4LogComment"} {
    selection clear $Red4Widgets(LC_ENTRY)
    $Red4Widgets(LC_ENTRY) delete 0 end

  } elseif {[string trim ${item}] == "red4Calibrate"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(WC_ENT01)
      $Red4Widgets(WC_ENT01) delete 0 end
      $Red4Widgets(WC_ENT01) insert end $Red4Widgets(DRO)
      selection clear $Red4Widgets(WC_ENT02)
      $Red4Widgets(WC_ENT02) delete 0 end
      $Red4Widgets(WC_ENT02) insert end 1
      selection clear $Red4Widgets(WC_ENT03)
      $Red4Widgets(WC_ENT03) delete 0 end
      $Red4Widgets(WC_ENT03) insert end 256
      selection clear $Red4Widgets(WC_ENT04)
      $Red4Widgets(WC_ENT04) delete 0 end
      $Red4Widgets(WC_ENT04) insert end 2
      selection clear $Red4Widgets(WC_ENT05)
      $Red4Widgets(WC_ENT05) delete 0 end
      $Red4Widgets(WC_ENT05) insert end 1
    } elseif {[string trim ${value}] == "WC_ENT01"} {
      selection clear $Red4Widgets(WC_ENT01)
      $Red4Widgets(WC_ENT01) delete 0 end
      $Red4Widgets(WC_ENT01) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "WC_ENT02"} {
      selection clear $Red4Widgets(WC_ENT02)
      $Red4Widgets(WC_ENT02) delete 0 end
      $Red4Widgets(WC_ENT02) insert end 1
    } elseif {[string trim ${value}] == "WC_ENT03"} {
      selection clear $Red4Widgets(WC_ENT03)
      $Red4Widgets(WC_ENT03) delete 0 end
      $Red4Widgets(WC_ENT03) insert end 256
    } elseif {[string trim ${value}] == "WC_ENT04"} {
      selection clear $Red4Widgets(WC_ENT04)
      $Red4Widgets(WC_ENT04) delete 0 end
      $Red4Widgets(WC_ENT04) insert end 2
    } elseif {[string trim ${value}] == "WC_ENT05"} {
      selection clear $Red4Widgets(WC_ENT05)
      $Red4Widgets(WC_ENT05) delete 0 end
      $Red4Widgets(WC_ENT05) insert end 1
    }

  } elseif {[string trim ${item}] == "red4Ext"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(SPC_ENT01)
      $Red4Widgets(SPC_ENT01) delete 0 end
      $Red4Widgets(SPC_ENT01) insert end $Red4Widgets(DRG)
      selection clear $Red4Widgets(SPC_ENT02)
      $Red4Widgets(SPC_ENT02) delete 0 end
      $Red4Widgets(SPC_ENT02) insert end 29
      selection clear $Red4Widgets(SPC_ENT03)
      $Red4Widgets(SPC_ENT03) delete 0 end
      $Red4Widgets(SPC_ENT03) insert end 29
      selection clear $Red4Widgets(SPC_ENT04)
      $Red4Widgets(SPC_ENT04) delete 0 end
      $Red4Widgets(SPC_ENT04) insert end -1
      selection clear $Red4Widgets(SPC_ENT05)
      $Red4Widgets(SPC_ENT05) delete 0 end
      $Red4Widgets(SPC_ENT05) insert end -1
      selection clear $Red4Widgets(SPC_ENT06)
      $Red4Widgets(SPC_ENT06) delete 0 end
      $Red4Widgets(SPC_ENT06) insert end -1
      selection clear $Red4Widgets(SPC_ENT07)
      $Red4Widgets(SPC_ENT07) delete 0 end
      $Red4Widgets(SPC_ENT07) insert end -1
      set Red4Widgets(SPC_ALGORITHM) BRIGHT
      set Red4Widgets(SPC_INVERT) 0
    } elseif {[string trim ${value}] == "SPC_ENT01"} {
      selection clear $Red4Widgets(SPC_ENT01)
      $Red4Widgets(SPC_ENT01) delete 0 end
      $Red4Widgets(SPC_ENT01) insert end $Red4Widgets(DRG)
    } elseif {[string trim ${value}] == "SPC_ENT02"} {
      selection clear $Red4Widgets(SPC_ENT02)
      $Red4Widgets(SPC_ENT02) delete 0 end
      $Red4Widgets(SPC_ENT02) insert end 29
    } elseif {[string trim ${value}] == "SPC_ENT03"} {
      selection clear $Red4Widgets(SPC_ENT03)
      $Red4Widgets(SPC_ENT03) delete 0 end
      $Red4Widgets(SPC_ENT03) insert end 29
    } elseif {[string trim ${value}] == "SPC_ENT04"} {
      selection clear $Red4Widgets(SPC_ENT04)
      $Red4Widgets(SPC_ENT04) delete 0 end
      $Red4Widgets(SPC_ENT04) insert end -1
    } elseif {[string trim ${value}] == "SPC_ENT05"} {
      selection clear $Red4Widgets(SPC_ENT05)
      $Red4Widgets(SPC_ENT05) delete 0 end
      $Red4Widgets(SPC_ENT05) insert end -1
    } elseif {[string trim ${value}] == "SPC_ENT06"} {
      selection clear $Red4Widgets(SPC_ENT06)
      $Red4Widgets(SPC_ENT06) delete 0 end
      $Red4Widgets(SPC_ENT06) insert end -1
    } elseif {[string trim ${value}] == "SPC_ENT07"} {
      selection clear $Red4Widgets(SPC_ENT07)
      $Red4Widgets(SPC_ENT07) delete 0 end
      $Red4Widgets(SPC_ENT07) insert end -1
    } elseif {[string trim ${value}] == "SPC_ALGORITHM"} {
      set Red4Widgets(SPC_ALGORITHM) BRIGHT
    } elseif {[string trim ${value}] == "SPC_INVERT"} {
      set Red4Widgets(SPC_INVERT) 0
    }

  } elseif {[string trim ${item}] == "red4ExtMask"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(ME_ENT01)
      $Red4Widgets(ME_ENT01) delete 0 end
      $Red4Widgets(ME_ENT01) insert end Name_of_new_bad_pixel_mask
      selection clear $Red4Widgets(ME_ENT02)
      $Red4Widgets(ME_ENT02) delete 0 end
      $Red4Widgets(ME_ENT02) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "ME_ENT01"} {
      selection clear $Red4Widgets(ME_ENT01)
      $Red4Widgets(ME_ENT01) delete 0 end
      $Red4Widgets(ME_ENT01) insert end Name_of_new_bad_pixel_mask
    } elseif {[string trim ${value}] == "ME_ENT02"} {
      selection clear $Red4Widgets(ME_ENT02)
      $Red4Widgets(ME_ENT02) delete 0 end
      $Red4Widgets(ME_ENT02) insert end $Red4Widgets(DRO)
    }

  } elseif {[string trim ${item}] == "red4EditMask"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(EM_ENT01)
      $Red4Widgets(EM_ENT01) delete 0 end
      $Red4Widgets(EM_ENT01) insert end #
      set Red4Widgets(EM_RDISP) 0
    } elseif {[string trim ${value}] == "EM_ENT01"} {
      selection clear $Red4Widgets(EM_ENT01)
      $Red4Widgets(EM_ENT01) delete 0 end
      $Red4Widgets(EM_ENT01) insert end #
    } elseif {[string trim ${value}] == "EM_RDISP"} {
      set Red4Widgets(EM_RDISP) 0
    }

  } elseif {[string trim ${item}] == "red4CreWinMask"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(WM_ENT01)
      $Red4Widgets(WM_ENT01) delete 0 end
      $Red4Widgets(WM_ENT01) insert end $Red4Widgets(DRO)
      selection clear $Red4Widgets(WM_ENT02)
      $Red4Widgets(WM_ENT02) delete 0 end
      $Red4Widgets(WM_ENT02) insert end 1
      selection clear $Red4Widgets(WM_ENT03)
      $Red4Widgets(WM_ENT03) delete 0 end
      $Red4Widgets(WM_ENT03) insert end 1
      selection clear $Red4Widgets(WM_ENT04)
      $Red4Widgets(WM_ENT04) delete 0 end
      $Red4Widgets(WM_ENT04) insert end 256
      selection clear $Red4Widgets(WM_ENT05)
      $Red4Widgets(WM_ENT05) delete 0 end
      $Red4Widgets(WM_ENT05) insert end 1
      selection clear $Red4Widgets(WM_ENT06)
      $Red4Widgets(WM_ENT06) delete 0 end
      $Red4Widgets(WM_ENT06) insert end 1
      selection clear $Red4Widgets(WM_ENT07)
      $Red4Widgets(WM_ENT07) delete 0 end
      $Red4Widgets(WM_ENT07) insert end 256
    } elseif {[string trim ${value}] == "WM_ENT01"} {
      selection clear $Red4Widgets(WM_ENT01)
      $Red4Widgets(WM_ENT01) delete 0 end
      $Red4Widgets(WM_ENT01) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "WM_ENT02"} {
      selection clear $Red4Widgets(WM_ENT02)
      $Red4Widgets(WM_ENT02) delete 0 end
      $Red4Widgets(WM_ENT02) insert end 1
    } elseif {[string trim ${value}] == "WM_ENT03"} {
      selection clear $Red4Widgets(WM_ENT03)
      $Red4Widgets(WM_ENT03) delete 0 end
      $Red4Widgets(WM_ENT03) insert end 1
    } elseif {[string trim ${value}] == "WM_ENT04"} {
      selection clear $Red4Widgets(WM_ENT04)
      $Red4Widgets(WM_ENT04) delete 0 end
      $Red4Widgets(WM_ENT04) insert end 256
    } elseif {[string trim ${value}] == "WM_ENT05"} {
      selection clear $Red4Widgets(WM_ENT05)
      $Red4Widgets(WM_ENT05) delete 0 end
      $Red4Widgets(WM_ENT05) insert end 1
    } elseif {[string trim ${value}] == "WM_ENT06"} {
      selection clear $Red4Widgets(WM_ENT06)
      $Red4Widgets(WM_ENT06) delete 0 end
      $Red4Widgets(WM_ENT06) insert end 1
    } elseif {[string trim ${value}] == "WM_ENT07"} {
      selection clear $Red4Widgets(WM_ENT07)
      $Red4Widgets(WM_ENT07) delete 0 end
      $Red4Widgets(WM_ENT07) insert end 256
    }

  } elseif {[string trim ${item}] == "red4CreThreshMask"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(TM_ENT01)
      $Red4Widgets(TM_ENT01) delete 0 end
      $Red4Widgets(TM_ENT01) insert end $Red4Widgets(DRO)
      selection clear $Red4Widgets(TM_ENT02)
      $Red4Widgets(TM_ENT02) delete 0 end
      $Red4Widgets(TM_ENT02) insert end "Name_of_new_bad_pixel_mask"
      set Red4Widgets(TM_RDISP) 0
    } elseif {[string trim ${value}] == "TM_ENT01"} {
      selection clear $Red4Widgets(TM_ENT01)
      $Red4Widgets(TM_ENT01) delete 0 end
      $Red4Widgets(TM_ENT01) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "TM_ENT02"} {
      selection clear $Red4Widgets(TM_ENT02)
      $Red4Widgets(TM_ENT02) delete 0 end
      $Red4Widgets(TM_ENT02) insert end "Name_of_new_bad_pixel_mask"
    } elseif {[string trim ${value}] == "TM_RDISP"} {
      set Red4Widgets(TM_RDISP) 0
    }

  } elseif {[string trim ${item}] == "red4CleanObs"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(CO_ENT01)
      $Red4Widgets(CO_ENT01) delete 0 end
      $Red4Widgets(CO_ENT01) insert end $Red4Widgets(DRO)
      selection clear $Red4Widgets(CO_ENT02)
      $Red4Widgets(CO_ENT02) delete 0 end
      $Red4Widgets(CO_ENT02) insert end 100.0
      selection clear $Red4Widgets(CO_ENT03)
      $Red4Widgets(CO_ENT03) delete 0 end
      $Red4Widgets(CO_ENT03) insert end 100.0
    } elseif {[string trim ${value}] == "CO_ENT01"} {
      selection clear $Red4Widgets(CO_ENT01)
      $Red4Widgets(CO_ENT01) delete 0 end
      $Red4Widgets(CO_ENT01) insert end $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "CO_ENT02"} {
      selection clear $Red4Widgets(CO_ENT02)
      $Red4Widgets(CO_ENT02) delete 0 end
      $Red4Widgets(CO_ENT02) insert end 100.0
    } elseif {[string trim ${value}] == "CO_ENT03"} {
      selection clear $Red4Widgets(CO_ENT03)
      $Red4Widgets(CO_ENT03) delete 0 end
      $Red4Widgets(CO_ENT03) insert end 100.0
    }

  } elseif {[string trim ${item}] == "red4SubPair1"} {
      selection clear $Red4Widgets(SP_ENT01)
      $Red4Widgets(SP_ENT01) delete 0 end
      $Red4Widgets(SP_ENT01) insert end $Red4Widgets(DOB)

  } elseif {[string trim ${item}] == "red4SubPair2"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(SP_ENT01)
      $Red4Widgets(SP_ENT01) delete 0 end
      $Red4Widgets(SP_ENT01) insert end $Red4Widgets(DOB)
      selection clear $Red4Widgets(SP_ENT02)
      $Red4Widgets(SP_ENT02) delete 0 end
      $Red4Widgets(SP_ENT02) insert end $Red4Widgets(DOB)
    } elseif {[string trim ${value}] == "SP_ENT01"} {
      selection clear $Red4Widgets(SP_ENT01)
      $Red4Widgets(SP_ENT01) delete 0 end
      $Red4Widgets(SP_ENT01) insert end $Red4Widgets(DOB)
    } elseif {[string trim ${value}] == "SP_ENT02"} {
      selection clear $Red4Widgets(SP_ENT02)
      $Red4Widgets(SP_ENT02) delete 0 end
      $Red4Widgets(SP_ENT02) insert end $Red4Widgets(DOB)
    }

  } elseif {[string trim ${item}] == "red4AddPair"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(AP_ENTRY)
      $Red4Widgets(AP_ENTRY) delete 0 end
      $Red4Widgets(AP_ENTRY) insert end $Red4Widgets(DOB)
      selection clear $Red4Widgets(AP_SKYWT)
      $Red4Widgets(AP_SKYWT) delete 0 end
      $Red4Widgets(AP_SKYWT) insert end 1.0
      set Red4Widgets(AP_VARWT) 0
    } elseif {[string trim ${value}] == "AP_VARWT"} {
      set Red4Widgets(AP_VARWT) 0
    } elseif {[string trim ${value}] == "AP_ENTRY"} {
      selection clear $Red4Widgets(AP_ENTRY)
      $Red4Widgets(AP_ENTRY) delete 0 end
      $Red4Widgets(AP_ENTRY) insert end $Red4Widgets(DOB)
    } elseif {[string trim ${value}] == "AP_SKYWT"} {
      selection clear $Red4Widgets(AP_SKYWT)
      $Red4Widgets(AP_SKYWT) delete 0 end
      $Red4Widgets(AP_SKYWT) insert end 1.0
    }

  } elseif {[string trim ${item}] == "red4Iarith"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(AR_ENT01)
      $Red4Widgets(AR_ENT01) delete 0 end
      $Red4Widgets(AR_ENT01) insert 0 "\$RODIR/ro$env(CGS4_DATE)_"
      selection clear $Red4Widgets(AR_ENT02)
      $Red4Widgets(AR_ENT02) delete 0 end
      $Red4Widgets(AR_ENT02) insert 0 "\$RODIR/ro$env(CGS4_DATE)_"
      selection clear $Red4Widgets(AR_ENT03)
      $Red4Widgets(AR_ENT03) delete 0 end
      $Red4Widgets(AR_ENT03) insert 0 "\$RODIR/ro$env(CGS4_DATE)_"
    } elseif {[string trim ${value}] == "AR_ENT01"} {
      selection clear $Red4Widgets(AR_ENT01)
      $Red4Widgets(AR_ENT01) delete 0 end
      $Red4Widgets(AR_ENT01) insert 0 "\$RODIR/ro$env(CGS4_DATE)_"
    } elseif {[string trim ${value}] == "AR_ENT02"} {
      selection clear $Red4Widgets(AR_ENT02)
      $Red4Widgets(AR_ENT02) delete 0 end
      $Red4Widgets(AR_ENT02) insert 0 "\$RODIR/ro$env(CGS4_DATE)_"
    } elseif {[string trim ${value}] == "AR_ENT03"} {
      selection clear $Red4Widgets(AR_ENT03)
      $Red4Widgets(AR_ENT03) delete 0 end
      $Red4Widgets(AR_ENT03) insert 0 "\$RODIR/ro$env(CGS4_DATE)_"
    }

  } elseif {[string trim ${item}] == "red4Iarith2"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(AR_ENT01)
      $Red4Widgets(AR_ENT01) delete 0 end
      $Red4Widgets(AR_ENT01) insert 0 "\$RODIR/ro$env(CGS4_DATE)_"
      selection clear $Red4Widgets(AR_ENT02)
      $Red4Widgets(AR_ENT02) delete 0 end
      $Red4Widgets(AR_ENT02) insert 0 "\$RODIR/ro$env(CGS4_DATE)_"
    } elseif {[string trim ${value}] == "AR_ENT01"} {
      selection clear $Red4Widgets(AR_ENT01)
      $Red4Widgets(AR_ENT01) delete 0 end
      $Red4Widgets(AR_ENT01) insert 0 "\$RODIR/ro$env(CGS4_DATE)_"
    } elseif {[string trim ${value}] == "AR_ENT02"} {
      selection clear $Red4Widgets(AR_ENT02)
      $Red4Widgets(AR_ENT02) delete 0 end
      $Red4Widgets(AR_ENT02) insert 0 "\$RODIR/ro$env(CGS4_DATE)_"
    }

  } elseif {[string trim ${item}] == "red4AppMask"} {
    if {[string trim ${value}] == "ALL"} {
      selection clear $Red4Widgets(AM_ENT01)
      $Red4Widgets(AM_ENT01) delete 0 end
      $Red4Widgets(AM_ENT01) insert 0 #
      selection clear $Red4Widgets(AM_ENT02)
      $Red4Widgets(AM_ENT02) delete 0 end
      $Red4Widgets(AM_ENT02) insert 0 $Red4Widgets(DRO)
    } elseif {[string trim ${value}] == "AM_ENT01"} {
      selection clear $Red4Widgets(AM_ENT01)
      $Red4Widgets(AM_ENT01) delete 0 end
      $Red4Widgets(AM_ENT01) insert 0 #
    } elseif {[string trim ${value}] == "AM_ENT02"} {
      selection clear $Red4Widgets(AM_ENT02)
      $Red4Widgets(AM_ENT02) delete 0 end
      $Red4Widgets(AM_ENT02) insert 0 $Red4Widgets(DRO)
    }
  }

  update idletasks
}
