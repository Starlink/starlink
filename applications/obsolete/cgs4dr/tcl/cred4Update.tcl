proc cred4Update {item value} {
  global env
  global Cred4Widgets
  if {[string trim ${item}] == "cred4Setup"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      set Cred4Widgets(ADD_INT)        "YES"
      set Cred4Widgets(SUBTRACT_BIAS)  "YES"
      set Cred4Widgets(SUBTRACT_DARK)  "NO"
      set Cred4Widgets(NORMALISE_FF)   "YES"
      set Cred4Widgets(DIVIDE_BY_FF)   "YES"
      set Cred4Widgets(ADD_OBS)        "YES"
      set Cred4Widgets(ARCHIVE_OBS)    "NO"
      set Cred4Widgets(FILE_OBS)       "YES"
      set Cred4Widgets(WAVELENGTH_CAL) "YES"
      set Cred4Widgets(DIVIDE_BY_STD)  "NO"
      set Cred4Widgets(EXTRACT_SPC)    "NO"
      set Cred4Widgets(AUTOFIT)        "NO"
    } elseif {[string toupper [string trim ${value}]] == "ADD_INT"} {
      set Cred4Widgets(ADD_INT)        "YES"
    } elseif {[string toupper [string trim ${value}]] == "SUBTRACT_BIAS"} {
      set Cred4Widgets(SUBTRACT_BIAS)  "YES"
    } elseif {[string toupper [string trim ${value}]] == "SUBTRACT_DARK"} {
      set Cred4Widgets(SUBTRACT_DARK)  "NO"
    } elseif {[string toupper [string trim ${value}]] == "NORMALISE_FF"} {
      set Cred4Widgets(NORMALISE_FF)   "YES"
    } elseif {[string toupper [string trim ${value}]] == "DIVIDE_BY_FF"} {
      set Cred4Widgets(DIVIDE_BY_FF)   "YES"
    } elseif {[string toupper [string trim ${value}]] == "ADD_OBS"} {
      set Cred4Widgets(ADD_OBS)        "YES"
    } elseif {[string toupper [string trim ${value}]] == "ARCHIVE_OBS"} {
      set Cred4Widgets(ARCHIVE_OBS)    "NO"
    } elseif {[string toupper [string trim ${value}]] == "FILE_OBS"} {
      set Cred4Widgets(FILE_OBS)       "YES"
    } elseif {[string toupper [string trim ${value}]] == "WAVELENGTH_CAL"} {
      set Cred4Widgets(WAVELENGTH_CAL) "YES"
    } elseif {[string toupper [string trim ${value}]] == "DIVIDE_BY_STD"} {
      set Cred4Widgets(DIVIDE_BY_STD)  "NO"
    } elseif {[string toupper [string trim ${value}]] == "EXTRACT_SPC"} {
      set Cred4Widgets(EXTRACT_SPC)    "NO"
    } elseif {[string toupper [string trim ${value}]] == "AUTOFIT"} {
      set Cred4Widgets(AUTOFIT)        "NO"
    }

  } elseif {[string trim ${item}] == "cred4Display"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      if {$Cred4Widgets(DTYPE) == "INT"} {
        set ifr 0
        while {$ifr <= 8} {
          set Cred4Widgets(IP$ifr) 0
          $Cred4Widgets(DIS_IP$ifr) delete 0 end
          incr ifr
        }
      } elseif {$Cred4Widgets(DTYPE) == "OBS"} {
        set ofr 0
        while {$ofr <= 8} {
          set Cred4Widgets(OP$ofr) 0
          $Cred4Widgets(DIS_OP$ofr) delete 0 end
          incr ofr
        }
      } elseif {$Cred4Widgets(DTYPE) == "GRP"} {
        set gfr 0
        while {$gfr <= 8} {
          set Cred4Widgets(GP$gfr) 0
          $Cred4Widgets(DIS_GP$gfr) delete 0 end
          incr gfr
        }
      } elseif {$Cred4Widgets(DTYPE) == "SPC"} {
        set sfr 0
        while {$sfr <= 8} {
          set Cred4Widgets(SP$sfr) 0
          $Cred4Widgets(DIS_SP$sfr) delete 0 end
          incr sfr
        }
      }
    } elseif {[string toupper [string trim ${value}]] == "NONE"} {
      set Cred4Widgets(DTYPE) "NONE"
    } else {
      set new_item [string toupper [string trim ${value}]]
      $Cred4Widgets($new_item) delete 0 end
      $Cred4Widgets($new_item) insert 0 "OVERGRAPH CUT=X SPOS= EPOS= COLOUR="
    }

  } elseif {[string trim ${item}] == "cred4Polysky"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      set Cred4Widgets(PF_POLYFIT) NONE
      set Cred4Widgets(PF_WEIGHT) 1
      $Cred4Widgets(PF_DEGREE) delete 0 end
      $Cred4Widgets(PF_DEGREE) insert 0 1
      $Cred4Widgets(PF_NREJECT) delete 0 end
      $Cred4Widgets(PF_NREJECT) insert 0 0
      $Cred4Widgets(PF_SAYS1) delete 0 end
      $Cred4Widgets(PF_SAYS1) insert 0 15
      $Cred4Widgets(PF_SAYE1) delete 0 end
      $Cred4Widgets(PF_SAYE1) insert 0 20
      $Cred4Widgets(PF_SAYS2) delete 0 end
      $Cred4Widgets(PF_SAYS2) insert 0 25
      $Cred4Widgets(PF_SAYE2) delete 0 end
      $Cred4Widgets(PF_SAYE2) insert 0 30
      $Cred4Widgets(PF_SAYS3) delete 0 end
      $Cred4Widgets(PF_SAYS3) insert 0 -1
      $Cred4Widgets(PF_SAYE3) delete 0 end
      $Cred4Widgets(PF_SAYE3) insert 0 -1
      $Cred4Widgets(PF_SAYS4) delete 0 end
      $Cred4Widgets(PF_SAYS4) insert 0 -1
      $Cred4Widgets(PF_SAYE4) delete 0 end
      $Cred4Widgets(PF_SAYE4) insert 0 -1
    } elseif {[string toupper [string trim ${value}]] == "PF_POLYFIT"} {
      set Cred4Widgets(PF_POLYFIT) NONE
    } elseif {[string toupper [string trim ${value}]] == "PF_DEGREE"} {
      $Cred4Widgets(PF_DEGREE) delete 0 end
      $Cred4Widgets(PF_DEGREE) insert 0 1
    } elseif {[string toupper [string trim ${value}]] == "PF_NREJECT"} {
      $Cred4Widgets(PF_NREJECT) delete 0 end
      $Cred4Widgets(PF_NREJECT) insert 0 0
    } elseif {[string toupper [string trim ${value}]] == "PF_WEIGHT"} {
      set Cred4Widgets(PF_WEIGHT) 1
    } elseif {[string toupper [string trim ${value}]] == "PF_SAYS1"} {
      $Cred4Widgets(PF_SAYS1) delete 0 end
      $Cred4Widgets(PF_SAYS1) insert 0 15
    } elseif {[string toupper [string trim ${value}]] == "PF_SAYE1"} {
      $Cred4Widgets(PF_SAYE1) delete 0 end
      $Cred4Widgets(PF_SAYE1) insert 0 20
    } elseif {[string toupper [string trim ${value}]] == "PF_SAYS2"} {
      $Cred4Widgets(PF_SAYS2) delete 0 end
      $Cred4Widgets(PF_SAYS2) insert 0 25
    } elseif {[string toupper [string trim ${value}]] == "PF_SAYE2"} {
      $Cred4Widgets(PF_SAYE2) delete 0 end
      $Cred4Widgets(PF_SAYE2) insert 0 30
    } elseif {[string toupper [string trim ${value}]] == "PF_SAYS3"} {
      $Cred4Widgets(PF_SAYS3) delete 0 end
      $Cred4Widgets(PF_SAYS3) insert 0 -1
    } elseif {[string toupper [string trim ${value}]] == "PF_SAYE3"} {
      $Cred4Widgets(PF_SAYE3) delete 0 end
      $Cred4Widgets(PF_SAYE3) insert 0 -1
    } elseif {[string toupper [string trim ${value}]] == "PF_SAYS4"} {
      $Cred4Widgets(PF_SAYS4) delete 0 end
      $Cred4Widgets(PF_SAYS4) insert 0 -1
    } elseif {[string toupper [string trim ${value}]] == "PF_SAYE4"} {
      $Cred4Widgets(PF_SAYE4) delete 0 end
      $Cred4Widgets(PF_SAYE4) insert 0 -1
    }

  } elseif {[string trim ${item}] == "cred4Extract"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      set Cred4Widgets(SPC_ALGORITHM) BRIGHT
      set Cred4Widgets(SPC_INVERT) 0
      $Cred4Widgets(SPC_ROW1S) delete 0 end
      $Cred4Widgets(SPC_ROW1S) insert end 29
      $Cred4Widgets(SPC_ROW1E) delete 0 end
      $Cred4Widgets(SPC_ROW1E) insert end 29
      $Cred4Widgets(SPC_ROW2S) delete 0 end
      $Cred4Widgets(SPC_ROW2S) insert end -1
      $Cred4Widgets(SPC_ROW2E) delete 0 end
      $Cred4Widgets(SPC_ROW2E) insert end -1
      $Cred4Widgets(SPC_ROW3S) delete 0 end
      $Cred4Widgets(SPC_ROW3S) insert end -1
      $Cred4Widgets(SPC_ROW3E) delete 0 end
      $Cred4Widgets(SPC_ROW3E) insert end -1
    } elseif {[string toupper [string trim ${value}]] == "SPC_ROW1S"} {
      $Cred4Widgets(SPC_ROW1S) delete 0 end
      $Cred4Widgets(SPC_ROW1S) insert end 29
    } elseif {[string toupper [string trim ${value}]] == "SPC_ROW1E"} {
      $Cred4Widgets(SPC_ROW1E) delete 0 end
      $Cred4Widgets(SPC_ROW1E) insert end 29
    } elseif {[string toupper [string trim ${value}]] == "SPC_ROW2S"} {
      $Cred4Widgets(SPC_ROW2S) delete 0 end
      $Cred4Widgets(SPC_ROW2S) insert end -1
    } elseif {[string toupper [string trim ${value}]] == "SPC_ROW2E"} {
      $Cred4Widgets(SPC_ROW2E) delete 0 end
      $Cred4Widgets(SPC_ROW2E) insert end -1
    } elseif {[string toupper [string trim ${value}]] == "SPC_ROW3S"} {
      $Cred4Widgets(SPC_ROW3S) delete 0 end
      $Cred4Widgets(SPC_ROW3S) insert end -1
    } elseif {[string toupper [string trim ${value}]] == "SPC_ROW3E"} {
      $Cred4Widgets(SPC_ROW3E) delete 0 end
      $Cred4Widgets(SPC_ROW3E) insert end -1
    }

  } elseif {[string trim ${item}] == "cred4Sky"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      set Cred4Widgets(ADD_IN_PAIRS) 1
      set Cred4Widgets(VARWT) 0
      set Cred4Widgets(ERRORS) FROM_OBS
      $Cred4Widgets(SKYWT) delete 0 end
      $Cred4Widgets(SKYWT) insert 0 1.0
    } elseif {[string toupper [string trim ${value}]] == "ADD_IN_PAIRS"} {
      set Cred4Widgets(ADD_IN_PAIRS) 1
    } elseif {[string toupper [string trim ${value}]] == "VARWT"} {
      set Cred4Widgets(VARWT) 0
    } elseif {[string toupper [string trim ${value}]] == "ERRORS"} {
      set Cred4Widgets(ERRORS) FROM_OBS
    } elseif {[string toupper [string trim ${value}]] == "SKYWT"} {
      $Cred4Widgets(SKYWT) delete 0 end
      $Cred4Widgets(SKYWT) insert 0 1.0
    }

  } elseif {[string trim ${item}] == "cred4Bias"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      set Cred4Widgets(SUBTRACT_BIAS) YES
      set Cred4Widgets(BIAS_MODE) BOTH
      $Cred4Widgets(SPEC_BIAS) delete 0 end
    } elseif {[string toupper [string trim ${value}]] == "SUBTRACT_BIAS"} {
      set Cred4Widgets(SUBTRACT_BIAS) YES
    } elseif {[string toupper [string trim ${value}]] == "BIAS_MODE"} {
      set Cred4Widgets(BIAS_MODE) BOTH
      $Cred4Widgets(SPEC_BIAS) delete 0 end
    } elseif {[string toupper [string trim ${value}]] == "SPEC_BIAS"} {
      set Cred4Widgets(BIAS_MODE) SPECIFIED
      $Cred4Widgets(SPEC_BIAS) delete 0 end
      $Cred4Widgets(SPEC_BIAS) insert 0 ro$env(CGS4_DATE)_oooo
    }

  } elseif {[string trim ${item}] == "cred4Dark"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      set Cred4Widgets(SUBTRACT_DARK) YES
      set Cred4Widgets(DARK_MODE) BOTH
      $Cred4Widgets(SPEC_DARK) delete 0 end
    } elseif {[string toupper [string trim ${value}]] == "SUBTRACT_DARK"} {
      set Cred4Widgets(SUBTRACT_DARK) YES
    } elseif {[string toupper [string trim ${value}]] == "DARK_MODE"} {
      set Cred4Widgets(DARK_MODE) BOTH
      $Cred4Widgets(SPEC_DARK) delete 0 end
    } elseif {[string toupper [string trim ${value}]] == "SPEC_DARK"} {
      set Cred4Widgets(DARK_MODE) SPECIFIED
      $Cred4Widgets(SPEC_DARK) delete 0 end
      $Cred4Widgets(SPEC_DARK) insert 0 ro$env(CGS4_DATE)_oooo
    }

  } elseif {[string trim ${item}] == "cred4Flat"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      set Cred4Widgets(DIVIDE_BY_FF) YES
      set Cred4Widgets(FLAT_MODE) BOTH
      set Cred4Widgets(FF_METHOD) POLYFIT
      set Cred4Widgets(NORMALISE_FF) YES
      $Cred4Widgets(SPEC_FLAT) delete 0 end
      $Cred4Widgets(ORDER) set 3
      $Cred4Widgets(BOXSIZE) set 5
    } elseif {[string toupper [string trim ${value}]] == "ORDER"} {
      $Cred4Widgets(ORDER) set 3
    } elseif {[string toupper [string trim ${value}]] == "BOXSIZE"} {
      $Cred4Widgets(BOXSIZE) set 5
    } elseif {[string toupper [string trim ${value}]] == "NORMALISE_FF"} {
      set Cred4Widgets(NORMALISE_FF) YES
    } elseif {[string toupper [string trim ${value}]] == "FF_METHOD"} {
      set Cred4Widgets(FF_METHOD) POLYFIT
    } elseif {[string toupper [string trim ${value}]] == "DIVIDE_BY_FF"} {
      set Cred4Widgets(DIVIDE_BY_FF) YES
    } elseif {[string toupper [string trim ${value}]] == "FLAT_MODE"} {
      set Cred4Widgets(FLAT_MODE) BOTH
      $Cred4Widgets(SPEC_FLAT) delete 0 end
    } elseif {[string toupper [string trim ${value}]] == "SPEC_FLAT"} {
      set Cred4Widgets(FLAT_MODE) SPECIFIED
      $Cred4Widgets(SPEC_FLAT) delete 0 end
      $Cred4Widgets(SPEC_FLAT) insert 0 ro$env(CGS4_DATE)_oooo
    }

  } elseif {[string trim ${item}] == "cred4Calib"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      set Cred4Widgets(TO_WAVELENGTH) YES
      set Cred4Widgets(CALIB_MODE) BOTH
      set Cred4Widgets(LAMBDA_METHOD) ESTIMATED
      $Cred4Widgets(SPEC_CALIB) delete 0 end
    } elseif {[string toupper [string trim ${value}]] == "LAMBDA_METHOD"} {
      set Cred4Widgets(LAMBDA_METHOD) ESTIMATED
    } elseif {[string toupper [string trim ${value}]] == "TO_WAVELENGTH"} {
      set Cred4Widgets(TO_WAVELENGTH) YES
    } elseif {[string toupper [string trim ${value}]] == "CALIB_MODE"} {
      set Cred4Widgets(CALIB_MODE) BOTH
      $Cred4Widgets(SPEC_CALIB) delete 0 end
    } elseif {[string toupper [string trim ${value}]] == "SPEC_CALIB"} {
      set Cred4Widgets(CALIB_MODE) SPECIFIED
      $Cred4Widgets(SPEC_CALIB) delete 0 end
      $Cred4Widgets(SPEC_CALIB) insert 0 ca$env(CGS4_DATE)_oooo
    }

  } elseif {[string trim ${item}] == "cred4Standard"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      set Cred4Widgets(DIVIDE_BY_STD) YES
      set Cred4Widgets(STANDARD_MODE) BOTH
      $Cred4Widgets(SPEC_STD) delete 0 end
    } elseif {[string toupper [string trim ${value}]] == "DIVIDE_BY_STD"} {
      set Cred4Widgets(DIVIDE_BY_STD) YES
    } elseif {[string toupper [string trim ${value}]] == "STANDARD_MODE"} {
      set Cred4Widgets(STANDARD_MODE) BOTH
      $Cred4Widgets(SPEC_STD) delete 0 end
    } elseif {[string toupper [string trim ${value}]] == "SPEC_STD"} {
      set Cred4Widgets(STANDARD_MODE) SPECIFIED
      $Cred4Widgets(SPEC_STD) delete 0 end
      $Cred4Widgets(SPEC_STD) insert 0 st$env(CGS4_DATE)_gggg
    }

  } elseif {[string trim ${item}] == "cred4Configs"} {
    $Cred4Widgets(CONFIG) delete 0 end
    $Cred4Widgets(CONFIG) insert 0 $env(CGS4_CONFIG)/default.cred4

  } elseif {[string trim ${item}] == "cred4Masks"} {
    if {[string toupper [string trim ${value}]] == "ALL"} {
      $Cred4Widgets(MASK) delete 0 end
      $Cred4Widgets(MASK) insert 0 #
      $Cred4Widgets(LINCOEFFS) delete 0 end
      $Cred4Widgets(LINCOEFFS) insert 0 #
    } elseif {[string toupper [string trim ${value}]] == "MASK"} {
      $Cred4Widgets(MASK) delete 0 end
      $Cred4Widgets(MASK) insert 0 #
    } elseif {[string toupper [string trim ${value}]] == "LINCOEFFS"} {
      $Cred4Widgets(LINCOEFFS) delete 0 end
      $Cred4Widgets(LINCOEFFS) insert 0 #
    }

  } else {
    set Cred4Widgets(${item}) [string trim ${value}]
  }
  update idletasks
}
