proc cgs4drSetEnv {data UT format} {
  global env

  set lformat [string trim [string tolower $format]]
  set uformat [string trim [string toupper $format]]

  set env(PID)                [cgs4drStrRev [pid]]
  set env(CGS4_CT)            $env(CGS4DR_ROOT)/$lformat
  set env(CGS4_TEMPLATES)     $env(CGS4DR_ROOT)/$lformat
  set env(CGS4_NDF_TEMPLATES) $env(CGS4DR_ROOT)/ndf
  set env(CGS4_DST_TEMPLATES) $env(CGS4DR_ROOT)/dst
  set env(CGS4_ARLINES)       $env(CGS4DR_ROOT)
  set env(CGS4_DOCS)          $env(CGS4DR_ROOT)/docs
  set env(CGS4_EXE)           $env(CGS4DR_ROOT)
  set env(CGS4_ICL)           $env(CGS4DR_ROOT)
  set env(CGS4_CONFIG)        $env(HOME)/cgs4dr_configs
  set env(CGS4_HOME)          $env(HOME)
  set env(CGS4_ENG)           $env(HOME)
  set env(CGS4_INDEX)         $data
  set env(CGS4_DATA)          $data
  set env(CGS4_DATE)          $UT
  set env(CGS4_FORMAT)        $uformat
  set status [catch {set masks $env(CGS4_MASKS)}]
  if {$status!=0} {set env(CGS4_MASKS) $env(CGS4DR_ROOT)/$lformat}

  set env(RED4_FORMAT)        $uformat

  set env(P4_ROOT)            $env(CGS4DR_ROOT)
  set env(P4_EXE)             $env(CGS4DR_ROOT)
  set env(P4_ICL)             $env(CGS4DR_ROOT)
  set env(P4_CT)              $env(CGS4DR_ROOT)/$lformat
  set env(P4_HOME)            $env(HOME)
  set env(P4_CONFIG)          $env(HOME)/cgs4dr_configs
  set env(P4_HC)              [string trim [exec /usr/bin/pwd]]
  set env(P4_DATE)            $UT
  set env(P4_DATA)            $data

  set env(QMAN_ROOT)          $env(CGS4DR_ROOT)
  set env(QMAN_EXE)           $env(CGS4DR_ROOT)
  set env(QMAN_ICL)           $env(CGS4DR_ROOT)
  set env(QMAN_CONFIG)        $env(HOME)/cgs4dr_configs
  set env(QMAN_DATE)          $UT
  if {$env(DOMAIN) != "ukirt.jach.hawaii.edu."} {
    set env(QMAN_PASS)        $env(PID)pass
    set env(QMAN_LOCK)        $env(PID)lock
  }

  set env(IDIR)               $data/idir
  set env(ODIR)               $data/odir
  set env(RIDIR)              $data/ridir
  set env(RGDIR)              $data/rgdir
  set env(RODIR)              $data/rodir

  set env(FIGARO_FORMATS)     $uformat
}
