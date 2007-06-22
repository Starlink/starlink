proc cgs3drSetEnv {data UT} {
  global env

# Set global environmental variables
  set env(PID)        [cgs3drStrRev [pid]]
  set env(TODAY2)     ${UT}
  set env(DATADIR)    ${data}
  set env(RODIR)      ${data}/rodir

# Check for existence of rodir
  if {![file exists $env(RODIR)]} {
    if {[file exists $env(DATADIR)/RODIR]} {
      set env(RODIR) ${data}/RODIR
    } else {
      exec /usr/bin/mkdir $env(RODIR)
    }
  }
}
