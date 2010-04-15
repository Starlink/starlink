proc cgs4drAdamnet {cmd} {

# Check we're at ukirt
  global env
  if {$env(DOMAIN) == "ukirt.jach.hawaii.edu."} {
    global ADAMnet

# Stop the adamnet process and task rendezvous files
    if {[string trim [string tolower $cmd]] == "stop"} {
      exec kill -9 $ADAMnet
      foreach file \
        [glob $env(HOME)/adam/[string trim [exec /usr/bin/uname -n]]_*] {
        exec /usr/bin/rm -f $file
      }
      set ADAMnet "???"

# Start the adamnet process
    } elseif {[string trim [string tolower $cmd]] == "start"} {
      set ADAMnet [exec /ukirt_sw/sun4_Solaris/bin/adamnet/adamnet &]
    }
  }
}
