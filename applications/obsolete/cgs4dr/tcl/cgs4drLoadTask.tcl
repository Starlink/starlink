proc cgs4drLoadTask {task} {
#+
#  Load a CGS4DR task
#-
   global env
   global QmanAccess
   set ltask [string trim [string tolower $task]]

# Cred4
   if {$ltask == "cred4"} {
     if {[file exists $env(CGS4_CONFIG)/default.cred4]==0} {
       exec /usr/bin/cp $env(CGS4DR_ROOT)/default.cred4 $env(CGS4_CONFIG)/default.cred4
     }

# P4
   } elseif {$ltask == "p4"} {
     if {[file exists $env(P4_CONFIG)/default.p4]==0} {
       exec /usr/bin/cp $env(CGS4DR_ROOT)/default.p4 $env(P4_CONFIG)/default.p4
     }

# Red4
   } elseif {$ltask == "red4"} {

# Qman
   } elseif {$ltask == "qman"} {
     if {[file exists $env(QMAN_CONFIG)/default.qman]==0} {
       exec /usr/bin/cp $env(CGS4DR_ROOT)/default.qman $env(QMAN_CONFIG)/default.qman
     }
     if {$env(DOMAIN) != "ukirt.jach.hawaii.edu."} {
       set QmanAccess "password=$env(QMAN_PASS) lockword=$env(QMAN_LOCK)"
     } else {
       set QmanAccess "password='' lockword=''"
     }
   }

# Load the task and return name
   if {$env(DOMAIN) != "ukirt.jach.hawaii.edu."} {
     set taskname $env(PID)_$ltask
   } else {
     set taskname [string tolower [string trim $ltask]]
   }
   adamtask $taskname $env(CGS4DR_ROOT)/$ltask
   cgs4drPath $taskname
   return $taskname
}

