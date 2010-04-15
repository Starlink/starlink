proc cgs4drLoadTask {task} {
#+
#  Load a CGS4DR task
#-
   global env
   global QmanAccess
   set ltask [string trim [string tolower $task]]

# Cred4
   if {$ltask == "cred4"} {
     if {[file exists $env(CGS4_CONFIG)/standard.cred4]==0} {
       exec /usr/bin/cp $env(CGS4DR_ROOT)/standard.cred4 $env(CGS4_CONFIG)/standard.cred4
     }
     if {[file exists $env(CGS4_CONFIG)/STANDARD.cred4]==0} {
       exec /usr/bin/cp $env(CGS4DR_ROOT)/standard.cred4 $env(CGS4_CONFIG)/STANDARD.cred4
     }
     if {[file exists $env(CGS4_CONFIG)/default.cred4]==0} {
       exec /usr/bin/cp $env(CGS4DR_ROOT)/default.cred4 $env(CGS4_CONFIG)/default.cred4
     }
     set taskname $env(PID)_$ltask
     adamtask $taskname $env(CGS4DR_ROOT)/$ltask
     cgs4drPath $taskname
     return $taskname

# P4
   } elseif {$ltask == "p4"} {

#  Need to dynamically edit standard.p4 to use proper PID
     set fid [open $env(CGS4DR_ROOT)/standard.p4 r]
     if {[file exists $env(P4_CONFIG)/standard.p4] == 1} {exec /usr/bin/rm -f $env(CGS4_CONFIG)/standard.p4}
     set fid2 [open $env(P4_CONFIG)/standard.p4 w]
     while {[gets $fid line] >=0 } {
       regsub -nocase "xwindows" ${line} "xwindows;$env(PID)xwin" line
       puts $fid2 $line
     }
     close $fid
     close $fid2
     exec /usr/bin/cp $env(P4_CONFIG)/standard.p4 $env(P4_CONFIG)/STANDARD.p4
     if {[file exists $env(P4_CONFIG)/default.p4]==0} {
       exec /usr/bin/cp $env(CGS4DR_ROOT)/default.p4 $env(P4_CONFIG)/default.p4
     }
     set taskname $env(PID)_$ltask
     adamtask $taskname $env(CGS4DR_ROOT)/$ltask
     cgs4drPath $taskname
     return $taskname

# Red4
   } elseif {$ltask == "red4"} {
     set taskname $env(PID)_$ltask
     adamtask $taskname $env(CGS4DR_ROOT)/$ltask
     cgs4drPath $taskname
     return $taskname

# Qman
   } elseif {$ltask == "qman"} {
     if {[file exists $env(QMAN_CONFIG)/default.qman]==0} {
       exec /usr/bin/cp $env(CGS4DR_ROOT)/default.qman $env(QMAN_CONFIG)/default.qman
     }
     if {$env(DOMAIN) != "ukirt.jach.hawaii.edu."} {
       set QmanAccess "password=$env(QMAN_PASS) lockword=$env(QMAN_LOCK)"
       set taskname $env(PID)_$ltask
     } else {
       set QmanAccess "password='' lockword=''"
       set taskname $ltask
     }
     adamtask $taskname $env(CGS4DR_ROOT)/$ltask
     cgs4drPath $taskname
     return $taskname
   }
}
