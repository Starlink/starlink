proc CCDCallTrace { } { 
   puts "Tcl Call Trace"
   for { set l [expr [info level]-1]} { $l > 0 } { incr l -1 } { 
      puts "$l: [info level $l"
   }
}
