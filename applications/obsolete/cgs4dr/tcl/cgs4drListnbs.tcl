proc cgs4drListnbs {taskname item} {
#+
# Lists noticeboard.
#-
    set info [nbs info $item]
    if {[lindex $info 0]} {
        set message "$item: ([lindex $info 1]*[lindex $info 2]) [lindex $info 3] [nbs get $item]"
        cgs4drInform $taskname $message
    } {
        for {set i 1} {$i < [llength $info]} {incr i} {
           cgs4drListnbs $taskname $item.[lindex $info $i]
        }
    }
}
