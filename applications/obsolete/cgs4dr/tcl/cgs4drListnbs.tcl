proc cgs4drListnbs {taskname item} {
#+
# Lists noticeboard.
#-
    set info [nbs info $item]
    if {[lindex $info 0]} {
        set item  "[string tolower $item]"
        set type  "([lindex $info 1]*[lindex $info 2]) [lindex $info 3]"
        set value "[string trim [nbs get $item]]"
        set message [format "%40s:\t %s\t %s" $item $type $value]
        cgs4drInform $taskname $message
    } {
        for {set i 1} {$i < [llength $info]} {incr i} {
           cgs4drListnbs $taskname $item.[lindex $info $i]
        }
    }
}
