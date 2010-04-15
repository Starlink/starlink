proc red4ReportEmlt {taskname} {
#+
# Creates a dialog box for red4 action
#-

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Do it
    $taskname obey report_emlt "" -inform "cgs4drInform $taskname %V"
}
