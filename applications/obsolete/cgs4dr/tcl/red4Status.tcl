proc red4Status {taskname} {
#+
# Creates a dialog box for red4 action
#-

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Do it
    $taskname obey status "" -inform "cgs4drInform $taskname %V"
}
