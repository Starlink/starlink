proc red4Reset {taskname} {
#+
# Creates a dialog box for red4 action
#-

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Do it
    set message "Resetting reduction monolith"
    cgs4drInform $taskname $message
    $taskname obey reset "" -inform "cgs4drInform $taskname %V"
}
