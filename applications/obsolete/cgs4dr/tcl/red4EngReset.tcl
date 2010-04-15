proc red4EngReset {taskname} {
#+
# Creates a dialog box for red4 action
#-

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Do it
    set message "Resetting engineering functions"
    cgs4drInform $taskname $message
    $taskname obey eng_reset "" -inform "cgs4drInform $taskname %V"
}
