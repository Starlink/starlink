proc red4Format {taskname format} {
#+
# Creates a dialog box for red4 action
#-
    global env

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Do it
    set hformat [string trim [string toupper $format]]
    set env(FIGARO_FORMATS) $hformat
    $taskname obey set_format "format=$hformat" -inform "cgs4drInform $taskname %V"
}
