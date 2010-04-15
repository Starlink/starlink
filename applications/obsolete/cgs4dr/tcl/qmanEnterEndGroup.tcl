proc qmanEnterEndGroup {taskname} {
#+
# Enter an end group
#-
    global QmanAccess
    global QmanWidgets
    global env

# Check that the group number is valid
    if {[qmanCheckEntry $QmanWidgets(GROUP_NUMBER) "The group number"] != 0} {return}

# Disable all buttons except the interrupt button
    cgs4drCursor pirate orange black
    grab $QmanWidgets(INTERRUPT)
    $taskname obey lock $QmanAccess -inform "cgs4drInform $taskname %V"

# Construct the record name
    set qpos $QmanWidgets(QUEUE_POSITION)
    set tag ENDGROUP
    set QmanWidgets(DGN) [$QmanWidgets(GROUP_NUMBER) get]
    set name "$tag rg$env(QMAN_DATE)_$QmanWidgets(DGN)"

# Write the record
    $taskname obey write "$QmanAccess qposition=$qpos string=\"$name\"" -inform "cgs4drInform $taskname %V"

# Unlock the database and release widget
    $taskname obey unlock $QmanAccess -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
    cgs4drCursor arrow green black
}
