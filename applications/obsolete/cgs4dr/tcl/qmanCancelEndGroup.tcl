proc qmanCancelEndGroup {taskname} {
#+
# Cancels an end group entry.
#-
    global QmanAccess
    global QmanWidgets
    global env

# Check that the group number is valid
    if {[qmanCheckEntry $QmanWidgets(GROUP_NUMBER) "The group number"] != 0} {return}

# Disable all buttons except the interrupt button and lock database
    cgs4drCursor pirate orange black
    grab $QmanWidgets(INTERRUPT)
    $taskname obey lock $QmanAccess -inform "cgs4drInform $taskname %V"

# Construct the record name.
    set qpos $QmanWidgets(QUEUE_POSITION)
    set tag ENDGROUP
    set QmanWidgets(DGN) [$QmanWidgets(GROUP_NUMBER) get]
    set name "$tag rg$env(QMAN_DATE)_$QmanWidgets(DGN)"

# Cancel the endgroup with a destructive read
    $taskname obey read "$QmanAccess search_mode=$qpos string=\"$name\" \
      read_mode=search destructive=true" -inform "cgs4drInform $taskname %V"
    $taskname obey unlock $QmanAccess -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
    cgs4drCursor arrow green black
}
