proc qmanListQueue {taskname} {
#-
# List the queue.
#-
    global QmanAccess
    global QmanWidgets

# Disable all buttons except the interrupt button
    cgs4drCursor pirate orange black
    grab $QmanWidgets(INTERRUPT)

# Sort the database
    $taskname obey lock $QmanAccess -inform "cgs4drInform $taskname %V"
    $taskname obey sort "$QmanAccess sort_mode=descending" -inform "cgs4drInform $taskname %V"
    $taskname obey list "$QmanAccess list_mode=all" -inform "cgs4drInform $taskname %V"
    $taskname obey unlock $QmanAccess -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
    cgs4drCursor arrow green black
}
