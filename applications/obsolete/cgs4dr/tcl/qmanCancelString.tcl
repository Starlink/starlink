proc qmanCancelString {taskname} {
#+
# Cancels a string (used for masks, configs, skywt, varwt and strings)
#-
    global QmanAccess
    global QmanWidgets

# Disable the all but the interrupt button
    cgs4drCursor pirate orange black
    grab $QmanWidgets(INTERRUPT)

# Lock the database
    $taskname obey lock $QmanAccess -inform "cgs4drInform $taskname %V"

# Set string
    set qpos $QmanWidgets(QUEUE_POSITION)
    set QmanWidgets(EW_STRING) [string trim [$QmanWidgets(STRING) get]]
    set QmanWidgets(RB_STRING) $QmanWidgets(RB_STRING)
    set name "$QmanWidgets(RB_STRING) $QmanWidgets(EW_STRING)"

# Delete the string using destructive reads
    $taskname obey read "$QmanAccess search_mode=$qpos string=\"$name\" \
      read_mode=search destructive=true" -inform "cgs4drInform $taskname %V"

# Unlock the database and release widget
    $taskname obey unlock $QmanAccess -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
    cgs4drCursor arrow green black
}
