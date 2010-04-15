proc qmanEnterString {taskname} {
#+
# Enter a string (used for masks, configs, skywt, varwt and strings)
#-
    global QmanAccess
    global QmanWidgets

# Disable all but the interrupt button
    cgs4drCursor pirate orange black
    grab $QmanWidgets(INTERRUPT)

# Lock the database
    $taskname obey lock $QmanAccess -inform "cgs4drInform $taskname %V"

# Set strings
    set qpos $QmanWidgets(QUEUE_POSITION)
    set QmanWidgets(EW_STRING) [string trim [$QmanWidgets(STRING) get]]
    if {$QmanWidgets(RB_STRING) == "DRCOMMENT"} {
      set name "$QmanWidgets(RB_STRING) $QmanWidgets(EW_STRING)"
      $taskname obey write "$QmanAccess qposition=$qpos string=\"$name\"" -inform "cgs4drInform $taskname %V"
    } else {
      if {$QmanWidgets(EW_STRING) != ""} {
        set name "$QmanWidgets(RB_STRING) $QmanWidgets(EW_STRING)"
        $taskname obey write "$QmanAccess qposition=$qpos string=\"$name\"" -inform "cgs4drInform $taskname %V"
      } else {
        cgs4drClear $taskname
        cgs4drInform $taskname "qmanEnterString error : Cannot accept a null string for $QmanWidgets(RB_STRING)!"
      }
    }

# Unlock the database
    $taskname obey unlock $QmanAccess  -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
    cgs4drCursor arrow green black
}

