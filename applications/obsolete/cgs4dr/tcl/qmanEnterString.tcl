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
    set sval [string trim [$QmanWidgets(STRING) get]]
    set fval $QmanWidgets(RB_STRING)
    if {$fval == "DRCOMMENT"} {
      set name "$fval $sval"
      $taskname obey write "$QmanAccess qposition=$qpos string=\"$name\"" -inform "cgs4drInform $taskname %V"
    } else {
      if {$sval != ""} {
        set name "$fval $sval"
        $taskname obey write "$QmanAccess qposition=$qpos string=\"$name\"" -inform "cgs4drInform $taskname %V"
      } else {
        cgs4drClear $taskname
        set message "qmanEnterString error : cannot accept a null string for $fval!"
        cgs4drInform $taskname $message
      }
    }

# Unlock the database
    $taskname obey unlock $QmanAccess  -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
    cgs4drCursor arrow green black
}

