proc qmanCancelEndRange {taskname} {
#+
# Cancel a range of observations
#-

# Check that the observation range is valid.
    global QmanWidgets
    if {[qmanCheckEntry $QmanWidgets(END_RANGE_FIRST) "The first observation number"] != 0} {return}
    if {[qmanCheckEntry $QmanWidgets(END_RANGE_LAST) "The last observation number"] != 0} {return}
    if {[qmanCheckRange $QmanWidgets(END_RANGE_FIRST) $QmanWidgets(END_RANGE_LAST)] != 0} {return}

# Disable all but the interrupt button
    cgs4drCursor pirate orange black
    grab $QmanWidgets(INTERRUPT)

# Get the desired queue position.
    set qpos $QmanWidgets(QUEUE_POSITION)

# Compute the start and end positions and the step (1 or -1).
    if {$qpos == "oldest"} {
	set index [$QmanWidgets(END_RANGE_FIRST) get]
        set QmanWidgets(DEF) $index
	set end [$QmanWidgets(END_RANGE_LAST) get]
        set QmanWidgets(DEL) $end
	set step 1
    } {
	set end [$QmanWidgets(END_RANGE_FIRST) get]
        set QmanWidgets(DEF) $end
	set index [$QmanWidgets(END_RANGE_LAST) get]
        set QmanWidgets(DEL) $index
	set step -1
    }

# Add step to the end position so that the loop termination condition is
# index equal to end which doesn't depend on the loop direction.
    incr end $step

# Clear the global abort flag.
    global QmanAbort
    set QmanAbort 0

# Start the obey sequence.
    global QmanAccess
    $taskname obey lock $QmanAccess  -inform "cgs4drInform $taskname %V" -endmsg "qmanCancelEnd1 $taskname $index $end $step $qpos"
}

proc qmanCancelEnd1 {taskname index end step qpos} {

# Check for loop termination (index equal to end or the interrupt flag set).
    global QmanAbort
    if {$index != $end && $QmanAbort==0} {

#     Form the record name.
	global env
	set name "END o$env(QMAN_DATE)_$index"

#     Delete the record.
	global QmanAccess
    	$taskname obey read "read_mode=search string=\"$name\" \
	    $QmanAccess search_mode=$qpos destructive=true" -inform "cgs4drInform $taskname %V" \
	    -endmsg "qmanCancelEnd2 $taskname [incr index $step] $end $step $qpos"
    } {

#     Unlock the database and re-enable the buttons when done.
	global QmanWidgets
	global QmanAccess
	$taskname obey unlock $QmanAccess  -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
        cgs4drCursor arrow green black
    }
}

proc qmanCancelEnd2 {taskname index end step qpos} {

# Check for loop termination (index equal to end or the interrupt flag set).
    global QmanAbort
    if {$index != $end && $QmanAbort==0} {

#     Form the record name.
	global env
	set name "END o$env(QMAN_DATE)_$index"

#     Delete the record.
	global QmanAccess
    	$taskname obey read "read_mode=search string=\"$name\" \
	    $QmanAccess search_mode=$qpos destructive=true" \
	    -inform "cgs4drInform $taskname %V" -endmsg "qmanCancelEnd1 $taskname [incr index $step] $end $step $qpos"
    } {

#     Unlock the database and re-enable the buttons when done.
	global QmanWidgets
	global QmanAccess
	$taskname obey unlock $QmanAccess  -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
        cgs4drCursor arrow green black
    }
}
