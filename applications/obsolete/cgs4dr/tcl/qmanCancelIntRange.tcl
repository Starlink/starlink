proc qmanCancelIntRange {taskname} {
#+
# Cancel a range of observations
#-

# Check that the observation range is valid.
    global QmanWidgets
    if {[qmanCheckEntry $QmanWidgets(OBS_NUMBER) "The observation number"] != 0} {return}
    if {[qmanCheckEntry $QmanWidgets(INT_RANGE_FIRST) "The first integration number"] != 0} {return}
    if {[qmanCheckEntry $QmanWidgets(INT_RANGE_LAST) "The last integration number"] != 0} {return}
    if {[qmanCheckRange $QmanWidgets(INT_RANGE_FIRST) $QmanWidgets(INT_RANGE_LAST)] != 0} {return}

# Disable all but the interrupt button
    cgs4drCursor pirate orange black
    grab $QmanWidgets(INTERRUPT)

# Get the desired queue position.
    set qpos $QmanWidgets(QUEUE_POSITION)

# Compute the start and end positions and the step (1 or -1).
    set obsnum [$QmanWidgets(OBS_NUMBER) get]
    set QmanWidgets(DON) $obsnum
    if {$qpos == "oldest"} {
	set index [$QmanWidgets(INT_RANGE_FIRST) get]
        set QmanWidgets(DIF) $index
	set end [$QmanWidgets(INT_RANGE_LAST) get]
        set QmanWidgets(DIL) $end
	set step 1
    } {
	set end [$QmanWidgets(INT_RANGE_FIRST) get]
        set QmanWidgets(DIF) $end
	set index [$QmanWidgets(INT_RANGE_LAST) get]
        set QmanWidgets(DIL) $index
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
    $taskname obey lock $QmanAccess  -inform "cgs4drInform $taskname %V" \
      -endmsg "qmanCancelInt1 $taskname $obsnum $index $end $step $qpos"
}

proc qmanCancelInt1 {taskname obsnum index end step qpos} {

# Check for loop termination (index equal to end or the interrupt flag set).
    global QmanAbort
    if {$index != $end && $QmanAbort==0} {

#     Form the REDUCE record name.
	global env
	set name "REDUCE i$env(QMAN_DATE)_[expr $obsnum]_[expr $index]"

#     Delete the record.
	global QmanAccess
    	$taskname obey read "read_mode=search string=\"$name\" $QmanAccess search_mode=$qpos destructive=true" \
	  -inform "cgs4drInform $taskname %V" -endmsg "qmanCancelInt2 $taskname $obsnum [incr index $step] $end $step $qpos"
    } {

#     Unlock the database and re-enable the buttons when done.
	global QmanWidgets
	global QmanAccess
	$taskname obey unlock $QmanAccess  -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
        cgs4drCursor arrow green black
    }
}

proc qmanCancelInt2 {taskname obsnum index end step qpos} {

# Check for loop termination (index equal to end or the interrupt flag set).
    global QmanAbort
    if {$index != $end && $QmanAbort==0} {

#     Form the REDUCE record name.
	global env
	set name "REDUCE i$env(QMAN_DATE)_[expr $obsnum]_[expr $index]"

#     Delete the record.
	global QmanAccess
    	$taskname obey read "read_mode=search string=\"$name\" $QmanAccess search_mode=$qpos destructive=true" \
	  -inform "cgs4drInform $taskname %V" -endmsg "qmanCancelInt1 $taskname $obsnum [incr index $step] $end $step $qpos"
    } {

#     Unlock the database and re-enable the buttons when done.
	global QmanWidgets
	global QmanAccess
	$taskname obey unlock $QmanAccess  -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
        cgs4drCursor arrow green black
    }
}
