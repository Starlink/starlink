proc qmanCancelObsRange {taskname} {
#+
# Cancel a range of observations.
#-

# Check that the observation range is valid.
    global QmanWidgets
    if {[qmanCheckEntry $QmanWidgets(OBS_RANGE_FIRST) "The first observation number"] != 0} {return}
    if {[qmanCheckEntry $QmanWidgets(OBS_RANGE_LAST) "The last observation number"] != 0} {return}
    if {[qmanCheckRange $QmanWidgets(OBS_RANGE_FIRST) $QmanWidgets(OBS_RANGE_LAST)] != 0} {return}

# Disable all buttons except the interrupt button
    cgs4drCursor pirate orange black
    grab $QmanWidgets(INTERRUPT)

# Get the desired queue position.
    set qpos $QmanWidgets(QUEUE_POSITION)

# Compute the start and end positions and the step (1 or -1).
    if {$qpos == "oldest"} {
	set index [$QmanWidgets(OBS_RANGE_FIRST) get]
	set end [$QmanWidgets(OBS_RANGE_LAST) get]
	set step 1
    } {
	set end [$QmanWidgets(OBS_RANGE_FIRST) get]
	set index [$QmanWidgets(OBS_RANGE_LAST) get]
	set step -1
    }

# Add step to the end position so that the loop termination condition is
# index equal to end which doesn't depend on the loop direction.
    incr end $step

# Clear the global abort flag.
    global QmanAbort
    set QmanAbort 0

# Send obey lock and call stage 2 when done.
    global QmanAccess
    $taskname obey lock $QmanAccess -inform "cgs4drInform $taskname %V" -endmsg "qmanCancelObs1 $taskname $index $end $step $qpos"
    cgs4drCursor arrow green black
}

proc qmanCancelObs1 {taskname index end step qpos} {
#+
# Cancel observation stage 2.

# Check for loop termination (index equal to end or the interrupt flag set).
    global QmanAbort
    if {$index != $end && $QmanAbort==0} {

#     Construct the REDUCE record name.
	set tag REDUCE
	global env
	set name "$tag o$env(QMAN_DATE)_$index"

#     Send obey read and call stage 3 when done.
	global QmanAccess
    	$taskname obey read "read_mode=search string=\"$name\" $QmanAccess search_mode=$qpos destructive=true" \
	  -inform "cgs4drInform $taskname %V" -endmsg "qmanCancelObs2 $taskname $index $end $step $qpos"
    } {

#     Unlock the database and re-enable the buttons when done.
	global QmanWidgets
	global QmanAccess
	$taskname obey unlock $QmanAccess  -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
    }
}

proc qmanCancelObs2 {taskname index end step qpos} {
#+
# Cancel end range stage 3.
#-

# Construct the END record name.
    set tag END
    global env
    set name "$tag o$env(QMAN_DATE)_$index"

# Send obey read and call stage 2 again with the index incremented when done.
    global QmanAccess
    $taskname obey read "read_mode=search string=\"$name\" $QmanAccess search_mode=$qpos destructive=true" \
      -inform "cgs4drInform $taskname %V" -endmsg "qmanCancelObs1 $taskname [incr index $step] $end $step $qpos"
}
