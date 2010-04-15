proc qmanEnterObsRange {taskname} {
#+
# Enter a range of observations.
#-
    global QmanWidgets

# Check that the observation range is valid.
    if {[qmanCheckEntry $QmanWidgets(OBS_RANGE_FIRST) "The first observation number"] != 0} {return}
    if {[qmanCheckEntry $QmanWidgets(OBS_RANGE_LAST) "The last observation number"] != 0} {return}
    if {[qmanCheckRange $QmanWidgets(OBS_RANGE_FIRST) $QmanWidgets(OBS_RANGE_LAST)] != 0} {return}

# Disable all except the abort button
    cgs4drCursor pirate orange black
    grab $QmanWidgets(INTERRUPT)

# Get the desired queue position.
    set qpos $QmanWidgets(QUEUE_POSITION)

# Get the start and end record numbers and set the step to 1 or -1.
    if {$qpos == "oldest"} {
	set index [$QmanWidgets(OBS_RANGE_FIRST) get]
	set end [$QmanWidgets(OBS_RANGE_LAST) get]
	set step 1
    } {
	set end [$QmanWidgets(OBS_RANGE_FIRST) get]
	set index [$QmanWidgets(OBS_RANGE_LAST) get]
	set step -1
    }

# Add the step to the end number so that the termination conditions is
# index equal to step which doesn't depend on the step direction.
    incr end $step

# Clear the global abort flag.
    global QmanAbort
    set QmanAbort 0

# Send obey lock and call stage 2 when done.
    global QmanAccess
    $taskname obey lock $QmanAccess -inform "cgs4drInform $taskname %V" -endmsg "qmanEnterObs1 $taskname $index $end $step $qpos"
    cgs4drCursor arrow green black
}

proc qmanEnterObs1 {taskname index end step qpos} {
#+
# Enter observation range stage 2.
#-
    global QmanAbort
    global QmanAccess

# Check for the loop end condition (index equal to step or the interrupt
# flag set.
    if {$index != $end && $QmanAbort == 0} {

#     Construct the record name. The tag depends on which end of the list we are adding to.
	if {$qpos == "oldest"} {
	    set tag REDUCE
	} {
	    set tag END
	}
	global env
	set name "$tag o$env(QMAN_DATE)_$index"

#     Send obey write and call stage 2 when done.
    	$taskname obey write "string=\"$name\" $QmanAccess qposition=$qpos" \
	  -inform "cgs4drInform $taskname %V" -endmsg "qmanEnterObs2 $taskname $index $end $step $qpos"
    } {

#     Send obey unlock and re-enable the buttons when done.
	global QmanWidgets
	$taskname obey unlock $QmanAccess -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
    }
}

proc qmanEnterObs2 {taskname index end step qpos} {
#+
# Enter observation range stage 3.
#-
    global QmanAccess

# Construct the "other" record name.
    if {$qpos == "newest"} {
	set tag REDUCE
    } {
	set tag END
    }

# Send obey write and call stage 2 again with the index incremented when done.
    global env
    set name "$tag o$env(QMAN_DATE)_$index"
    $taskname obey write "string=\"$name\" $QmanAccess qposition=$qpos" \
      -inform "cgs4drInform $taskname %V" -endmsg "qmanEnterObs1 $taskname [incr index $step] $end $step $qpos"
}
