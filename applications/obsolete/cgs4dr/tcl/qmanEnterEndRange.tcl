proc qmanEnterEndRange {taskname} {
#+
# Enter a range of observations.
#-
    global QmanWidgets

# Check that the observation range is valid.
    if {[qmanCheckEntry $QmanWidgets(END_RANGE_FIRST) "The first observation number"] != 0} {return}
    if {[qmanCheckEntry $QmanWidgets(END_RANGE_LAST) "The last observation number"] != 0} {return}
    if {[qmanCheckRange $QmanWidgets(END_RANGE_FIRST) $QmanWidgets(END_RANGE_LAST)] != 0} {return}

# Disable all except the abort button
    cgs4drCursor pirate orange black
    grab $QmanWidgets(INTERRUPT)

# Get the desired queue position.
    set qpos $QmanWidgets(QUEUE_POSITION)

# Get the start and end record numbers and set the step to 1 or -1.
    if {$qpos == "oldest"} {
	set index [$QmanWidgets(END_RANGE_FIRST) get]
	set end [$QmanWidgets(END_RANGE_LAST) get]
	set step 1
    } {
	set end [$QmanWidgets(END_RANGE_FIRST) get]
	set index [$QmanWidgets(END_RANGE_LAST) get]
	set step -1
    }

# Add the step to the end number so that the termination conditions is
# index equal to step which doesn't depend on the step direction.
    incr end $step

# Clear the global abort flag.
    global QmanAbort
    set QmanAbort 0

# Start the OBEY sequence.
    global QmanAccess
    $taskname obey lock $QmanAccess -inform "cgs4drInform $taskname %V" -endmsg "qmanEnterEnd1 $taskname $index $end $step $qpos"
}

proc qmanEnterEnd1 {taskname index end step qpos} {
    global QmanAbort
    global QmanAccess

# Check for the loop end condition (index equal to step or the interrupt
# flag set.
    if {$index != $end && $QmanAbort == 0} {

#     Form the record name.
	global env
	set name "END o$env(QMAN_DATE)_$index"

#     Add the record.
    	$taskname obey write "string=\"$name\" $QmanAccess qposition=$qpos" \
	  -inform "cgs4drInform $taskname %V" -endmsg "qmanEnterEnd2 $taskname [incr index $step] $end $step $qpos"
    } {

#     Unlock the database and re-enable the buttons when done.
	global QmanWidgets
	$taskname obey unlock $QmanAccess  -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
        cgs4drCursor arrow green black
    }
}

proc qmanEnterEnd2 {taskname index end step qpos} {
    global QmanAbort
    global QmanAccess

# Check for the loop end condition (index equal to step or the interrupt
# flag set.
    if {$index != $end && $QmanAbort == 0} {

#     Form the record name.
	global env
	set name "END o$env(QMAN_DATE)_$index"

#     Add the record.
    	$taskname obey write "string=\"$name\" $QmanAccess qposition=$qpos" \
	  -inform "cgs4drInform $taskname %V" -endmsg "qmanEnterEnd1 $taskname [incr index $step] $end $step $qpos"
    } {

#     Unlock the database and re-enable the buttons when done.
	global QmanWidgets
	$taskname obey unlock $QmanAccess  -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
        cgs4drCursor arrow green black
    }
}
