proc bindQmanWidgets {taskname} {
#+
# This procedure binds the actions to all the "active" widgets in the qman interface.
#-
    global QmanWidgets
    $QmanWidgets(LIST_QUEUE) configure -command "qmanListQueue $taskname"
    $QmanWidgets(ENTER_OBSERVATION_RANGE) configure -command "qmanEnterObsRange $taskname"
    $QmanWidgets(CANCEL_OBSERVATION_RANGE) configure -command "qmanCancelObsRange $taskname"
    $QmanWidgets(CANCEL_ALL_ENTRIES) configure -command "qmanCancelAll $taskname"
    $QmanWidgets(INTERRUPT) configure -command "set QmanAbort 1"

# Trace the verbose variable
    trace variable QmanWidgets(VERBOSE) w "cgs4drVerbose $taskname"
    set QmanWidgets(VERBOSE) 0
}
