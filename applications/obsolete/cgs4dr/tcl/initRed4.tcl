proc initRed4 {taskname} {
#+
# Initialises the red4 adam task by sending a series of SET/OBEY messages.
#-

    global env
    after 1000
    $taskname obey init "" -inform "cgs4drInform $taskname %V"
    $taskname obey set_format "format=$env(RED4_FORMAT)" -inform "cgs4drInform $taskname %V"
    cgs4drClear $taskname
    $taskname obey status "" -inform "cgs4drInform $taskname %V"
}

