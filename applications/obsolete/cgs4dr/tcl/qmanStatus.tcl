proc qmanStatus {taskname} {
#+
# Sends a status command to qman
#-
    global QmanAccess
    cgs4drCursor pirate orange black
    $taskname obey status $QmanAccess -inform "cgs4drInform $taskname %V"
    cgs4drCursor arrow green black
}
