proc qmanStatus {taskname} {
#+
# Sends a status command to qman
#-
    global QmanAccess
    cgs4drCursor pirate orange black
    cgs4drClear $taskname
    $taskname obey status $QmanAccess -inform "cgs4drInform $taskname %V"
    cgs4drCursor arrow green black
}
