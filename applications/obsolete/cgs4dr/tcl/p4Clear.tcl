proc p4Clear taskname {
#+
# Sends obey clear to the p4 task.
#-
    global P4Widgets
    cgs4drCursor pirate orange black
    $taskname obey clear "port=$P4Widgets(PORT_NO)" -inform "cgs4drInform $taskname %V"
    cgs4drCursor arrow green black
}
