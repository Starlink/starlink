proc initQman taskname {
#+
# Initialises the qman adam task by sending it a series of OBEY messages.
# After the final OBEY has been completed the active widgets are enabled.
#-
    global env
    global QmanAccess
    global QmanWidgets
    set qman_df $env(HOME)/cgs4dr_configs/default.qman
    $taskname obey init $QmanAccess -inform "cgs4drInform $taskname %V"
    $taskname obey restore "$QmanAccess file=$qman_df" -inform "cgs4drInform $taskname %V"
    cgs4drClear $taskname
    $taskname obey status $QmanAccess -inform "cgs4drInform $taskname %V" -endmsg "grab release $QmanWidgets(INTERRUPT)"
}
