proc initCred4 {taskname} {
#+
# Initialises the cred4 adam task by sending a series of SET/OBEY messages
#-
    global Cred4NoticeBoard
    global QmanTask
    global P4Task
    global Red4Task
    global env

# Set some parameters
    $taskname set noticeboard $Cred4NoticeBoard -setresponse "cgs4drClear $taskname"
    $taskname set qman_alias  $QmanTask         -setresponse "cgs4drClear $taskname"
    if {$env(DOMAIN) != "ukirt.jach.hawaii.edu."} {
      $taskname set qman_pwrd   $env(QMAN_PASS)   -setresponse "cgs4drClear $taskname"
      $taskname set qman_lwrd   $env(QMAN_LOCK)   -setresponse "cgs4drClear $taskname"
    }
    $taskname set p4_alias    $P4Task           -setresponse "cgs4drClear $taskname"
    $taskname set red4_alias  $Red4Task         -setresponse "cgs4drClear $taskname"

# Do the obey sequence
    after 1000
    $taskname obey init "" -inform "cgs4drInform $taskname %V"
    $taskname obey open_nb "" -inform "cgs4drInform $taskname %V"
    $taskname obey open_qfile "" -inform "cgs4drInform $taskname %V"
    cgs4drClear $taskname
    $taskname obey status "" -inform "cgs4drInform $taskname %V"
}
