proc cgs4drKillPack {taskname} {
  if {[info commands ukirtfig*] != ""} {
    cgs4drInform $taskname "Killing ukirtfig..."
    ukirtfig kill
  }
  if {[info commands figaro1*] != ""} {
    cgs4drInform $taskname "Killing figaro1..."
    figaro1 kill
  }
  if {[info commands figaro3*] != ""} {
    cgs4drInform $taskname "Killing figaro3..."
    figaro3 kill
  }
  if {[info commands ndfpack_mon*] != ""} {
    cgs4drInform $taskname "Killing ndfpack_mon..."
    ndfpack_mon kill
  }
  if {[info commands hdstrace*] != ""} {
    cgs4drInform $taskname "Killing hdstrace..."
    hdstrace kill
  }
}
