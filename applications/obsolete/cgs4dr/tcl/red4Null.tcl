proc red4Null {taskname} {
  cgs4drClear $taskname
  set message "red4Null : The requested application is not yet available!"
  cgs4drInform $taskname $message
}
