proc red4GetResponse {task param value status} {
  global $param
  if {[lindex $status 0]=="SAI__OK"} {
    set message "$param = $value"
    cgs4drInform $task $message
    set $param $value
    return $param
  } else {
    cgs4drClear $task
    cgs4drInform $task "red4GetResponse error : $param = $value returned with status != SAI__OK"
  }
}
