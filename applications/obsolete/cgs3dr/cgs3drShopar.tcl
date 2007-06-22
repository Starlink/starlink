proc cgs3drShopar {} {
  global Cgs3drTask
  cgs3drClear
  $Cgs3drTask obey shopar "" -inform "cgs3drInform %V"
}
