proc cgs3drClear {} {
  global Cgs3drWidgets
  $Cgs3drWidgets(OUTPUT) configure -state normal
  $Cgs3drWidgets(OUTPUT) delete 1.0 end
  $Cgs3drWidgets(OUTPUT) configure -state disabled
}
