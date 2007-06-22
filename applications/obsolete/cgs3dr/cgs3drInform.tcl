proc cgs3drInform {message} {
  global Cgs3drWidgets
  $Cgs3drWidgets(OUTPUT) configure -state normal
  $Cgs3drWidgets(OUTPUT) insert end "$message\n"
  $Cgs3drWidgets(OUTPUT) yview -pickplace end
  $Cgs3drWidgets(OUTPUT) configure -state disabled
  update idletasks
}





