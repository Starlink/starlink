proc bindCgs3drWidgets {} {
  global Cgs3drWidgets

# Command buttons.
  $Cgs3drWidgets(REDUCE_RUN) configure -command "cgs3drReduce RUN"
  $Cgs3drWidgets(REDUCE_GRP) configure -command "cgs3drReduce GRP"
  $Cgs3drWidgets(REDUCE_PHOT) configure -command "cgs3drReduce PHOT"
  $Cgs3drWidgets(SETPAR) configure -command "cgs3drSetpar"
  $Cgs3drWidgets(SHOPAR) configure -command "cgs3drShopar"

  global Red3Widgets
  bind $Red3Widgets(LB) <Double-Button-1> "red3Action \[selection get]\ "
}
