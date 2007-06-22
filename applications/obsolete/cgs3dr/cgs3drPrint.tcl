proc cgs3drPrint {} {

# Set a local task name
  global env
  global Cgs3drWidgets
  global Cgs3drTask

# Confirm that this is what we want to do
  set frame [dialogStart .cgs3drDialogue "Print Confirmation" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set label1 [label $frame.lab1 -text "This action copies the textpane to the printer." -width 60]
  set label2 [label $frame.lab2 -text "Continue?" -width 60]
  pack $label1 $label2 -in $frame -side top

# Show the dialog box and proceed if OK
  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv == 0} {

# open the output file and copy the contents
    set fid [open $env(HOME)/$Cgs3drTask.textpane w]
    $Cgs3drWidgets(OUTPUT) configure -state normal
    puts $fid [$Cgs3drWidgets(OUTPUT) get 1.0 end]
    $Cgs3drWidgets(OUTPUT) configure -state disabled
    close $fid
    cgs3drInform "Sending $env(HOME)/$Cgs3drTask.textpane to the printer"
    exec /usr/bin/lp -c $env(HOME)/$Cgs3drTask.textpane
    update idletasks
  }

# Destroy the panel and quit
  destroy .cgs3drDialogue
  cgs3drCursor arrow green black
}
