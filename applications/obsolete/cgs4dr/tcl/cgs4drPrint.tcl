proc cgs4drPrint {taskname} {

# Set a local task name
  global env
  global QmanTask
  global P4Task
  global Cred4Task
  global Red4Task
  set local_task [string tolower [string trim $taskname]]

# Confirm that this is what we want to do
  set frame [dialogStart .cgs4drDialogue "Print Confirmation" 0 OK Cancel]
  cgs4drCursor pirate orange black
  .cgs4drDialogue config -cursor {arrow green black}

  set label1 [label $frame.lab1 -text "This action copies the textpane to the printer." -width 60]
  set label2 [label $frame.lab2 -text "Continue?" -width 60]
  pack $label1 $label2 -in $frame -side top

# Show the dialog box and proceed if OK
  set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
  if {$bv == 0} {

# open the output file
    set fid [open $env(HOME)/$taskname.textpane w]

# qman task
    if {[string match $QmanTask $local_task]} {
      global QmanWidgets
      $QmanWidgets(OUTPUT) configure -state normal
      puts $fid [$QmanWidgets(OUTPUT) get 1.0 end]
      $QmanWidgets(OUTPUT) configure -state disabled

# p4 task
    } elseif {[string match $P4Task $local_task]} {
      global P4Widgets
      $P4Widgets(OUTPUT) configure -state normal
      puts $fid [$P4Widgets(OUTPUT) get 1.0 end]
      $P4Widgets(OUTPUT) configure -state disabled

# cred4 task
    } elseif {[string match $Cred4Task $local_task]} {
      global Cred4Widgets
      $Cred4Widgets(OUTPUT) configure -state normal
      puts $fid [$Cred4Widgets(OUTPUT) get 1.0 end]
      $Cred4Widgets(OUTPUT) configure -state disabled

# red4 task
    } elseif {[string match $Red4Task $local_task]} {
      global Red4Widgets
      $Red4Widgets(OUTPUT) configure -state normal
      puts $fid [$Red4Widgets(OUTPUT) get 1.0 end]
      $Red4Widgets(OUTPUT) configure -state disabled
    }

# print off the result
    close $fid
    cgs4drInform $taskname "Sending $env(HOME)/$taskname.textpane to the printer"
    exec /usr/bin/lp -c $env(HOME)/$taskname.textpane
    update idletasks
  }

# Destroy the panel and quit
  destroy .cgs4drDialogue
  cgs4drCursor arrow green black
}
