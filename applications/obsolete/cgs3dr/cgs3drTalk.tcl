proc cgs3drTalk {} {
  global FigTask
  global Red3Task
  global Cgs3drTask
  global TspTask
  global Cgs3drWidgets

# Create a dialog box
  if {[winfo exists .cgs3drDialogue]} {destroy .cgs3drDialogue}
  set frame [dialogStart .cgs3drDialogue "Task Communications" 0 OK Cancel]
  cgs3drCursor pirate orange black
  .cgs3drDialogue config -cursor {arrow green black}

  set top [frame $frame.top -relief sunken -bd 2]
  set mid [frame $frame.mid -relief sunken -bd 2]
  set bot [frame $frame.bot -relief sunken -bd 2]
  pack $top $mid $bot -fill both -expand yes

  set label [label $top.label -text Task]
  set r1 [radiobutton $top.r1 -text Figaro -variable Cgs3drWidgets(TASK_NAME) -value $FigTask]
  set r2 [radiobutton $top.r2 -text Red3 -variable Cgs3drWidgets(TASK_NAME) -value $Red3Task]
  set r3 [radiobutton $top.r3 -text Cgs3dr -variable Cgs3drWidgets(TASK_NAME) -value $Cgs3drTask]
  set r4 [radiobutton $top.r4 -text Tsp -variable Cgs3drWidgets(TASK_NAME) -value $TspTask]
  pack $label -side left -padx 2 -pady 2
  pack $r4 $r3 $r2 $r1 -side right -padx 2 -pady 2

  set label [label $mid.label -text Action]
  set action [entry $mid.action -width 50 -relief sunken -bd 2 -textvariable Cgs3drWidgets(TASK_ACTION)]
  pack $label -side left -padx 2 -pady 2
  pack $action -side right -padx 2 -pady 2

  set label [label $bot.label -text Parameters]
  set params [entry $bot.params -width 50 -relief sunken -bd 2 -textvariable Cgs3drWidgets(TASK_PARAMS)]
  pack $label -side left -padx 2 -pady 2
  pack $params -side right -padx 2 -pady 2

# If user presses OK, send it to the task
  set bv [dialogShow .cgs3drDialogue .cgs3drDialogue]
  if {$bv==0} {
    cgs3drClear
    cgs3drCursor watch red white
    set actnam [string trim [$action get]]
    set parnam [string trim [$params get]]
    if {$actnam!=""} {
      $Cgs3drWidgets(TASK_NAME) obey $actnam "$parnam" -inform "cgs3drInform %V"
    } else {
      cgs3drInform "cgs3drTalk error : Action name not specified!"
    }
  }

# Destroy the dialog box
  cgs3drCursor arrow green black
  destroy .cgs3drDialogue
}
