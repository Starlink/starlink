proc cgs4drTalk {taskname} {
#+
#  Sends taskname an action
#-
    global cgs4drHtml

# Create a dialog box
    if {[winfo exists .cgs4drDialogue]} {destroy .cgs4drDialogue}
    set frame [dialogStart .cgs4drDialogue "Task Communications to $taskname" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cgs4drDialogue config -cursor {arrow green black}

    set top [frame $frame.top]
    set bot [frame $frame.bot]
    pack $top $bot -side top -fill both -expand yes

    set actlab [label $top.actlab -text Action]
    set action [entry $top.action -width 50 -relief sunken -bd 2]
    pack $actlab -in $top -side left -padx 2 -pady 2
    pack $action -in $top -side right -padx 2 -pady 2

    set parlab [label $bot.parlab -text Parameters]
    set param [entry $bot.param -width 50 -relief sunken -bd 2]
    pack $parlab -in $bot -side left -padx 2 -pady 2
    pack $param -in $bot -side right -padx 2 -pady 2

# Bind some actions
    bind $actlab <Button-2> "$action delete 0 end; $param delete 0 end"
    bind $action <Button-2> "$action delete 0 end"
    bind $action <Double-Button-2> "$action delete 0 end"
    bind $actlab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drTalk.html"
    bind $action <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drTalk.html"
    bind $parlab <Button-2> "$action delete 0 end; $param delete 0 end"
    bind $param  <Button-2> "$param delete 0 end"
    bind $param  <Double-Button-2> "$param delete 0 end"
    bind $parlab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drTalk.html"
    bind $param  <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drTalk.html"

# If user presses OK, send it to the task
    set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set actnam [string trim [$action get]]
      set parnam [string trim [$param get]]
      if {$parnam == ""} {
        $taskname obey $actnam "" -inform "cgs4drInform $taskname %V"
      } else {
        $taskname obey $actnam "$parnam" -inform "cgs4drInform $taskname %V"
      }
    }

# Destroy the dialog box
    cgs4drCursor arrow green black
    destroy .cgs4drDialogue
}
