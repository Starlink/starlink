proc cgs4drPeeknbs {taskname noticeboard} {
#+
# Lists noticeboard.
#-
    global cgs4drHtml

# Set the box, cursor etc
    if {[winfo exists .cgs4drDialogue]} {destroy .cgs4drDialogue}
    set frame [dialogStart .cgs4drDialogue "NBS Peek Value" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cgs4drDialogue config -cursor {arrow green black}

# Create a dialog box
    set top [frame $frame.top]
    pack $top -side top -fill both -expand yes
    set ilab [label $top.ilab -text Item]
    set inbs [entry $top.item -width 50]
    pack $ilab $inbs -in $top -side left
    $inbs insert end $noticeboard

    bind $ilab <Button-2> "$inbs delete 0 end; $inbs insert 0 $noticeboard"
    bind $inbs <Button-2> "$inbs delete 0 end; $inbs insert 0 $noticeboard"
    bind $inbs <Double-Button-2> "$inbs delete 0 end"
    bind $ilab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4PeekNbsBox1.html"
    bind $inbs <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4PeekNbsBox1.html"

# If user presses OK, send it to the task
    set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set item [string trim [$inbs get]]
      if {$item==""} {
        cgs4drClear $taskname
        set message "cgs4drPeeknbs error : Noticeboard or value incorrectly specified!"
        cgs4drInform $taskname $message
      } else {
        cgs4drListnbs $taskname $item
      }
    }

# Destroy the dialog box
    cgs4drCursor arrow green black
    destroy .cgs4drDialogue
}
