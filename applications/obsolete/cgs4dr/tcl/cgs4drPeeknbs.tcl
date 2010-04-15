proc cgs4drPeeknbs {taskname} {
#+
# Lists noticeboard.
#-
    global Cred4NoticeBoard
    global C4UserNb
    global Cred4Task
    global P4NoticeBoard
    global P4UserNb
    global P4Task
    global cgs4drHtml

# Set a default depending upon task
    if {[string match $Cred4Task $taskname]} {
      set noticeboard $C4UserNb
    } elseif {[string match $P4Task $taskname]} {
      set noticeboard $P4UserNb
    } else {
      set noticeboard ""
    }

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

# Set some bindings
    if {[string match $Cred4Task $taskname]} {
      bind $ilab <Button-2> "$inbs delete 0 end; $inbs insert 0 $Cred4NoticeBoard"
      bind $inbs <Button-2> "$inbs delete 0 end; $inbs insert 0 $Cred4NoticeBoard"
      bind $inbs <Double-Button-2> "$inbs delete 0 end"
      set noticeboard $C4UserNb
    } elseif {[string match $P4Task $taskname]} {
      bind $ilab <Button-2> "$inbs delete 0 end; $inbs insert 0 $P4NoticeBoard"
      bind $inbs <Button-2> "$inbs delete 0 end; $inbs insert 0 $P4NoticeBoard"
      bind $inbs <Double-Button-2> "$inbs delete 0 end"
      set noticeboard $P4UserNb
    } else {
      bind $ilab <Button-2> "$inbs delete 0 end"
      bind $inbs <Button-2> "$inbs delete 0 end"
      bind $inbs <Double-Button-2> "$inbs delete 0 end"
      set noticeboard ""
    }
    bind $ilab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4PeekNbsBox1.html"
    bind $inbs <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4PeekNbsBox1.html"

# If user presses OK, send it to the task
    set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set item [string trim [$inbs get]]
      if {$item==""} {
        cgs4drClear $taskname
        cgs4drInform $taskname "cgs4drPeeknbs error : Noticeboard or value incorrectly specified!"
      } else {

#     Set a default depending upon task and list nbs
        if {[string match $Cred4Task $taskname]} {
          set C4UserNb $item
        } elseif {[string match $P4Task $taskname]} {
          set P4UserNb $item
        }
        cgs4drListnbs $taskname $item
      }
    }

# Destroy the dialog box
    cgs4drCursor arrow green black
    destroy .cgs4drDialogue
}
