proc cgs4drCheckTask {task} {
#+
# Checks NBS for task busy signal
#-
   global Cred4NoticeBoard

# If variable doesn't exist, assume it wasn't created and exit
   if {[info exists Cred4NoticeBoard]==0} {return 0}

# Check to see if task is busy
   set status [catch {set nbv [nbs get ${Cred4NoticeBoard}.flags.${task}_busy]}]
   if {$status==0} {

#   If zero, red4 is not busy so return
     if {$nbv==0} {
       return 0

#   Else it is busy so prompt user
     } else {
       if {[winfo exists .cgs4drDialogue]} {destroy .cgs4drDialogue}
       set frame [dialogStart .cgs4drDialogue "Task Busy" 0 OK Cancel]
       cgs4drCursor pirate orange black
       .cgs4drDialogue config -cursor {arrow green black}

       set label1 [label $frame.lab1 -text "The ${task} task is busy." -width 60]
       set label2 [label $frame.lab2 -text "Continue?" -width 60]
       pack $label1 $label2 -in $frame -side top

       set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
       destroy .cgs4drDialogue
       cgs4drCursor arrow green black
       return $bv
     }

# Failed to get nbs value
   } else {
     if {[winfo exists .cgs4drDialogue]} {destroy .cgs4drDialogue}
     set frame [dialogStart .cgs4drDialogue "NBS Not Available" error Dismiss]
     cgs4drCursor pirate orange black
     .cgs4drDialogue config -cursor {arrow green black}

     set label1 [label $frame.lab1 -text "Failed to get NBS value!" -width 60]
     pack $label1 -in $frame -side top

     set bv [dialogShow .cgs4drDialogue .cgs4drDialogue]
     destroy .cgs4drDialogue
     cgs4drCursor arrow green black
     return 1
   }
}
