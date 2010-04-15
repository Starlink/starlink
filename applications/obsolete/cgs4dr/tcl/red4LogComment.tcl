proc red4LogComment {taskname} {
#+
# Creates a dialog box for red4 action
#-
    global Red4Widgets

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 Log Comment" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set Red4Widgets(LC_LABEL) [label $frame.lb -text "Comment"]
    set Red4Widgets(LC_ENTRY) [entry $frame.en -width 60]
    pack $Red4Widgets(LC_LABEL) $Red4Widgets(LC_ENTRY) -in $frame -side left

# Bind the defaults button
    bind $Red4Widgets(LC_LABEL) <Button-2> "red4Update red4LogComment LC_ENTRY"
    bind $Red4Widgets(LC_ENTRY) <Button-2> "red4Update red4LogComment LC_ENTRY"
    bind $Red4Widgets(LC_ENTRY) <Double-Button-2> "$Red4Widgets(LC_ENTRY) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set comment [string trim [$Red4Widgets(LC_ENTRY) get]]
      if {$comment!=""} {
        $taskname obey log_comment "comment='$comment'" -inform "cgs4drInform $taskname %V"
      } else {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4LogComment error : Null comments are not logged!"
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
