proc red4Int {taskname action} {
#+
# Creates a dialog box for red4 action
#-
    global env
    global Red4Widgets
    global cgs4drHtml

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    if {$action=="add"} {
      set frame [dialogStart .red4Dialogue "Red4 Add Integration to Observation" 0 OK Cancel]
    } elseif {$action=="delete" || $action=="remove"} {
      set frame [dialogStart .red4Dialogue "Red4 Delete Integration from Observation" 0 OK Cancel]
    }
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set Red4Widgets(AI_LABEL) [label $frame.lb -text "Filename"]
    set Red4Widgets(AI_ENTRY) [entry $frame.en -width 40]
    pack $Red4Widgets(AI_LABEL) $Red4Widgets(AI_ENTRY) -in $frame -side left
    $Red4Widgets(AI_ENTRY) insert end $Red4Widgets(IN)

#  Bind defaults key as help has already been done
    bind $Red4Widgets(AI_LABEL) <Button-2> "red4Update red4Int ALL"
    bind $Red4Widgets(AI_ENTRY) <Button-2> "red4Update red4Int ALL"
    bind $Red4Widgets(AI_ENTRY) <Double-Button-2> "$Red4Widgets(AI_ENTRY) delete 0 end"
    if {$action=="add"} {
      bind $Red4Widgets(AI_LABEL) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IntBox1.html"
      bind $Red4Widgets(AI_ENTRY) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IntBox1.html"
    } elseif {$action=="delete"} {
      bind $Red4Widgets(AI_LABEL) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IntBox2.html"
      bind $Red4Widgets(AI_ENTRY) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/red4IntBox2.html"
    }

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set int [string trim [$Red4Widgets(AI_ENTRY) get]]
      if {$int=="" || $int==$Red4Widgets(DIN)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Int error : A dataset has not been specified properly!"
      } else {

# Add or remove integration
        set Red4Widgets(IN) $int
        if {$action=="add"} {
          $taskname obey add_int "intfile=$int" -inform "cgs4drInform $taskname %V"
        } elseif {$action=="delete"} {
          $taskname obey remove_int "intfile=$int" -inform "cgs4drInform $taskname %V"
        } elseif {$action=="remove"} {
          $taskname obey remove_int "intfile=$int" -inform "cgs4drInform $taskname %V"
        }
      }
    }

# Destroy the box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
