proc red4CleanObs {taskname} {
#+
# Creates a dialog box for red4 action
#-
    global env
    global Red4Widgets

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 Clean Observation" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set top [frame $frame.top]
    set bot [frame $frame.bot]
    pack $top $bot -in $frame -side top

    set Red4Widgets(CO_LAB01) [label $top.l1 -text "Filename"]
    set Red4Widgets(CO_ENT01) [entry $top.e1 -width 40]
    pack $Red4Widgets(CO_LAB01) $Red4Widgets(CO_ENT01) -in $top -side left
    $Red4Widgets(CO_ENT01) insert end $Red4Widgets(RO)

    set Red4Widgets(CO_LAB02) [label $bot.l1 -text "Min S/N"]
    set Red4Widgets(CO_ENT02) [entry $bot.sn]
    set Red4Widgets(CO_LAB03) [label $bot.l2 -text "Min Value"]
    set Red4Widgets(CO_ENT03) [entry $bot.dv]
    pack $Red4Widgets(CO_LAB02) $Red4Widgets(CO_ENT02) -in $bot -side left -pady 2m
    pack $Red4Widgets(CO_ENT03) $Red4Widgets(CO_LAB03) -in $bot -side right -pady 2m
    $Red4Widgets(CO_ENT02) insert end $Red4Widgets(DMS)
    $Red4Widgets(CO_ENT03) insert end $Red4Widgets(DMV)

# Bind the defaults button
    bind $Red4Widgets(CO_LAB01) <Button-2> "red4Update red4CleanObs ALL"
    bind $Red4Widgets(CO_LAB02) <Button-2> "red4Update red4CleanObs ALL"
    bind $Red4Widgets(CO_LAB03) <Button-2> "red4Update red4CleanObs ALL"
    bind $Red4Widgets(CO_ENT01) <Button-2> "red4Update red4CleanObs CO_ENT01"
    bind $Red4Widgets(CO_ENT02) <Button-2> "red4Update red4CleanObs CO_ENT02"
    bind $Red4Widgets(CO_ENT03) <Button-2> "red4Update red4CleanObs CO_ENT03"
    bind $Red4Widgets(CO_ENT01) <Double-Button-2> "$Red4Widgets(CO_ENT01) delete 0 end"
    bind $Red4Widgets(CO_ENT02) <Double-Button-2> "$Red4Widgets(CO_ENT02) delete 0 end"
    bind $Red4Widgets(CO_ENT03) <Double-Button-2> "$Red4Widgets(CO_ENT03) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set Red4Widgets(RO) [string trim [$Red4Widgets(CO_ENT01) get]]
      set Red4Widgets(DMS) [string trim [$Red4Widgets(CO_ENT02) get]]
      set Red4Widgets(DMV) [string trim [$Red4Widgets(CO_ENT03) get]]
      if {$Red4Widgets(RO)=="" || $Red4Widgets(RO)==$Red4Widgets(DRO)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4CleanObs error : A dataset has not been specified properly!"
      } else {

# Remove observation
        $taskname obey clean_obs "data=$Red4Widgets(RO) cut=$Red4Widgets(DMS) tlow=$Red4Widgets(DMV)" \
          -inform "cgs4drInform $taskname %V"
      }
    }

# Destroy the box
    destroy .red4Dialogue
    cgs4drCursor arrow green black
}
