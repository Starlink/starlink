proc red4NormObs {taskname} {
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
    set frame [dialogStart .red4Dialogue "Red4 Normalise Observation" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set top [frame $frame.top]
    set bot [frame $frame.bot]
    pack $top $bot -in $frame -side top

    set Red4Widgets(NO_LAB01) [label $top.l1 -text "Filename"]
    set Red4Widgets(NO_ENT01) [entry $top.e1 -width 40]
    pack $Red4Widgets(NO_LAB01) $Red4Widgets(NO_ENT01) -in $top -side left
    $Red4Widgets(NO_ENT01) insert end $Red4Widgets(RO)
    bind $Red4Widgets(NO_LAB01) <Button-2> "red4Update red4NormObs ALL"
    bind $Red4Widgets(NO_ENT01) <Button-2> "red4Update red4NormObs NO_ENT01"
    bind $Red4Widgets(NO_ENT01) <Double-Button-2> "$Red4Widgets(NO_ENT01) delete 0 end"

    set pf [radiobutton $bot.pf -text "Polyfit" -variable Red4Widgets(NO_METHOD) -value "POLYFIT"]
    set sm [radiobutton $bot.sm -text "Smooth" -variable Red4Widgets(NO_METHOD) -value "SMOOTH"]
    set Red4Widgets(NO_LAB02) [label $bot.l2 -text "Polynomial"]
    set Red4Widgets(NO_ENT02) [entry $bot.e2]
    set Red4Widgets(NO_LAB03) [label $bot.l3 -text "Boxsize"]
    set Red4Widgets(NO_ENT03) [entry $bot.e3]
    pack $pf $sm $Red4Widgets(NO_LAB02) $Red4Widgets(NO_ENT02) $Red4Widgets(NO_LAB03) \
       $Red4Widgets(NO_ENT03) -in $bot -side left -pady 2m
    set Red4Widgets(NO_METHOD) POLYFIT
    $Red4Widgets(NO_ENT02) insert end $Red4Widgets(NO_DPOL)
    $Red4Widgets(NO_ENT03) insert end $Red4Widgets(NO_DBOX)
    bind $pf <Button-2> "red4Update red4NormObs NO_METHOD"
    bind $sm <Button-2> "red4Update red4NormObs NO_METHOD"
    bind $Red4Widgets(NO_LAB02) <Button-2> "red4Update red4NormObs ALL"
    bind $Red4Widgets(NO_ENT02) <Button-2> "red4Update red4NormObs NO_ENT02"
    bind $Red4Widgets(NO_ENT02) <Double-Button-2> "$Red4Widgets(NO_ENT02) delete 0 end"
    bind $Red4Widgets(NO_LAB03) <Button-2> "red4Update red4NormObs ALL"
    bind $Red4Widgets(NO_ENT03) <Button-2> "red4Update red4NormObs NO_ENT03"
    bind $Red4Widgets(NO_ENT03) <Double-Button-2> "$Red4Widgets(NO_ENT03) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set Red4Widgets(RO) [string trim [$Red4Widgets(NO_ENT01) get]]
      set Red4Widgets(NO_DPOL) [string trim [$Red4Widgets(NO_ENT02) get]]
      set Red4Widgets(NO_DBOX) [string trim [$Red4Widgets(NO_ENT03) get]]
      if {$Red4Widgets(RO)=="" || $Red4Widgets(RO)==$Red4Widgets(DRO)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4NormObs error : A dataset has not been specified properly!"
      } else {

# Remove observation
        set out $Red4Widgets(RO)_ff
        set message "Generating normalised observation from $Red4Widgets(RO) output to $out"
        cgs4drInform $taskname $message
        set param "input=$Red4Widgets(RO) output=$out norm_method=$Red4Widgets(NO_METHOD)"
        set param "$param order=$Red4Widgets(NO_DPOL) boxsize=$Red4Widgets(NO_DBOX)"
        $taskname obey normalise_ff "$param" -inform "cgs4drInform $taskname %V"
      }
    }

# Destroy the box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
