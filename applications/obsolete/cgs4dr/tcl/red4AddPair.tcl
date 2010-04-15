proc red4AddPair {taskname pair} {
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
    if {$pair=="obs"} {
      set frame [dialogStart .red4Dialogue "Red4 Add Observation to Group" 0 OK Cancel]
    } elseif {$pair=="pair"} {
      set frame [dialogStart .red4Dialogue "Red4 Add Pair to Group" 0 OK Cancel]
    }
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set top [frame $frame.top]
    set bot [frame $frame.bot]
    pack $top $bot -in $frame -side top

    set Red4Widgets(AP_LAB01) [label $top.l1 -text "Filename" -width 15]
    set Red4Widgets(AP_ENTRY) [entry $top.e1 -width 40]
    pack $Red4Widgets(AP_LAB01) $Red4Widgets(AP_ENTRY) -in $top -side left
    $Red4Widgets(AP_ENTRY) insert end $Red4Widgets(OB)

    set Red4Widgets(AP_LAB02) [label $bot.l1 -text "Sky Weighting Factor"]
    set Red4Widgets(AP_SKYWT) [entry $bot.sk]
    set Red4Widgets(AP_CHECK) [checkbutton $bot.l2 -text "Variance Weight" -variable Red4Widgets(AP_VARWT)]
    pack $Red4Widgets(AP_CHECK) -in $bot -side right -pady 2m
    pack $Red4Widgets(AP_LAB02) $Red4Widgets(AP_SKYWT) -in $bot -side left -pady 2m
    $Red4Widgets(AP_SKYWT) insert end $Red4Widgets(DSKYWT)

# Create default bindings for MB2
    bind $Red4Widgets(AP_LAB01) <Button-2> "red4Update red4AddPair ALL"
    bind $Red4Widgets(AP_LAB02) <Button-2> "red4Update red4AddPair ALL"
    bind $Red4Widgets(AP_ENTRY) <Button-2> "red4Update red4AddPair AP_ENTRY"
    bind $Red4Widgets(AP_ENTRY) <Double-Button-2> "$Red4Widgets(AP_ENTRY) delete 0 end"
    bind $Red4Widgets(AP_SKYWT) <Button-2> "red4Update red4AddPair AP_SKYWT"
    bind $Red4Widgets(AP_SKYWT) <Double-Button-2> "$Red4Widgets(AP_SKYWT) delete 0 end"
    bind $Red4Widgets(AP_CHECK) <Button-2> "red4Update red4AddPair AP_VARWT"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set obs [string trim [$Red4Widgets(AP_ENTRY) get]]
      set sky [string trim [$Red4Widgets(AP_SKYWT) get]]
      if {$obs=="" || $obs==$Red4Widgets(DOB)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4AddPair error : A dataset has not been specified properly!"
      } else {

# Remove observation
        set Red4Widgets(OB) $obs
        set Red4Widgets(DSKYWT) $sky
        if {$pair=="obs"} {
          $taskname obey add_obs "obsfile=$obs variance_wt=$Red4Widgets(AP_VARWT) sky_wt=$sky" \
            -inform "cgs4drInform $taskname %V"
        } elseif {$pair=="pair"} {
          $taskname obey add_pair "obsfile=$obs variance_wt=$Red4Widgets(AP_VARWT) errors='FROM_OBS' sky_wt=$sky" \
            -inform "cgs4drInform $taskname %V"
        }
      }
    }

# Destroy the box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
