proc red4SubPair {taskname pair} {
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
    if {$pair==1} {
      set frame [dialogStart .red4Dialogue "Red4 Subtract Observation from Group" 0 OK Cancel]
    } elseif {$pair==2} {
      set frame [dialogStart .red4Dialogue "Red4 Subtract Pair from Group" 0 OK Cancel]
    }
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set top [frame $frame.top]
    set bot [frame $frame.bot]
    pack $top $bot -in $frame -side top

    if {$pair==1} {
      set Red4Widgets(SP_LAB01) [label $top.l1 -text "Filename"]
      set Red4Widgets(SP_ENT01) [entry $top.e1 -width 40]
      pack $Red4Widgets(SP_LAB01) $Red4Widgets(SP_ENT01) -in $top -side left
      $Red4Widgets(SP_ENT01) insert end $Red4Widgets(OB)
    } elseif {$pair==2} {
      set Red4Widgets(SP_LAB01) [label $top.l1 -text "Object" -width 20]
      set Red4Widgets(SP_ENT01) [entry $top.e1 -width 40]
      set Red4Widgets(SP_LAB02) [label $bot.l2 -text "Sky" -width 20]
      set Red4Widgets(SP_ENT02) [entry $bot.e2 -width 40]
      pack $Red4Widgets(SP_LAB01) $Red4Widgets(SP_ENT01) -in $top -side left
      pack $Red4Widgets(SP_LAB02) $Red4Widgets(SP_ENT02) -in $bot -side left
      $Red4Widgets(SP_ENT01) insert end $Red4Widgets(OB)
      $Red4Widgets(SP_ENT02) insert end $Red4Widgets(OB)
    }

# Bind the defaults to button-2
   if {$pair == 1} {
     bind $Red4Widgets(SP_LAB01) <Button-2> "red4Update red4SubPair1 SP_ENT01"
     bind $Red4Widgets(SP_ENT01) <Button-2> "red4Update red4SubPair1 SP_ENT01"
     bind $Red4Widgets(SP_ENT01) <Double-Button-2> "$Red4Widgets(SP_ENT01) delete 0 end"
   } elseif {$pair == 2} {
     bind $Red4Widgets(SP_LAB01) <Button-2> "red4Update red4SubPair2 ALL"
     bind $Red4Widgets(SP_LAB02) <Button-2> "red4Update red4SubPair2 ALL"
     bind $Red4Widgets(SP_ENT01) <Button-2> "red4Update red4SubPair2 SP_ENT01"
     bind $Red4Widgets(SP_ENT02) <Button-2> "red4Update red4SubPair2 SP_ENT02"
     bind $Red4Widgets(SP_ENT01) <Double-Button-2> "$Red4Widgets(SP_ENT01) delete 0 end"
     bind $Red4Widgets(SP_ENT02) <Double-Button-2> "$Red4Widgets(SP_ENT02) delete 0 end"
   }

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set sky "somecompletepieceofgarbage"
      set obj [string trim [$Red4Widgets(SP_ENT01) get]]
      if {$pair==2} {set sky [string trim [$Red4Widgets(SP_ENT02) get]]}
      if {$obj=="" || $obj==$Red4Widgets(DOB) || $sky=="" || $sky==$Red4Widgets(DOB)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4SubPair error : A dataset has not been specified properly!"
      } else {

# Remove observation or pair
        set Red4Widgets(OB) $obj
        if {$pair==1} {
          $taskname obey remove_obs "obsfile=$obj" -inform "cgs4drInform $taskname %V"
        } elseif {$pair==2} {
          $taskname obey remove_pair "objectobs=$obj skyobs=$sky" -inform "cgs4drInform $taskname %V"
        }
      }
    }

# Destroy the box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
