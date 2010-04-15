proc red4Stats {taskname} {
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
    set frame [dialogStart .red4Dialogue "Red4 Statistics" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set lev1 [frame $frame.lev1]
    set lev2 [frame $frame.lev2]
    set lev3 [frame $frame.lev3]
    pack $lev1 $lev2 $lev3 -in $frame -side top

    set Red4Widgets(GS_LAB01) [label $lev1.l1 -text "Filename"]
    set Red4Widgets(GS_ENT01) [entry $lev1.e1 -width 40]
    pack $Red4Widgets(GS_LAB01) $Red4Widgets(GS_ENT01) -in $lev1 -side left
    $Red4Widgets(GS_ENT01) insert end $Red4Widgets(RO)
    bind $Red4Widgets(GS_LAB01) <Button-2> "red4Update red4Stats ALL"
    bind $Red4Widgets(GS_ENT01) <Button-2> "red4Update red4Stats GS_ENT01"
    bind $Red4Widgets(GS_ENT01) <Double-Button-2> "$Red4Widgets(GS_ENT01) delete 0 end"

    set Red4Widgets(GS_LAB02) [label $lev2.l2 -text "Plane"]
    set dp [radiobutton $lev2.dp -text "Data" -value DATA -variable Red4Widgets(GS_PLANE)]
    set ep [radiobutton $lev2.ep -text "Error" -value ERRORS -variable Red4Widgets(GS_PLANE)]
    set qp [radiobutton $lev2.qp -text "Quality" -value QUALITY -variable Red4Widgets(GS_PLANE)]
    pack $Red4Widgets(GS_LAB02) $dp $ep $qp -in $lev2 -side left -padx 1 -pady 1
    bind $Red4Widgets(GS_LAB02) <Button-2> "red4Update red4Stats ALL"
    bind $dp <Button-2> "red4Update red4Stats GS_PLANE"
    bind $ep <Button-2> "red4Update red4Stats GS_PLANE"
    bind $qp <Button-2> "red4Update red4Stats GS_PLANE"

    set wa [checkbutton $lev2.wa -variable Red4Widgets(GS_WHOLE) -text "Use Whole Array"]
    set as [checkbutton $lev2.as -variable Red4Widgets(GS_AUTOSCALE) -text "Autoscale Data"]
    pack $wa $as -in $lev2 -side right -padx 1 -pady 1
    bind $wa <Button-2> "red4Update red4Stats GS_WHOLE"
    bind $as <Button-2> "red4Update red4Stats GS_AUTOSCALE"

    set flip [frame $lev3.fl]
    pack $flip -in $lev3 -fill x -expand y

    set limits1 [frame $flip.al]
    set isl [label $limits1.isl -text "Istart"]
    set Red4Widgets(GS_ENT02)  [entry $limits1.is]
    set iel [label $limits1.iel -text "Iend"]
    set Red4Widgets(GS_ENT03)  [entry $limits1.ie]
    set itl [label $limits1.itl -text "Istep"]
    set Red4Widgets(GS_ENT04)  [entry $limits1.it]
    set jsl [label $limits1.jsl -text "Jstart"]
    set Red4Widgets(GS_ENT05)  [entry $limits1.js]
    set jel [label $limits1.jel -text "Jend"]
    set Red4Widgets(GS_ENT06)  [entry $limits1.je]
    set jtl [label $limits1.jtl -text "Jstep"]
    set Red4Widgets(GS_ENT07)  [entry $limits1.jt]
    $Red4Widgets(GS_ENT02) insert end $Red4Widgets(GS_DIST)
    $Red4Widgets(GS_ENT03) insert end $Red4Widgets(GS_DIEN)
    $Red4Widgets(GS_ENT04) insert end $Red4Widgets(GS_DIIN)
    $Red4Widgets(GS_ENT05) insert end $Red4Widgets(GS_DJST)
    $Red4Widgets(GS_ENT06) insert end $Red4Widgets(GS_DJEN)
    $Red4Widgets(GS_ENT07) insert end $Red4Widgets(GS_DJIN)
    pack $isl $Red4Widgets(GS_ENT02) $iel $Red4Widgets(GS_ENT03) $itl $Red4Widgets(GS_ENT04) $jsl $Red4Widgets(GS_ENT05) \
      $jel $Red4Widgets(GS_ENT06) $jtl $Red4Widgets(GS_ENT07) -side left -padx 2m
    trace variable Red4Widgets(GS_WHOLE) w "RedFlipOut $limits1"

    set limits2 [frame $flip.sl]
    set hl [label $limits2.hl -text High]
    set Red4Widgets(GS_ENT08) [entry $limits2.hi]
    set ll [label $limits2.ll -text Low]
    set Red4Widgets(GS_ENT09) [entry $limits2.lo]
    pack $ll $Red4Widgets(GS_ENT08) $hl $Red4Widgets(GS_ENT09) -side left -padx 1 -pady 1
    $Red4Widgets(GS_ENT08) insert end $Red4Widgets(GS_DHI)
    $Red4Widgets(GS_ENT09) insert end $Red4Widgets(GS_DLO)
    trace variable Red4Widgets(GS_AUTOSCALE) w "RedFlipOut $limits2"

    bind $Red4Widgets(GS_ENT02) <Button-2> "red4Update red4Stats GS_ENT02"
    bind $Red4Widgets(GS_ENT02) <Double-Button-2> "$Red4Widgets(GS_ENT02) delete 0 end"
    bind $Red4Widgets(GS_ENT03) <Button-2> "red4Update red4Stats GS_ENT03"
    bind $Red4Widgets(GS_ENT03) <Double-Button-2> "$Red4Widgets(GS_ENT03) delete 0 end"
    bind $Red4Widgets(GS_ENT04) <Button-2> "red4Update red4Stats GS_ENT04"
    bind $Red4Widgets(GS_ENT04) <Double-Button-2> "$Red4Widgets(GS_ENT04) delete 0 end"
    bind $Red4Widgets(GS_ENT05) <Button-2> "red4Update red4Stats GS_ENT05"
    bind $Red4Widgets(GS_ENT05) <Double-Button-2> "$Red4Widgets(GS_ENT05) delete 0 end"
    bind $Red4Widgets(GS_ENT06) <Button-2> "red4Update red4Stats GS_ENT06"
    bind $Red4Widgets(GS_ENT06) <Double-Button-2> "$Red4Widgets(GS_ENT06) delete 0 end"
    bind $Red4Widgets(GS_ENT07) <Button-2> "red4Update red4Stats GS_ENT07"
    bind $Red4Widgets(GS_ENT07) <Double-Button-2> "$Red4Widgets(GS_ENT07) delete 0 end"
    bind $Red4Widgets(GS_ENT08) <Button-2> "red4Update red4Stats GS_ENT08"
    bind $Red4Widgets(GS_ENT08) <Double-Button-2> "$Red4Widgets(GS_ENT08) delete 0 end"
    bind $Red4Widgets(GS_ENT09) <Button-2> "red4Update red4Stats GS_ENT09"
    bind $Red4Widgets(GS_ENT09) <Double-Button-2> "$Red4Widgets(GS_ENT09) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set Red4Widgets(RO) [string trim [$Red4Widgets(GS_ENT01) get]]
      if {$Red4Widgets(RO)=="" || $Red4Widgets(RO)==$Red4Widgets(DRO)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Stats error : A dataset has not been specified properly!"
      } else {

# Do the stats
        set param "data='[string trim $Red4Widgets(RO)]' plane='[string trim $Red4Widgets(GS_PLANE)]'"
        if {$Red4Widgets(GS_WHOLE) == 1} {
          set Red4Widgets(GS_DIST) 1
          set Red4Widgets(GS_DIEN) 256
          set Red4Widgets(GS_DIIN) 1
          set Red4Widgets(GS_DJST) 1
          set Red4Widgets(GS_DJEN) 256
          set Red4Widgets(GS_DJIN) 1
          set param "${param} whole='T'"
        } else {
          set Red4Widgets(GS_DIST) [string trim [$Red4Widgets(GS_ENT02) get]]
          set Red4Widgets(GS_DIEN) [string trim [$Red4Widgets(GS_ENT03) get]]
          set Red4Widgets(GS_DIIN) [string trim [$Red4Widgets(GS_ENT04) get]]
          set Red4Widgets(GS_DJST) [string trim [$Red4Widgets(GS_ENT05) get]]
          set Red4Widgets(GS_DJEN) [string trim [$Red4Widgets(GS_ENT06) get]]
          set Red4Widgets(GS_DJIN) [string trim [$Red4Widgets(GS_ENT07) get]]
          set param "${param} whole='F' istart=$Red4Widgets(GS_DIST) iend=$Red4Widgets(GS_DIEN)"
          set param "$param iincr=$Red4Widgets(GS_DIIN) jstart=$Red4Widgets(GS_DJST)"
          set param "$param jend=$Red4Widgets(GS_DJEN) jincr=$Red4Widgets(GS_DJIN)"
        }
        if {$Red4Widgets(GS_AUTOSCALE) == 1} {
          set Red4Widgets(GS_DHI) 1000.0
          set Red4Widgets(GS_DLO) 0.0
          set param "${param} autoscale='T'"
        } else {
          set Red4Widgets(GS_DHI) [string trim [$Red4Widgets(GS_ENT08) get]]
          set Red4Widgets(GS_DLO) [string trim [$Red4Widgets(GS_ENT09) get]]
          set param "${param} autoscale='F' high=$Red4Widhets(GS_DHI) low=$Red4Widhets(GS_DLO)"
        }
        set param "${param} mean=0.0 sigma=0.0 median=0.0 mode=0.0"
        $taskname obey stats "${param}" -inform "cgs4drInform $taskname %V"
      }
    }

# Destroy the box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}

proc RedFlipOut {w name el op} {
    global $name
    set val [set ${name}($el)]
    if {$val==0} {
	pack $w -fill x
    } {
	pack forget $w
    }
}
