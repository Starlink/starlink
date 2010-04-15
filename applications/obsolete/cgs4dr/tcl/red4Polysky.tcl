proc red4Polysky {taskname} {

# Get some global values
    global Red4Widgets
    global cgs4drHtml

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 Polysky" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Bottom-Right contains Enhanced Sky Subtraction Parameters
    set lev(1) [frame $frame.l1]
    set lev(2) [frame $frame.l2]
    set lev(3) [frame $frame.l3]
    set lev(4) [frame $frame.l4]
    set lev(5) [frame $frame.l5]
    set lev(6) [frame $frame.l6]
    set lev(7) [frame $frame.l7]
    pack $lev(1) $lev(2) $lev(3) $lev(4) $lev(5) $lev(6) $lev(7) -in $frame

    set l1 [label $lev(1).l1 -text "Filename"]
    set Red4Widgets(PF_INPUT) [entry $lev(1).dg -width 40]
    pack $l1 $Red4Widgets(PF_INPUT) -in $lev(1) -side left
    $Red4Widgets(PF_INPUT) delete 0 end
    $Red4Widgets(PF_INPUT) insert end $Red4Widgets(RG)
    bind $l1 <Button-2> "red4Update red4Polysky ALL"
    bind $l1 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
    bind $Red4Widgets(PF_INPUT) <Button-2> "red4Update red4Polysky PF_INPUT"
    bind $Red4Widgets(PF_INPUT) <Double-Button-2> "$Red4Widgets(PF_INPUT) delete 0 end"
    bind $Red4Widgets(PF_INPUT) <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"

    set pf2 [radiobutton $lev(2).f2 -text "RGs" -variable Red4Widgets(PF_POLYFIT) -value "REDUCED_GRP"]
    set pf3 [radiobutton $lev(2).f3 -text "Obj" -variable Red4Widgets(PF_POLYFIT) -value "OBJECT"]
    set pf4 [radiobutton $lev(2).f4 -text "Obj-Sky" -variable Red4Widgets(PF_POLYFIT) -value "OBJ-SKY"]
    pack $pf2 $pf3 $pf4 -in $lev(2) -side left
    bind $pf2 <Button-2> "red4Update red4Polysky PF_POLYFIT"
    bind $pf3 <Button-2> "red4Update red4Polysky PF_POLYFIT"
    bind $pf4 <Button-2> "red4Update red4Polysky PF_POLYFIT"
    bind $pf2 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
    bind $pf3 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
    bind $pf4 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"

    set l1 [label $lev(3).l1 -text "Degree"]
    set Red4Widgets(PF_DEGREE) [entry $lev(3).dg -width 15]
    set l2 [label $lev(3).l2 -text "Nreject"]
    set Red4Widgets(PF_NREJECT) [entry $lev(3).nr -width 15]
    set we [checkbutton $lev(3).we -text "Weight" -variable Red4Widgets(PF_WEIGHT)]
    set l3 [label $lev(3).l3 -text " "]
    pack $l1 $Red4Widgets(PF_DEGREE) $l2 $Red4Widgets(PF_NREJECT) -in $lev(3) -side left
    pack $we $l3 -in $lev(3) -side right
    bind $l1 <Button-2> "red4Update red4Polysky ALL"
    bind $l2 <Button-2> "red4Update red4Polysky ALL"
    bind $l3 <Button-2> "red4Update red4Polysky ALL"
    bind $l1 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
    bind $l2 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
    bind $l3 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
    bind $Red4Widgets(PF_DEGREE) <Button-2> "red4Update red4Polysky PF_DEGREE"
    bind $Red4Widgets(PF_DEGREE) <Double-Button-2> "$Red4Widgets(PF_DEGREE) delete 0 end"
    bind $Red4Widgets(PF_DEGREE) <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
    bind $Red4Widgets(PF_NREJECT) <Button-2> "red4Update red4Polysky PF_NREJECT"
    bind $Red4Widgets(PF_NREJECT) <Double-Button-2> "$Red4Widgets(PF_NREJECT) delete 0 end"
    bind $Red4Widgets(PF_NREJECT) <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
    bind $we <Button-2> "red4Update red4Polysky PF_WEIGHT"
    bind $we <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
    $Red4Widgets(PF_DEGREE) delete 0 end
    $Red4Widgets(PF_DEGREE) insert end $Red4Widgets(PF_DDEG)
    $Red4Widgets(PF_NREJECT) delete 0 end
    $Red4Widgets(PF_NREJECT) insert end $Red4Widgets(PF_DNRJ)

# Do sky areas in loop
    set skn 4
    while {$skn <=7} {
      set skynum [expr $skn - 3]
      set l1 [label $lev($skn).l1 -text "Start"]
      set l2 [label $lev($skn).l2 -text "End"]
      set l3 [label $lev($skn).l3 -text "Sky Area $skynum" -width 15]
      set l4 [label $lev($skn).l4 -text " "]
      set Red4Widgets(PF_SAYS$skynum) [entry $lev($skn).s$skn]
      set Red4Widgets(PF_SAYE$skynum) [entry $lev($skn).e$skn]
      pack $l3 $l4 -in $lev($skn) -side left
      pack $Red4Widgets(PF_SAYE$skynum) $l2 $Red4Widgets(PF_SAYS$skynum) $l1 -in $lev($skn) -side right
      bind $l1 <Button-2> "red4Update red4Polysky ALL"
      bind $l1 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
      bind $l2 <Button-2> "red4Update red4Polysky ALL"
      bind $l2 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
      bind $l3 <Button-2> "red4Update red4Polysky ALL"
      bind $l3 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
      bind $l4 <Button-2> "red4Update red4Polysky ALL"
      bind $l4 <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
      bind $Red4Widgets(PF_SAYS$skynum) <Button-2> "red4Update red4Polysky PF_SAYS$skynum"
      bind $Red4Widgets(PF_SAYS$skynum) <Double-Button-2> "$Red4Widgets(PF_SAYS$skynum) delete 0 end"
      bind $Red4Widgets(PF_SAYS$skynum) <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
      bind $Red4Widgets(PF_SAYE$skynum) <Button-2> "red4Update red4Polysky PF_SAYE$skynum"
      bind $Red4Widgets(PF_SAYE$skynum) <Double-Button-2> "$Red4Widgets(PF_SAYE$skynum) delete 0 end"
      bind $Red4Widgets(PF_SAYE$skynum) <Button-3> "red4HelpDialog .helpDialog $cgs4drHtml/red4PolyskyBox1.html"
      if {$skynum == 1} {
        $Red4Widgets(PF_SAYS$skynum) delete 0 end
        $Red4Widgets(PF_SAYS$skynum) insert end $Red4Widgets(PF_DYS1)
        $Red4Widgets(PF_SAYE$skynum) delete 0 end
        $Red4Widgets(PF_SAYE$skynum) insert end $Red4Widgets(PF_DYE1)
      } elseif {$skynum == 2} {
        $Red4Widgets(PF_SAYS$skynum) delete 0 end
        $Red4Widgets(PF_SAYS$skynum) insert end $Red4Widgets(PF_DYS2)
        $Red4Widgets(PF_SAYE$skynum) delete 0 end
        $Red4Widgets(PF_SAYE$skynum) insert end $Red4Widgets(PF_DYE2)
      } elseif {$skynum == 3} {
        $Red4Widgets(PF_SAYS$skynum) delete 0 end
        $Red4Widgets(PF_SAYS$skynum) insert end $Red4Widgets(PF_DYS3)
        $Red4Widgets(PF_SAYE$skynum) delete 0 end
        $Red4Widgets(PF_SAYE$skynum) insert end $Red4Widgets(PF_DYE3)
      } else {
        $Red4Widgets(PF_SAYS$skynum) delete 0 end
        $Red4Widgets(PF_SAYS$skynum) insert end $Red4Widgets(PF_DYS4)
        $Red4Widgets(PF_SAYE$skynum) delete 0 end
        $Red4Widgets(PF_SAYE$skynum) insert end $Red4Widgets(PF_DYE4)
      }
      incr skn
    }

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white

#   Get dataset and check it's OK
      set Red4Widgets(RG) [string trim [$Red4Widgets(PF_INPUT) get]]
      if {$Red4Widgets(RG)=="" || $Red4Widgets(RG)==$Red4Widgets(DRG)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Polysky error : A dataset has not been specified properly!"
      } else {
        set output $Red4Widgets(RG)_psky

#     Set the enhanced sky subtraction parameters
        set polyfit $Red4Widgets(PF_POLYFIT)
        set weight  $Red4Widgets(PF_WEIGHT)
        set Red4Widgets(PF_DDEG) [string trim [$Red4Widgets(PF_DEGREE) get]]
        set Red4Widgets(PF_DNRJ) [string trim [$Red4Widgets(PF_NREJECT) get]]
        set Red4Widgets(PF_DYS1) [string trim [$Red4Widgets(PF_SAYS1) get]]
        set Red4Widgets(PF_DYE1) [string trim [$Red4Widgets(PF_SAYE1) get]]
        set Red4Widgets(PF_DYS2) [string trim [$Red4Widgets(PF_SAYS2) get]]
        set Red4Widgets(PF_DYE2) [string trim [$Red4Widgets(PF_SAYE2) get]]
        set Red4Widgets(PF_DYS3) [string trim [$Red4Widgets(PF_SAYS3) get]]
        set Red4Widgets(PF_DYE3) [string trim [$Red4Widgets(PF_SAYE3) get]]
        set Red4Widgets(PF_DYS4) [string trim [$Red4Widgets(PF_SAYS4) get]]
        set Red4Widgets(PF_DYE4) [string trim [$Red4Widgets(PF_SAYE4) get]]

#    Do the action
        cgs4drInform $taskname "Polysky-ing $Red4Widgets(RG) into $output"
        if {$weight == 1} {
          set param "INPUT=$Red4Widgets(RG) OUTPUT=$output PF_POLYFIT=$polyfit PF_WEIGHT='TRUE'"
        } else {
          set param "INPUT=$Red4Widgets(RG) OUTPUT=$output PF_POLYFIT=$polyfit PF_WEIGHT='FALSE'"
        }
        set param "$param PF_DEGREE=$Red4Widgets(PF_DDEG) PF_NREJECT=$Red4Widgets(PF_DNRJ)"
        set param "$param PF_SAYS1=$Red4Widgets(PF_DYS1) PF_SAYE1=$Red4Widgets(PF_DYE1)"
        set param "$param PF_SAYS2=$Red4Widgets(PF_DYS2) PF_SAYE2=$Red4Widgets(PF_DYE2)"
        set param "$param PF_SAYS3=$Red4Widgets(PF_DYS3) PF_SAYE3=$Red4Widgets(PF_DYE3)"
        set param "$param PF_SAYS4=$Red4Widgets(PF_DYS4) PF_SAYE4=$Red4Widgets(PF_DYE4)"
        $taskname obey polyfit "$param" -inform "cgs4drInform $taskname %V"
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
