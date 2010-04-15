proc cred4Polysky {taskname} {

# Get some global values
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Polysky Parameters Setup" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Bottom-Right contains Enhanced Sky Subtraction Parameters
    set lev(2) [frame $frame.l2]
    set lev(3) [frame $frame.l3]
    set lev(4) [frame $frame.l4]
    set lev(5) [frame $frame.l5]
    set lev(6) [frame $frame.l6]
    set lev(7) [frame $frame.l7]
    pack $lev(2) $lev(3) $lev(4) $lev(5) $lev(6) $lev(7) -in $frame

    set pf1 [radiobutton $lev(2).f1 -text "None" -variable Cred4Widgets(PF_POLYFIT) -value "NONE"]
    set pf2 [radiobutton $lev(2).f2 -text "RGs" -variable Cred4Widgets(PF_POLYFIT) -value "REDUCED_GRP"]
    set pf3 [radiobutton $lev(2).f3 -text "Obj" -variable Cred4Widgets(PF_POLYFIT) -value "OBJECT"]
    set pf4 [radiobutton $lev(2).f4 -text "Obj-Sky" -variable Cred4Widgets(PF_POLYFIT) -value "OBJ-SKY"]
    pack $pf1 $pf2 $pf3 $pf4 -in $lev(2) -side left
    bind $pf1 <Button-2> "cred4Update cred4Polysky PF_POLYFIT"
    bind $pf2 <Button-2> "cred4Update cred4Polysky PF_POLYFIT"
    bind $pf3 <Button-2> "cred4Update cred4Polysky PF_POLYFIT"
    bind $pf4 <Button-2> "cred4Update cred4Polysky PF_POLYFIT"
    bind $pf1 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    bind $pf2 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    bind $pf3 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    bind $pf4 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    set Cred4Widgets(PF_POLYFIT) [string trim [string toupper [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_polyfit]]]

    set l1 [label $lev(3).l1 -text "Degree"]
    set Cred4Widgets(PF_DEGREE) [entry $lev(3).dg -width 15]
    set l2 [label $lev(3).l2 -text "Nreject"]
    set Cred4Widgets(PF_NREJECT) [entry $lev(3).nr -width 15]
    set we [checkbutton $lev(3).we -text "Weight" -variable Cred4Widgets(PF_WEIGHT)]
    set l3 [label $lev(3).l3 -text " "]
    pack $l1 $Cred4Widgets(PF_DEGREE) $l2 $Cred4Widgets(PF_NREJECT) -in $lev(3) -side left
    pack $we $l3 -in $lev(3) -side right
    bind $l1 <Button-2> "cred4Update cred4Polysky ALL"
    bind $l2 <Button-2> "cred4Update cred4Polysky ALL"
    bind $l3 <Button-2> "cred4Update cred4Polysky ALL"
    bind $l1 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    bind $l2 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    bind $l3 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    bind $Cred4Widgets(PF_DEGREE) <Button-2> "cred4Update cred4Polysky PF_DEGREE"
    bind $Cred4Widgets(PF_DEGREE) <Double-Button-2> "$Cred4Widgets(PF_DEGREE) delete 0 end"
    bind $Cred4Widgets(PF_DEGREE) <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    bind $Cred4Widgets(PF_NREJECT) <Button-2> "cred4Update cred4Polysky PF_NREJECT"
    bind $Cred4Widgets(PF_NREJECT) <Double-Button-2> "$Cred4Widgets(PF_NREJECT) delete 0 end"
    bind $Cred4Widgets(PF_NREJECT) <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    bind $we <Button-2> "cred4Update cred4Polysky PF_WEIGHT"
    bind $we <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    $Cred4Widgets(PF_DEGREE) delete 0 end
    $Cred4Widgets(PF_DEGREE) insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_degree]]
    $Cred4Widgets(PF_NREJECT) delete 0 end
    $Cred4Widgets(PF_NREJECT) insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_nreject]]
    set Cred4Widgets(PF_WEIGHT) [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_weight]

# Do sky areas in loop
    set skn 4
    while {$skn <=7} {
      set skynum [expr $skn - 3]
      set l1 [label $lev($skn).l1 -text "Start"]
      set l2 [label $lev($skn).l2 -text "End"]
      set l3 [label $lev($skn).l3 -text "Sky Area $skynum" -width 15]
      set l4 [label $lev($skn).l4 -text " "]
      set Cred4Widgets(PF_SAYS$skynum) [entry $lev($skn).s$skn]
      set Cred4Widgets(PF_SAYE$skynum) [entry $lev($skn).e$skn]
      pack $l3 $l4 -in $lev($skn) -side left
      pack $Cred4Widgets(PF_SAYE$skynum) $l2 $Cred4Widgets(PF_SAYS$skynum) $l1 -in $lev($skn) -side right
      bind $l1 <Button-2> "cred4Update cred4Polysky ALL"
      bind $l1 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
      bind $l2 <Button-2> "cred4Update cred4Polysky ALL"
      bind $l2 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
      bind $l3 <Button-2> "cred4Update cred4Polysky ALL"
      bind $l3 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
      bind $l4 <Button-2> "cred4Update cred4Polysky ALL"
      bind $l4 <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
      bind $Cred4Widgets(PF_SAYS$skynum) <Button-2> "cred4Update cred4Polysky PF_SAYS$skynum"
      bind $Cred4Widgets(PF_SAYS$skynum) <Double-Button-2> "$Cred4Widgets(PF_SAYS$skynum) delete 0 end"
      bind $Cred4Widgets(PF_SAYS$skynum) <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
      bind $Cred4Widgets(PF_SAYE$skynum) <Button-2> "cred4Update cred4Polysky PF_SAYE$skynum"
      bind $Cred4Widgets(PF_SAYE$skynum) <Double-Button-2> "$Cred4Widgets(PF_SAYE$skynum) delete 0 end"
      bind $Cred4Widgets(PF_SAYE$skynum) <Button-3> "cred4HelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
      $Cred4Widgets(PF_SAYS$skynum) delete 0 end
      $Cred4Widgets(PF_SAYS$skynum) insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_says$skynum]]
      $Cred4Widgets(PF_SAYE$skynum) delete 0 end
      $Cred4Widgets(PF_SAYE$skynum) insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_saye$skynum]]
      incr skn
    }

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white

#   Set the enhanced sky subtraction parameters
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_polyfit $Cred4Widgets(PF_POLYFIT)
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_weight $Cred4Widgets(PF_WEIGHT)
      set degree [string trim [$Cred4Widgets(PF_DEGREE) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_degree $degree
      set nreject [string trim [$Cred4Widgets(PF_NREJECT) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_nreject $nreject
      set pf_says1 [string trim [$Cred4Widgets(PF_SAYS1) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says1 $pf_says1
      set pf_saye1 [string trim [$Cred4Widgets(PF_SAYE1) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye1 $pf_saye1
      set pf_says2 [string trim [$Cred4Widgets(PF_SAYS2) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says2 $pf_says2
      set pf_saye2 [string trim [$Cred4Widgets(PF_SAYE2) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye2 $pf_saye2
      set pf_says3 [string trim [$Cred4Widgets(PF_SAYS3) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says3 $pf_says3
      set pf_saye3 [string trim [$Cred4Widgets(PF_SAYE3) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye3 $pf_saye3
      set pf_says4 [string trim [$Cred4Widgets(PF_SAYS4) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says4 $pf_says4
      set pf_saye4 [string trim [$Cred4Widgets(PF_SAYE4) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye4 $pf_saye4
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .cred4Dialogue
}
