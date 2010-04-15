proc cred4AstParams {} {

# Get some global values
    global Cred4NoticeBoard
    global Cred4Widgets

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Astronomical Parameters Setup" 0 OK Defaults Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Create panel layout
    set rtop    [frame $frame.top    -relief sunken -bd 2]
    set rmiddle [frame $frame.middle -relief sunken -bd 2]
    set rbottom [frame $frame.bottom -relief sunken -bd 2]
    pack $rtop $rbottom $rmiddle -in $frame -side top -fill both -expand yes

# Top-Right contains Normal Sky Subtraction Parameters
    set toptop [frame $rtop.top]
    set midtop [frame $rtop.mid]
    set bottop [frame $rtop.bot]
    pack $toptop $midtop $bottop -in $rtop

    set title [label $toptop.title -text "Normal Sky Subtraction Parameters"]
    pack $title -in $toptop

    set l1 [checkbutton $midtop.l1 -text "Add Observations into Pairs" -variable Cred4Widgets(ADD_IN_PAIRS)]
    set l2 [label $midtop.l2 -text " "]
    set fi [radiobutton $midtop.fi -text "Errors from Int" -variable Cred4Widgets(ERRORS) -value "FROM_INT" -width 15]
    set fo [radiobutton $midtop.fo -text "Errors from Obs" -variable Cred4Widgets(ERRORS) -value "FROM_OBS" -width 15]
    pack $l1 $l2 -in $midtop -side left -pady 2m
    pack $fo $fi -in $midtop -side right -pady 2m

    set Cred4Widgets(ADD_IN_PAIRS) 1
    if {[nbs get ${Cred4NoticeBoard}.miscellaneous.add_in_pairs]==0} {set Cred4Widgets(ADD_IN_PAIRS) 0}
    set Cred4Widgets(ERRORS) "FROM_OBS"

    set l1 [label $bottop.l1 -text "Sky Weighting Factor"]
    set sk [entry $bottop.sk -width 15 -textvariable Cred4Widgets(SKYWT)]
    set vw [checkbutton $bottop.l2 -text "Variance Weighting" -variable Cred4Widgets(VARWT)]
    set l3 [label $bottop.l3 -text " "]
    pack $vw $l3 -in $bottop -side left -pady 2m
    pack $sk $l1 -in $bottop -side right -pady 2m

    set Cred4Widgets(VARWT) [nbs get ${Cred4NoticeBoard}.miscellaneous.variance_wt]
    $sk delete 0 end
    $sk insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.sky_wt]]

# Middle-Right contains Extract Nodded Spectrum Parameters
    set toptop [frame $rmiddle.top]
    set midtop [frame $rmiddle.mid]
    set bottop [frame $rmiddle.bot]
    set basetop [frame $rmiddle.bas]
    set lbasetop [frame $rmiddle.lba]
    pack $toptop $midtop $bottop $basetop $lbasetop -in $rmiddle

    set title [label $toptop.title -text "Extract Nodded Spectra Parameters"]
    pack $title -in $toptop

    set label [label $midtop.lb -text "Algorithm"]
    set f1 [radiobutton $midtop.f1 -text "Bright" -variable Cred4Widgets(SPC_ALGORITHM) -value "BRIGHT"]
    set f2 [radiobutton $midtop.f2 -text "Faint" -variable Cred4Widgets(SPC_ALGORITHM) -value "FAINT"]
    set iv [checkbutton $midtop.we -text "Invert" -variable Cred4Widgets(SPC_INVERT)]
    set l4 [label $midtop.l4 -text " "]
    pack $label $f1 $f2 $l4 -in $midtop -side left -pady 2m
    pack $iv -in $midtop -side right -pady 2m
    set Cred4Widgets(SPC_ALGORITHM) [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.algorithm]]]
    set Cred4Widgets(SPC_INVERT) [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.invert]

    set l1 [label $bottop.l1 -text "Start"]
    set l2 [label $bottop.l2 -text "End"]
    set l3 [label $bottop.l3 -text "Top Extraction Row" -width 20]
    set l4 [label $bottop.l4 -text " "]
    set s1 [entry $bottop.s1 -textvariable Cred4Widgets(SPC_ROW1S)]
    set e1 [entry $bottop.e1 -textvariable Cred4Widgets(SPC_ROW1E)]
    pack $l3 $l4 -in $bottop -side left -pady 2m
    pack $e1 $l2 $s1 $l1 -in $bottop -side right -pady 2m
    $s1 delete 0 end
    $s1 insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row1s]]
    $e1 delete 0 end
    $e1 insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row1e]]

    set l1 [label $basetop.l1 -text "Start"]
    set l2 [label $basetop.l2 -text "End"]
    set l3 [label $basetop.l3 -text "Middle Extraction Row" -width 20]
    set l4 [label $basetop.l4 -text " "]
    set s2 [entry $basetop.s1 -textvariable Cred4Widgets(SPC_ROW2S)]
    set e2 [entry $basetop.e1 -textvariable Cred4Widgets(SPC_ROW2E)]
    pack $l3 $l4 -in $basetop -side left -pady 2m
    pack $e2 $l2 $s2 $l1 -in $basetop -side right -pady 2m
    $s2 delete 0 end
    $s2 insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row2s]]
    $e2 delete 0 end
    $e2 insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row2e]]

    set l1 [label $lbasetop.l1 -text "Start"]
    set l2 [label $lbasetop.l2 -text "End"]
    set l3 [label $lbasetop.l3 -text "Bottom Extraction Row" -width 20]
    set l4 [label $lbasetop.l4 -text " "]
    set s3 [entry $lbasetop.s1 -textvariable Cred4Widgets(SPC_ROW3S)]
    set e3 [entry $lbasetop.e1 -textvariable Cred4Widgets(SPC_ROW3E)]
    pack $l3 $l4 -in $lbasetop -side left -pady 2m
    pack $e3 $l2 $s3 $l1 -in $lbasetop -side right -pady 2m
    $s3 delete 0 end
    $s3 insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row3s]]
    $e3 delete 0 end
    $e3 insert end [string trim [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.row3e]]

# Bottom-Right contains Enhanced Sky Subtraction Parameters
    set lev1 [frame $rbottom.l1]
    set lev2 [frame $rbottom.l2]
    set lev3 [frame $rbottom.l3]
    set lev4 [frame $rbottom.l4]
    set lev5 [frame $rbottom.l5]
    set lev6 [frame $rbottom.l6]
    set lev7 [frame $rbottom.l7]
    pack $lev1 $lev2 $lev3 $lev4 $lev5 $lev6 $lev7 -in $rbottom

    set title [label $lev1.title -text "Enhanced Sky Subtraction Parameters (Polysky)"]
    pack $title -in $lev1

    set pf1 [radiobutton $lev2.f1 -text "None" -variable Cred4Widgets(PF_POLYFIT) -value "NONE"]
    set pf2 [radiobutton $lev2.f2 -text "RGs" -variable Cred4Widgets(PF_POLYFIT) -value "REDUCED_GRP"]
    set pf3 [radiobutton $lev2.f3 -text "Obj" -variable Cred4Widgets(PF_POLYFIT) -value "OBJECT"]
    set pf4 [radiobutton $lev2.f4 -text "Obj-Sky" -variable Cred4Widgets(PF_POLYFIT) -value "OBJ-SKY"]
    pack $pf1 $pf2 $pf3 $pf4 -in $lev2 -side left -pady 2m
    set Cred4Widgets(PF_POLYFIT) [string trim [string toupper [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_polyfit]]]

    set l1 [label $lev3.l1 -text "Degree"]
    set dg [entry $lev3.dg -width 15 -textvariable Cred4Widgets(PF_DEGREE)]
    set l2 [label $lev3.l2 -text "Nreject"]
    set nr [entry $lev3.nr -width 15 -textvariable Cred4Widgets(PF_NREJECT)]
    set we [checkbutton $lev3.we -text "Weight" -variable Cred4Widgets(PF_WEIGHT)]
    set l3 [label $lev3.l3 -text " "]
    pack $l1 $dg $l2 $nr -in $lev3 -side left -pady 2m
    pack $we $l3 -in $lev3 -side right -pady 2m
    $dg delete 0 end
    $dg insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_degree]]
    $nr delete 0 end
    $nr insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_nreject]]
    set Cred4Widgets(PF_WEIGHT) [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_weight]

    set l1 [label $lev4.l1 -text "Start"]
    set l2 [label $lev4.l2 -text "End"]
    set l3 [label $lev4.l3 -text "First Sky Area" -width 15]
    set l4 [label $lev4.l4 -text " "]
    set ss1 [entry $lev4.s1 -textvariable Cred4Widgets(PF_SAYS1)]
    set se1 [entry $lev4.e1 -textvariable Cred4Widgets(PF_SAYE1)]
    pack $l3 $l4 -in $lev4 -side left -pady 2m
    pack $se1 $l2 $ss1 $l1 -in $lev4 -side right -pady 2m
    $ss1 delete 0 end
    $ss1 insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_says1]]
    $se1 delete 0 end
    $se1 insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_saye1]]

    set l1 [label $lev5.l1 -text "Start"]
    set l2 [label $lev5.l2 -text "End"]
    set l3 [label $lev5.l3 -text "Second Sky Area" -width 15]
    set l4 [label $lev5.l5 -text " "]
    set ss2 [entry $lev5.s1 -textvariable Cred4Widgets(PF_SAYS2)]
    set se2 [entry $lev5.e1 -textvariable Cred4Widgets(PF_SAYE2)]
    pack $l3 $l4 -in $lev5 -side left -pady 2m
    pack $se2 $l2 $ss2 $l1 -in $lev5 -side right -pady 2m
    $ss2 delete 0 end
    $ss2 insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_says2]]
    $se2 delete 0 end
    $se2 insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_saye2]]

    set l1 [label $lev6.l1 -text "Start"]
    set l2 [label $lev6.l2 -text "End"]
    set l3 [label $lev6.l3 -text "Third Sky Area" -width 15]
    set l4 [label $lev6.l6 -text " "]
    set ss3 [entry $lev6.s1 -textvariable Cred4Widgets(PF_SAYS3)]
    set se3 [entry $lev6.e1 -textvariable Cred4Widgets(PF_SAYE3)]
    pack $l3 $l4 -in $lev6 -side left -pady 2m
    pack $se3 $l2 $ss3 $l1 -in $lev6 -side right -pady 2m
    $ss3 delete 0 end
    $ss3 insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_says3]]
    $se3 delete 0 end
    $se3 insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_saye3]]

    set l1 [label $lev7.l1 -text "Start"]
    set l2 [label $lev7.l2 -text "End"]
    set l3 [label $lev7.l3 -text "Fourth Sky Area" -width 15]
    set l4 [label $lev7.l7 -text " "]
    set ss4 [entry $lev7.s1 -textvariable Cred4Widgets(PF_SAYS4)]
    set se4 [entry $lev7.e1 -textvariable Cred4Widgets(PF_SAYE4)]
    pack $l3 $l4 -in $lev7 -side left -pady 2m
    pack $se4 $l2 $ss4 $l1 -in $lev7 -side right -pady 2m
    $ss4 delete 0 end
    $ss4 insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_says4]]
    $se4 delete 0 end
    $se4 insert end [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.pf_saye4]]

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white

#   Set the sky subtraction parameters
      nbs put ${Cred4NoticeBoard}.miscellaneous.errors $Cred4Widgets(ERRORS)
      nbs put ${Cred4NoticeBoard}.miscellaneous.add_in_pairs $Cred4Widgets(ADD_IN_PAIRS)
      nbs put ${Cred4NoticeBoard}.miscellaneous.variance_wt $Cred4Widgets(VARWT)
      set skywt [string trim [$sk get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.sky_wt $skywt

#   Set the enhanced sky subtraction parameters
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_polyfit $Cred4Widgets(PF_POLYFIT)
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_weight $Cred4Widgets(PF_WEIGHT)
      set degree [string trim [$dg get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_degree $degree
      set nreject [string trim [$nr get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_nreject $nreject
      set pf_says1 [string trim [$ss1 get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says1 $pf_says1
      set pf_saye1 [string trim [$se1 get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye1 $pf_saye1
      set pf_says2 [string trim [$ss2 get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says2 $pf_says2
      set pf_saye2 [string trim [$se2 get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye2 $pf_saye2
      set pf_says3 [string trim [$ss3 get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says3 $pf_says3
      set pf_saye3 [string trim [$se3 get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye3 $pf_saye3
      set pf_says4 [string trim [$ss4 get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says4 $pf_says4
      set pf_saye4 [string trim [$se4 get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye4 $pf_saye4

#   Set the extract nodded spectra parameters
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.algorithm $Cred4Widgets(SPC_ALGORITHM)
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.invert $Cred4Widgets(SPC_INVERT)
      set row1s [string trim [$s1 get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row1s $row1s
      set row1e [string trim [$e1 get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row1e $row1e
      set row2s [string trim [$s2 get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row2s $row2s
      set row2e [string trim [$e2 get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row2e $row2e
      set row3s [string trim [$s3 get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row3s $row3s
      set row3e [string trim [$e3 get]]
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row3e $row3e

#   Set some defaults
    } elseif {$bv == 1} {
      cgs4drCursor watch red white
      nbs put ${Cred4NoticeBoard}.miscellaneous.errors FROM_OBS
      nbs put ${Cred4NoticeBoard}.miscellaneous.add_in_pairs TRUE
      nbs put ${Cred4NoticeBoard}.miscellaneous.variance_wt FALSE
      nbs put ${Cred4NoticeBoard}.miscellaneous.sky_wt 1.0
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_polyfit NONE
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_weight TRUE
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_degree 1
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_nreject 0
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says1 20
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye1 25
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says2 35
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye2 40
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says3 -1
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye3 -1
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_says4 -1
      nbs put ${Cred4NoticeBoard}.miscellaneous.pf_saye4 -1
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.algorithm BRIGHT
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.invert FALSE
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row1s 27
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row1e 27
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row2s -1
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row2e -1
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row3s -1
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.row3e -1
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .cred4Dialogue
}
