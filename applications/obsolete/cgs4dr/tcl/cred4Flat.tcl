proc cred4Flat {taskname} {

# Get some global values
    global env
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Flat Parameters Setup" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Create panel layout
    set top    [frame $frame.top]
    set top2   [frame $frame.top2]
    set middle [frame $frame.middle]
    set bottom [frame $frame.bottom]
    set base   [frame $frame.base]
    set lbase  [frame $frame.lbase]
    pack $top $top2 $middle $bottom $base $lbase -in $frame -side top -fill both -expand yes

    set tlb [label $top.tlb       -text "Divide by FLAT Field?"]
    set rby [radiobutton $top.rby -text "Yes" -variable Cred4Widgets(DIVIDE_BY_FF) -value "YES"]
    set rbn [radiobutton $top.rbn -text "No" -variable Cred4Widgets(DIVIDE_BY_FF) -value "NO"]
    set rba [radiobutton $top.rba -text "Ask" -variable Cred4Widgets(DIVIDE_BY_FF) -value "ASK"]
    pack $tlb -in $top -side left
    pack $rba $rbn $rby -in $top -side right
    bind $tlb <Button-2> "cred4Update cred4Flat ALL"
    bind $tlb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $rby <Button-2> "cred4Update cred4Flat DIVIDE_BY_FF"
    bind $rbn <Button-2> "cred4Update cred4Flat DIVIDE_BY_FF"
    bind $rba <Button-2> "cred4Update cred4Flat DIVIDE_BY_FF"
    bind $rby <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $rbn <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $rba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    set Cred4Widgets(DIVIDE_BY_FF)  [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.divide_by_ff.execute]]]

    set blb [label $top2.blb -text "Normalise FLAT Field?"]
    set rby [radiobutton $top2.rby -text Yes -variable Cred4Widgets(NORMALISE_FF) -value YES]
    set rbn [radiobutton $top2.rbn -text No -variable Cred4Widgets(NORMALISE_FF) -value NO]
    set rba [radiobutton $top2.rba -text Ask -variable Cred4Widgets(NORMALISE_FF) -value ASK]
    set Cred4Widgets(NORMALISE_FF) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.reduction.normalise_ff.execute]]]
    pack $blb -side left
    pack $rba $rbn $rby -side right
    bind $blb <Button-2> "cred4Update cred4Flat ALL"
    bind $blb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $rby <Button-2> "cred4Update cred4Flat NORMALISE_FF"
    bind $rby <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $rbn <Button-2> "cred4Update cred4Flat NORMALISE_FF"
    bind $rbn <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $rba <Button-2> "cred4Update cred4Flat NORMALISE_FF"
    bind $rba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"

    set mlb [label $middle.mlb -text "Selection Method    "]
    set mfr [radiobutton $middle.fw -text "Forwards" -variable Cred4Widgets(FLAT_MODE) -value "FORWARDS"]
    set mba [radiobutton $middle.ba -text "Backwards" -variable Cred4Widgets(FLAT_MODE) -value "BACKWARDS"]
    set mbo [radiobutton $middle.bo -text "Each Way" -variable Cred4Widgets(FLAT_MODE) -value "BOTH"]
    pack $mlb -in $middle -side left
    pack $mbo $mba $mfr -in $middle -side right
    bind $mlb <Button-2> "cred4Update cred4Flat ALL"
    bind $mlb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $mfr <Button-2> "cred4Update cred4Flat FLAT_MODE"
    bind $mba <Button-2> "cred4Update cred4Flat FLAT_MODE"
    bind $mbo <Button-2> "cred4Update cred4Flat FLAT_MODE"
    bind $mfr <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $mba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $mbo <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    set Cred4Widgets(FLAT_MODE) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.flat_mode]]]

    set blb [label $bottom.blb -text "Specific Flat       "]
    set Cred4Widgets(SPEC_FLAT) [entry $bottom.bfi -width 50]
    pack $blb -in $bottom -side left
    pack $Cred4Widgets(SPEC_FLAT) -in $bottom -side right
    $Cred4Widgets(SPEC_FLAT) delete 0 end
    bind $blb <Button-2> "cred4Update cred4Flat ALL"
    bind $blb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $Cred4Widgets(SPEC_FLAT) <Button-2> "cred4Update cred4Flat SPEC_FLAT"
    bind $Cred4Widgets(SPEC_FLAT) <Double-Button-2> "$Cred4Widgets(SPEC_FLAT) delete 0 end"
    bind $Cred4Widgets(SPEC_FLAT) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    set spec_flat [string tolower [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.specified_flat]]]
    if {$Cred4Widgets(FLAT_MODE) == "SPECIFIED"} {
      $Cred4Widgets(SPEC_FLAT) insert 0 $spec_flat
    }

    set blb2 [label $base.blb2 -text "FLAT Field Normalisation Method"]
    set pf [radiobutton $base.pf -text "Polyfit" -variable Cred4Widgets(FF_METHOD) -value POLYFIT]
    set sm [radiobutton $base.sm -text "Smooth" -variable Cred4Widgets(FF_METHOD) -value SMOOTH]
    set Cred4Widgets(FF_METHOD) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.reduction.normalise_ff.method]]]
    pack $blb2 -side left
    pack $sm $pf -side right
    bind $blb2 <Button-2> "cred4Update cred4Flat ALL"
    bind $blb2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $pf <Button-2> "cred4Update cred4Flat FF_METHOD"
    bind $pf <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $sm <Button-2> "cred4Update cred4Flat FF_METHOD"
    bind $sm <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"

    set Cred4Widgets(ORDER) [scale $lbase.order -label "Polynomial Order" -from 1 -to 7  -orient h -length 8c -width 0.5c -tickinterval 1]
    set Cred4Widgets(BOXSIZE) [scale $lbase.boxsize -label "Smooth Boxsize"   -from 1 -to 15 -orient h -length 8c -width 0.5c -tickinterval 2]
    pack $Cred4Widgets(ORDER) -side left
    pack $Cred4Widgets(BOXSIZE) -side right
    bind $Cred4Widgets(ORDER) <Button-2> "cred4Update cred4Flat ORDER"
    bind $Cred4Widgets(ORDER) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $Cred4Widgets(BOXSIZE) <Button-2> "cred4Update cred4Flat BOXSIZE"
    bind $Cred4Widgets(BOXSIZE) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    $Cred4Widgets(ORDER)   set [nbs get ${Cred4NoticeBoard}.reduction.normalise_ff.order]
    $Cred4Widgets(BOXSIZE) set [nbs get ${Cred4NoticeBoard}.reduction.normalise_ff.boxsize]

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]

# If OK, set them using nbs
    if {$bv == 0} {
      cgs4drCursor watch red white
      nbs put ${Cred4NoticeBoard}.reduction.divide_by_ff.execute $Cred4Widgets(DIVIDE_BY_FF)
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.method $Cred4Widgets(FF_METHOD)
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.execute $Cred4Widgets(NORMALISE_FF)
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.order [string trim [$Cred4Widgets(ORDER) get]]
      set bnum [$Cred4Widgets(BOXSIZE) get]
      if {$Cred4Widgets(FF_METHOD) == "SMOOTH" && [expr $bnum % 2] != 1} {
        set message "cred4Flat warning : Smoothing box value ($bnum) is even, using [expr $bnum - 1]"
        cgs4drInform $taskname $message
        set bnum [expr $bnum - 1]
      }
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.boxsize $bnum
      nbs put ${Cred4NoticeBoard}.miscellaneous.flat_mode $Cred4Widgets(FLAT_MODE)
      if {$Cred4Widgets(FLAT_MODE) == "SPECIFIED"} {
        set spec_file [string trim [$Cred4Widgets(SPEC_FLAT) get]]
        if {$spec_file=="royymmdd_oooo" || $spec_file=="ro$env(CGS4_DATE)_oooo" || $spec_flat==""} {
          set message "cred4Flat error : A dataset has not been specified properly!"
          cgs4drInform $taskname $message
        } else {
          nbs put ${Cred4NoticeBoard}.miscellaneous.specified_flat $spec_file
        }
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
}
