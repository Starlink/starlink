proc cred4Standard {taskname} {

# Get some global values
    global env
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Standard Parameters Setup" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Create panel layout
    set top    [frame $frame.top]
    set middle [frame $frame.middle]
    set bottom [frame $frame.bottom]
    pack $top $middle $bottom -in $frame -side top -fill both -expand yes

    set tlb [label $top.tlb       -text "Divide by Standard "]
    set rby [radiobutton $top.rby -text "Yes" -variable Cred4Widgets(DIVIDE_BY_STD) -value "YES"]
    set rbn [radiobutton $top.rbn -text "No" -variable Cred4Widgets(DIVIDE_BY_STD) -value "NO"]
    set rba [radiobutton $top.rba -text "Ask" -variable Cred4Widgets(DIVIDE_BY_STD) -value "ASK"]
    pack $tlb -in $top -side left
    pack $rba $rbn $rby -in $top -side right
    bind $tlb <Button-2> "cred4Update cred4Standard ALL"
    bind $tlb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    bind $rby <Button-2> "cred4Update cred4Standard DIVIDE_BY_STD"
    bind $rbn <Button-2> "cred4Update cred4Standard DIVIDE_BY_STD"
    bind $rba <Button-2> "cred4Update cred4Standard DIVIDE_BY_STD"
    bind $rby <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    bind $rbn <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    bind $rba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    set Cred4Widgets(DIVIDE_BY_STD)  [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.divide_by_std.execute]]]

    set mlb [label $middle.mlb -text "Selection Method   "]
    set mfr [radiobutton $middle.fw -text "Forwards" -variable Cred4Widgets(STANDARD_MODE) -value "FORWARDS"]
    set mba [radiobutton $middle.ba -text "Backwards" -variable Cred4Widgets(STANDARD_MODE) -value "BACKWARDS"]
    set mbo [radiobutton $middle.bo -text "Each Way" -variable Cred4Widgets(STANDARD_MODE) -value "BOTH"]
    pack $mlb -in $middle -side left
    pack $mbo $mba $mfr -in $middle -side right
    bind $mlb <Button-2> "cred4Update cred4Standard ALL"
    bind $mlb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    bind $mfr <Button-2> "cred4Update cred4Standard STANDARD_MODE"
    bind $mba <Button-2> "cred4Update cred4Standard STANDARD_MODE"
    bind $mbo <Button-2> "cred4Update cred4Standard STANDARD_MODE"
    bind $mfr <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    bind $mba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    bind $mbo <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    set Cred4Widgets(STANDARD_MODE) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.standard_mode]]]

    set blb [label $bottom.blb -text "Specific Standard  "]
    set Cred4Widgets(SPEC_STD) [entry $bottom.bfi -width 50]
    pack $blb -in $bottom -side left
    pack $Cred4Widgets(SPEC_STD) -in $bottom -side right
    $Cred4Widgets(SPEC_STD) delete 0 end
    bind $blb <Button-2> "cred4Update cred4Standard ALL"
    bind $blb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    bind $Cred4Widgets(SPEC_STD) <Button-2> "cred4Update cred4Standard SPEC_STD"
    bind $Cred4Widgets(SPEC_STD) <Double-Button-2> "$Cred4Widgets(SPEC_STD) delete 0 end"
    bind $Cred4Widgets(SPEC_STD) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    set spec_standard [string tolower [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.specified_std]]]
    if {$Cred4Widgets(STANDARD_MODE) == "SPECIFIED"} {
      $Cred4Widgets(SPEC_STD) insert 0 $spec_standard
    }

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]

# If OK, set them using nbs
    if {$bv == 0} {
      cgs4drCursor watch red white
      nbs put ${Cred4NoticeBoard}.reduction.divide_by_std.execute $Cred4Widgets(DIVIDE_BY_STD)
      nbs put ${Cred4NoticeBoard}.miscellaneous.standard_mode $Cred4Widgets(STANDARD_MODE)
      if {$Cred4Widgets(STANDARD_MODE) == "SPECIFIED"} {
        set spec_file [string trim [$Cred4Widgets(SPEC_STD) get]]
        if {$spec_file=="styymmdd_gggg" || $spec_file=="st$env(CGS4_DATE)_gggg" || $spec_standard==""} {
          set message "cred4Standard error : A dataset has not been specified properly!"
          cgs4drInform $taskname $message
        } else {
          nbs put ${Cred4NoticeBoard}.miscellaneous.specified_std $spec_file
        }
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
}
