proc cred4Bias {taskname} {

# Get some global values
    global env
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Bias Parameters Setup" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Create panel layout
    set top    [frame $frame.top]
    set middle [frame $frame.middle]
    set bottom [frame $frame.bottom]
    pack $top $middle $bottom -in $frame -side top -fill both -expand yes

    set tlb [label $top.tlb       -text "Subtract BIAS Frame"]
    set rby [radiobutton $top.rby -text "Yes" -variable Cred4Widgets(SUBTRACT_BIAS) -value "YES"]
    set rbn [radiobutton $top.rbn -text "No" -variable Cred4Widgets(SUBTRACT_BIAS) -value "NO"]
    set rba [radiobutton $top.rba -text "Ask" -variable Cred4Widgets(SUBTRACT_BIAS) -value "ASK"]
    pack $tlb -in $top -side left
    pack $rba $rbn $rby -in $top -side right
    bind $tlb <Button-2> "cred4Update cred4Bias ALL"
    bind $tlb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    bind $rby <Button-2> "cred4Update cred4Bias SUBTRACT_BIAS"
    bind $rbn <Button-2> "cred4Update cred4Bias SUBTRACT_BIAS"
    bind $rba <Button-2> "cred4Update cred4Bias SUBTRACT_BIAS"
    bind $rby <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    bind $rbn <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    bind $rba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    set Cred4Widgets(SUBTRACT_BIAS)  [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.subtract_bias.execute]]]

    set mlb [label $middle.mlb -text "Selection Method   "]
    set mfr [radiobutton $middle.fw -text "Forwards" -variable Cred4Widgets(BIAS_MODE) -value "FORWARDS"]
    set mba [radiobutton $middle.ba -text "Backwards" -variable Cred4Widgets(BIAS_MODE) -value "BACKWARDS"]
    set mbo [radiobutton $middle.bo -text "Each Way" -variable Cred4Widgets(BIAS_MODE) -value "BOTH"]
    pack $mlb -in $middle -side left
    pack $mbo $mba $mfr -in $middle -side right
    bind $mlb <Button-2> "cred4Update cred4Bias ALL"
    bind $mlb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    bind $mfr <Button-2> "cred4Update cred4Bias BIAS_MODE"
    bind $mba <Button-2> "cred4Update cred4Bias BIAS_MODE"
    bind $mbo <Button-2> "cred4Update cred4Bias BIAS_MODE"
    bind $mfr <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    bind $mba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    bind $mbo <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    set Cred4Widgets(BIAS_MODE) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.bias_mode]]]

    set blb [label $bottom.blb -text "Specific Bias      "]
    set Cred4Widgets(SPEC_BIAS) [entry $bottom.bfi -width 50]
    pack $blb -in $bottom -side left
    pack $Cred4Widgets(SPEC_BIAS) -in $bottom -side right
    $Cred4Widgets(SPEC_BIAS) delete 0 end
    bind $blb <Button-2> "cred4Update cred4Bias ALL"
    bind $blb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    bind $Cred4Widgets(SPEC_BIAS) <Button-2> "cred4Update cred4Bias SPEC_BIAS"
    bind $Cred4Widgets(SPEC_BIAS) <Double-Button-2> "$Cred4Widgets(SPEC_BIAS) delete 0 end"
    bind $Cred4Widgets(SPEC_BIAS) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    set spec_bias [string tolower [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.specified_bias]]]
    if {$Cred4Widgets(BIAS_MODE) == "SPECIFIED"} {
      $Cred4Widgets(SPEC_BIAS) insert 0 $spec_bias
    }

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]

# If OK, set them using nbs
    if {$bv == 0} {
      cgs4drCursor watch red white
      nbs put ${Cred4NoticeBoard}.reduction.subtract_bias.execute $Cred4Widgets(SUBTRACT_BIAS)
      nbs put ${Cred4NoticeBoard}.miscellaneous.bias_mode $Cred4Widgets(BIAS_MODE)
      if {$Cred4Widgets(BIAS_MODE) == "SPECIFIED"} {
        set spec_file [string trim [$Cred4Widgets(SPEC_BIAS) get]]
        if {$spec_file=="royymmdd_oooo" || $spec_file=="ro$env(CGS4_DATE)_oooo" || $spec_bias==""} {
          set message "cred4Bias error : A dataset has not been specified properly!"
          cgs4drInform $taskname $message
        } else {
          nbs put ${Cred4NoticeBoard}.miscellaneous.specified_bias $spec_file
        }
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
}
