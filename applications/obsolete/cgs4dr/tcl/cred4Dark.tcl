proc cred4Dark {taskname} {

# Get some global values
    global env
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Dark Parameters Setup" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Create panel layout
    set top    [frame $frame.top]
    set middle [frame $frame.middle]
    set bottom [frame $frame.bottom]
    pack $top $middle $bottom -in $frame -side top -fill both -expand yes

    set tlb [label $top.tlb       -text "Subtract DARK Frame"]
    set rby [radiobutton $top.rby -text "Yes" -variable Cred4Widgets(SUBTRACT_DARK) -value "YES"]
    set rbn [radiobutton $top.rbn -text "No" -variable Cred4Widgets(SUBTRACT_DARK) -value "NO"]
    set rba [radiobutton $top.rba -text "Ask" -variable Cred4Widgets(SUBTRACT_DARK) -value "ASK"]
    pack $tlb -in $top -side left
    pack $rba $rbn $rby -in $top -side right
    bind $tlb <Button-2> "cred4Update cred4Dark ALL"
    bind $tlb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    bind $rby <Button-2> "cred4Update cred4Dark SUBTRACT_DARK"
    bind $rbn <Button-2> "cred4Update cred4Dark SUBTRACT_DARK"
    bind $rba <Button-2> "cred4Update cred4Dark SUBTRACT_DARK"
    bind $rby <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    bind $rbn <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    bind $rba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    set Cred4Widgets(SUBTRACT_DARK)  [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.subtract_dark.execute]]]

    set mlb [label $middle.mlb -text "Selection Method   "]
    set mfr [radiobutton $middle.fw -text "Forwards" -variable Cred4Widgets(DARK_MODE) -value "FORWARDS"]
    set mba [radiobutton $middle.ba -text "Backwards" -variable Cred4Widgets(DARK_MODE) -value "BACKWARDS"]
    set mbo [radiobutton $middle.bo -text "Each Way" -variable Cred4Widgets(DARK_MODE) -value "BOTH"]
    pack $mlb -in $middle -side left
    pack $mbo $mba $mfr -in $middle -side right
    bind $mlb <Button-2> "cred4Update cred4Dark ALL"
    bind $mlb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    bind $mfr <Button-2> "cred4Update cred4Dark DARK_MODE"
    bind $mba <Button-2> "cred4Update cred4Dark DARK_MODE"
    bind $mbo <Button-2> "cred4Update cred4Dark DARK_MODE"
    bind $mfr <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    bind $mba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    bind $mbo <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    set Cred4Widgets(DARK_MODE) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.dark_mode]]]

    set blb [label $bottom.blb -text "Specific Dark      "]
    set Cred4Widgets(SPEC_DARK) [entry $bottom.bfi -width 50]
    pack $blb -in $bottom -side left
    pack $Cred4Widgets(SPEC_DARK) -in $bottom -side right
    $Cred4Widgets(SPEC_DARK) delete 0 end
    bind $blb <Button-2> "cred4Update cred4Dark ALL"
    bind $blb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    bind $Cred4Widgets(SPEC_DARK) <Button-2> "cred4Update cred4Dark SPEC_DARK"
    bind $Cred4Widgets(SPEC_DARK) <Double-Button-2> "$Cred4Widgets(SPEC_DARK) delete 0 end"
    bind $Cred4Widgets(SPEC_DARK) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    set spec_dark [string tolower [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.specified_dark]]]
    if {$Cred4Widgets(DARK_MODE) == "SPECIFIED"} {
      $Cred4Widgets(SPEC_DARK) insert 0 $spec_dark
    }

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]

# If OK, set them using nbs
    if {$bv == 0} {
      cgs4drCursor watch red white
      nbs put ${Cred4NoticeBoard}.reduction.subtract_dark.execute $Cred4Widgets(SUBTRACT_DARK)
      nbs put ${Cred4NoticeBoard}.miscellaneous.dark_mode $Cred4Widgets(DARK_MODE)
      if {$Cred4Widgets(DARK_MODE) == "SPECIFIED"} {
        set spec_file [string trim [$Cred4Widgets(SPEC_DARK) get]]
        if {$spec_file=="royymmdd_oooo" || $spec_file=="ro$env(CGS4_DATE)_oooo" || $spec_dark==""} {
          set message "cred4Dark error : A dataset has not been specified properly!"
          cgs4drInform $taskname $message
        } else {
          nbs put ${Cred4NoticeBoard}.miscellaneous.specified_dark $spec_file
        }
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
}
