proc cred4Calib {taskname} {

# Get some global values
    global env
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Calibration Parameters Setup" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Create panel layout
    set top    [frame $frame.top]
    set middle [frame $frame.middle]
    set bottom [frame $frame.bottom]
    set base   [frame $frame.base]
    pack $top $middle $bottom $base -in $frame -side top -fill both -expand yes

    set tlb [label $top.tlb       -text "Wavelength Calibrate"]
    set rby [radiobutton $top.rby -text "Yes" -variable Cred4Widgets(TO_WAVELENGTH) -value "YES"]
    set rbn [radiobutton $top.rbn -text "No" -variable Cred4Widgets(TO_WAVELENGTH) -value "NO"]
    set rba [radiobutton $top.rba -text "Ask" -variable Cred4Widgets(TO_WAVELENGTH) -value "ASK"]
    pack $tlb -in $top -side left
    pack $rba $rbn $rby -in $top -side right
    bind $tlb <Button-2> "cred4Update cred4Calib ALL"
    bind $tlb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    bind $rby <Button-2> "cred4Update cred4Calib TO_WAVELENGTH"
    bind $rbn <Button-2> "cred4Update cred4Calib TO_WAVELENGTH"
    bind $rba <Button-2> "cred4Update cred4Calib TO_WAVELENGTH"
    bind $rby <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    bind $rbn <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    bind $rba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    set Cred4Widgets(TO_WAVELENGTH)  [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.to_wavelength.execute]]]

    set mlb [label $middle.mlb    -text "Selection Method    "]
    set mfr [radiobutton $middle.fw -text "Forwards" -variable Cred4Widgets(CALIB_MODE) -value "FORWARDS"]
    set mba [radiobutton $middle.ba -text "Backwards" -variable Cred4Widgets(CALIB_MODE) -value "BACKWARDS"]
    set mbo [radiobutton $middle.bo -text "Each Way" -variable Cred4Widgets(CALIB_MODE) -value "BOTH"]
    pack $mlb -in $middle -side left
    pack $mbo $mba $mfr -in $middle -side right
    bind $mlb <Button-2> "cred4Update cred4Calib ALL"
    bind $mlb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    bind $mfr <Button-2> "cred4Update cred4Calib CALIB_MODE"
    bind $mba <Button-2> "cred4Update cred4Calib CALIB_MODE"
    bind $mbo <Button-2> "cred4Update cred4Calib CALIB_MODE"
    bind $mfr <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    bind $mba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    bind $mbo <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    set Cred4Widgets(CALIB_MODE) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.calib_mode]]]

    set blb [label $bottom.blb -text "Specific Calibration"]
    set Cred4Widgets(SPEC_CALIB) [entry $bottom.bfi -width 50]
    pack $blb -in $bottom -side left
    pack $Cred4Widgets(SPEC_CALIB) -in $bottom -side right
    $Cred4Widgets(SPEC_CALIB) delete 0 end
    bind $blb <Button-2> "cred4Update cred4Calib ALL"
    bind $blb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    bind $Cred4Widgets(SPEC_CALIB) <Button-2> "cred4Update cred4Calib SPEC_CALIB"
    bind $Cred4Widgets(SPEC_CALIB) <Double-Button-2> "$Cred4Widgets(SPEC_CALIB) delete 0 end"
    bind $Cred4Widgets(SPEC_CALIB) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    set spec_calib [string tolower [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.specified_calib]]]
    if {$Cred4Widgets(CALIB_MODE) == "SPECIFIED"} {
      $Cred4Widgets(SPEC_CALIB) insert 0 $spec_calib
    }

    set alb [label $base.blb -text "Calibration Method  "]
    set es [radiobutton $base.es -text "Estimated" -variable Cred4Widgets(LAMBDA_METHOD) -value "ESTIMATED"]
    set ca [radiobutton $base.ca -text "Calibrated" -variable Cred4Widgets(LAMBDA_METHOD) -value "CALIBRATED"]
    pack $alb $es $ca -side left -in $base
    bind $alb <Button-2> "cred4Update cred4Calib ALL"
    bind $alb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    bind $es <Button-2> "cred4Update cred4Calib LAMBDA_METHOD"
    bind $es <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    bind $ca <Button-2> "cred4Update cred4Calib LAMBDA_METHOD"
    bind $ca <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    set Cred4Widgets(LAMBDA_METHOD) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.reduction.to_wavelength.method]]]

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]

# If OK, set them using nbs
    if {$bv == 0} {
      cgs4drCursor watch red white
      nbs put ${Cred4NoticeBoard}.reduction.to_wavelength.execute $Cred4Widgets(TO_WAVELENGTH)
      nbs put ${Cred4NoticeBoard}.miscellaneous.calib_mode $Cred4Widgets(CALIB_MODE)
      nbs put ${Cred4NoticeBoard}.reduction.to_wavelength.method $Cred4Widgets(LAMBDA_METHOD)
      if {$Cred4Widgets(CALIB_MODE) == "SPECIFIED"} {
        set spec_file [string trim [$Cred4Widgets(SPEC_CALIB) get]]
        if {$spec_file=="cayymmdd_oooo" || $spec_file=="ca$env(CGS4_DATE)_oooo" || $spec_calib==""} {
          set message "cred4Calib error : A dataset has not been specified properly!"
          cgs4drInform $taskname $message
        } else {
          nbs put ${Cred4NoticeBoard}.miscellaneous.specified_calib $spec_file
        }
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
}
