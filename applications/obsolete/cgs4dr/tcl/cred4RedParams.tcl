proc cred4RedParams {taskname} {

# Get some global values
    global env
    global Cred4NoticeBoard
    global Cred4Widgets

# Create dialog box
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Reduction Parameters Setup" 0 OK Defaults Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Create panel layout
    set ltop    [frame $frame.top    -relief sunken -bd 2]
    set lmiddle [frame $frame.middle -relief sunken -bd 2]
    set lbottom [frame $frame.bottom -relief sunken -bd 2]
    set lbase   [frame $frame.base   -relief sunken -bd 2]
    pack $ltop $lmiddle $lbottom $lbase -in $frame -side top -fill both -expand yes

# Top-Left contains bad pixel mask or Linearisation file
    set toptop [frame $ltop.top]
    set bottop [frame $ltop.bot]
    pack $toptop $bottop -in $ltop

    set title [label $toptop.title -text "Bad Pixel Mask or Linearisation File Selection"]
    pack $title -in $toptop

    set labpm [label $bottop.labpm -text "Bad Pixel Mask"]
    set Cred4Widgets(MASK) [entry $bottop.mask -relief sunken -bd 2 -width 20]
    pack $labpm $Cred4Widgets(MASK) -in $bottop -side left -pady 2m
    $Cred4Widgets(MASK) insert 0 [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.mask]]

    set lalin [label $bottop.lalin -text "Linearisation File"]
    set Cred4Widgets(LINCOEFFS) [entry $bottop.line -relief sunken -bd 2 -width 20]
    pack $Cred4Widgets(LINCOEFFS) $lalin -in $bottop -side right -pady 2m
    $Cred4Widgets(LINCOEFFS) insert 0 [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.lincoeffs]]

# Middle-Left frame FF Normalisation
    set toptop [frame $lmiddle.top]
    set midtop [frame $lmiddle.mid]
    pack $toptop $midtop -in $lmiddle

    set title [label $toptop.title -text "Flat Field Normalisation Parameters"]
    pack $title -in $toptop

    set pf [radiobutton $midtop.pf -text "Polyfit" -variable Cred4Widgets(FF_METHOD) -value "POLYFIT"]
    set label1 [label $midtop.label1 -text "Polynomial"]
    set Cred4Widgets(ORDER) [entry $midtop.order -width 10 -relief sunken -bd 2]
    set sm [radiobutton $midtop.sm -text "Smooth" -variable Cred4Widgets(FF_METHOD) -value "SMOOTH"]
    set label2 [label $midtop.label2 -text "Boxsize"]
    set Cred4Widgets(BOXSIZE) [entry $midtop.boxsize -width 10 -relief sunken -bd 2]

    pack $pf $sm $label1 $Cred4Widgets(ORDER) $label2 $Cred4Widgets(BOXSIZE) -in $midtop -side left -pady 2m

    $Cred4Widgets(ORDER) delete 0 end
    $Cred4Widgets(ORDER) insert 0 [string trim [nbs get ${Cred4NoticeBoard}.reduction.normalise_ff.order]]
    $Cred4Widgets(BOXSIZE) delete 0 end
    $Cred4Widgets(BOXSIZE) insert 0 [string trim [nbs get ${Cred4NoticeBoard}.reduction.normalise_ff.boxsize]]

    set Cred4Widgets(FF_METHOD) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.reduction.normalise_ff.method]]]

# Bottom-Left contains Wavelength Calibration
    set toptop [frame $lbottom.top]
    set midtop [frame $lbottom.mid]
    pack $toptop $midtop -in $lbottom

    set title [label $toptop.title -text "Wavelength Calibration Method"]
    pack $title -in $toptop

    set es [radiobutton $midtop.es -text "Estimated from Grating" -variable Cred4Widgets(LAMBDA_METHOD) -value "ESTIMATED"]
    set ca [radiobutton $midtop.ca -text "Calibrated from Arc" -variable Cred4Widgets(LAMBDA_METHOD) -value "CALIBRATED"]
    pack $es $ca -side left -in $midtop -pady 2m -ipadx 15m

    set Cred4Widgets(LAMBDA_METHOD) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.reduction.to_wavelength.method]]]

# Base-Left contains File Selection Parameters
    set top1top   [frame $lbase.top1]
    set bias1  [frame $lbase.mid1]
    set dark1  [frame $lbase.top2]
    set flat1  [frame $lbase.bot2]
    set calib1 [frame $lbase.mid3]
    set std1   [frame $lbase.top4]
    pack $top1top $bias1 $dark1 $flat1 $calib1 $std1 -in $lbase

    set title [label $top1top.title -text "Calibration Files"]
    pack $title -in $top1top

    set blb [label $bias1.lb -text "BIAS" -width 15]
    set bfr [radiobutton $bias1.fw -text "Forwards" -variable Cred4Widgets(BIAS_MODE) -value "FORWARDS"]
    set bba [radiobutton $bias1.ba -text "Backwards" -variable Cred4Widgets(BIAS_MODE) -value "BACKWARDS"]
    set bbo [radiobutton $bias1.bo -text "Each Way" -variable Cred4Widgets(BIAS_MODE) -value "BOTH"]
    set bsp [radiobutton $bias1.sp -text "Specific File" -variable Cred4Widgets(BIAS_MODE) -value "SPECIFIED"]
    set bfi [entry $bias1.bfi -textvariable Cred4Widgets(SPEC_BIAS) -width 20]
    pack $blb $bfr $bba $bbo $bsp $bfi -in $bias1 -side left -pady 2m
    set Cred4Widgets(BIAS_MODE) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.bias_mode]]]
    set Cred4Widgets(SPEC_BIAS) [string tolower [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.specified_bias]]]
    if {$Cred4Widgets(SPEC_BIAS)=="royymmdd_oooo"} {set Cred4Widgets(SPEC_BIAS) ro$env(CGS4_DATE)_oooo}

    set dlb [label $dark1.lb -text "DARK" -width 15]
    set dfr [radiobutton $dark1.fw -text "Forwards" -variable Cred4Widgets(DARK_MODE) -value "FORWARDS"]
    set dba [radiobutton $dark1.ba -text "Backwards" -variable Cred4Widgets(DARK_MODE) -value "BACKWARDS"]
    set dbo [radiobutton $dark1.bo -text "Each Way" -variable Cred4Widgets(DARK_MODE) -value "BOTH"]
    set dsp [radiobutton $dark1.sp -text "Specific File" -variable Cred4Widgets(DARK_MODE) -value "SPECIFIED"]
    set dfi [entry $dark1.bfi -textvariable Cred4Widgets(SPEC_DARK) -width 20]
    pack $dlb $dfr $dba $dbo $dsp $dfi -in $dark1 -side left -pady 2m
    set Cred4Widgets(DARK_MODE) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.dark_mode]]]
    set Cred4Widgets(SPEC_DARK) [string tolower [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.specified_dark]]]
    if {$Cred4Widgets(SPEC_DARK)=="royymmdd_oooo"} {set Cred4Widgets(SPEC_DARK) ro$env(CGS4_DATE)_oooo}

    set flb [label $flat1.lb -text "FLAT" -width 15]
    set ffr [radiobutton $flat1.fw -text "Forwards" -variable Cred4Widgets(FLAT_MODE) -value "FORWARDS"]
    set fba [radiobutton $flat1.ba -text "Backwards" -variable Cred4Widgets(FLAT_MODE) -value "BACKWARDS"]
    set fbo [radiobutton $flat1.bo -text "Each Way" -variable Cred4Widgets(FLAT_MODE) -value "BOTH"]
    set fsp [radiobutton $flat1.sp -text "Specific File" -variable Cred4Widgets(FLAT_MODE) -value "SPECIFIED"]
    set ffi [entry $flat1.bfi -textvariable Cred4Widgets(SPEC_FLAT) -width 20]
    pack $flb $ffr $fba $fbo $fsp $ffi -in $flat1 -side left -pady 2m
    set Cred4Widgets(FLAT_MODE) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.flat_mode]]]
    set Cred4Widgets(SPEC_FLAT) [string tolower [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.specified_flat]]]
    if {$Cred4Widgets(SPEC_FLAT)=="royymmdd_oooo"} {set Cred4Widgets(SPEC_FLAT) ro$env(CGS4_DATE)_oooo}

    set clb [label $calib1.lb -text "ARC" -width 15]
    set cfr [radiobutton $calib1.fw -text "Forwards" -variable Cred4Widgets(CALIB_MODE) -value "FORWARDS"]
    set cba [radiobutton $calib1.ba -text "Backwards" -variable Cred4Widgets(CALIB_MODE) -value "BACKWARDS"]
    set cbo [radiobutton $calib1.bo -text "Each Way" -variable Cred4Widgets(CALIB_MODE) -value "BOTH"]
    set csp [radiobutton $calib1.sp -text "Specific File" -variable Cred4Widgets(CALIB_MODE) -value "SPECIFIED"]
    set cfi [entry $calib1.bfi -textvariable Cred4Widgets(SPEC_CALIB) -width 20]
    pack $clb $cfr $cba $cbo $csp $cfi -in $calib1 -side left -pady 2m
    set Cred4Widgets(CALIB_MODE) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.calib_mode]]]
    set Cred4Widgets(SPEC_CALIB) [string tolower [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.specified_calib]]]
    if {$Cred4Widgets(SPEC_CALIB)=="cayymmdd_oooo"} {set Cred4Widgets(SPEC_CALIB) ca$env(CGS4_DATE)_oooo}

    set slb [label $std1.lb -text "STANDARD" -width 15]
    set sfr [radiobutton $std1.fw -text "Forwards" -variable Cred4Widgets(STANDARD_MODE) -value "FORWARDS"]
    set sba [radiobutton $std1.ba -text "Backwards" -variable Cred4Widgets(STANDARD_MODE) -value "BACKWARDS"]
    set sbo [radiobutton $std1.bo -text "Each Way" -variable Cred4Widgets(STANDARD_MODE) -value "BOTH"]
    set ssp [radiobutton $std1.sp -text "Specific File" -variable Cred4Widgets(STANDARD_MODE) -value "SPECIFIED"]
    set sfi [entry $std1.bfi -textvariable Cred4Widgets(SPEC_STD) -width 20]
    pack $slb $sfr $sba $sbo $ssp $sfi -in $std1 -side left -pady 2m
    set Cred4Widgets(STANDARD_MODE) [string toupper [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.standard_mode]]]
    set Cred4Widgets(SPEC_STD) [string tolower [string trim [nbs get ${Cred4NoticeBoard}.miscellaneous.specified_std]]]
    if {$Cred4Widgets(SPEC_STD)=="styymmdd_gggg"} {set Cred4Widgets(SPEC_STD) st$env(CGS4_DATE)_gggg}

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]
    if {$bv == 0} {
      cgs4drCursor watch red white
      cgs4drClear $taskname

#   Set the bad pixel mask and linearisation file
      set file [string trim [$Cred4Widgets(MASK) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.mask $file
      set file [string trim [$Cred4Widgets(LINCOEFFS) get]]
      nbs put ${Cred4NoticeBoard}.miscellaneous.lincoeffs $file

#   Set the flat field parameters
      set order [string trim [$Cred4Widgets(ORDER) get]]
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.order $order
      set boxsize [string trim [$Cred4Widgets(BOXSIZE) get]]
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.boxsize $boxsize
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.method $Cred4Widgets(FF_METHOD)

#   Set the wavelength calibration method
      nbs put ${Cred4NoticeBoard}.reduction.to_wavelength.method $Cred4Widgets(LAMBDA_METHOD)

#   Set the file selection parameters
      nbs put ${Cred4NoticeBoard}.miscellaneous.bias_mode $Cred4Widgets(BIAS_MODE)
      if {$Cred4Widgets(BIAS_MODE)=="SPECIFIED"} {
        set file [string trim [$bfi get]]
        if {$file=="royymmdd_oooo" || $file=="ro$env(CGS4_DATE)_oooo"} {
          set message "cred4RedParams error : A BIAS has not been specified properly!"
          cgs4drInform $taskname $message
        } else {
          nbs put ${Cred4NoticeBoard}.miscellaneous.specified_bias $file
        }
      }
      nbs put ${Cred4NoticeBoard}.miscellaneous.dark_mode $Cred4Widgets(DARK_MODE)
      if {$Cred4Widgets(DARK_MODE)=="SPECIFIED"} {
        set file [string trim [$dfi get]]
        if {$file=="royymmdd_oooo" || $file=="ro$env(CGS4_DATE)_oooo"} {
          set message "cred4RedParams error : A DARK has not been specified properly!"
          cgs4drInform $taskname $message
        } else {
          nbs put ${Cred4NoticeBoard}.miscellaneous.specified_dark $file
        }
      }
      nbs put ${Cred4NoticeBoard}.miscellaneous.flat_mode $Cred4Widgets(FLAT_MODE)
      if {$Cred4Widgets(FLAT_MODE)=="SPECIFIED"} {
        set file [string trim [$ffi get]]
        if {$file=="royymmdd_oooo" || $file=="ro$env(CGS4_DATE)_oooo"} {
          set message "cred4RedParams error : A FLAT has not been specified properly!"
          cgs4drInform $taskname $message
        } else {
          nbs put ${Cred4NoticeBoard}.miscellaneous.specified_flat $file
        }
      }
      nbs put ${Cred4NoticeBoard}.miscellaneous.calib_mode $Cred4Widgets(CALIB_MODE)
      if {$Cred4Widgets(CALIB_MODE)=="SPECIFIED"} {
        set file [string trim [$cfi get]]
        if {$file=="cayymmdd_oooo" || $file=="ca$env(CGS4_DATE)_oooo"} {
          set message "cred4RedParams error : A CALIB has not been specified properly!"
          cgs4drInform $taskname $message
        } else {
          nbs put ${Cred4NoticeBoard}.miscellaneous.specified_calib $file
        }
      }
      nbs put ${Cred4NoticeBoard}.miscellaneous.standard_mode $Cred4Widgets(STANDARD_MODE)
      if {$Cred4Widgets(STANDARD_MODE)=="SPECIFIED"} {
        set file [string trim [$sfi get]]
        if {$file=="styymmdd_gggg" || $file=="st$env(CGS4_DATE)_gggg"} {
          set message "cred4RedParams error : A STANDARD has not been specified properly!"
          cgs4drInform $taskname $message
        } else {
          nbs put ${Cred4NoticeBoard}.miscellaneous.specified_std $file
        }
      }
    } elseif {$bv == 1} {
      cgs4drCursor watch red white
      cgs4drClear $taskname
      nbs put ${Cred4NoticeBoard}.miscellaneous.mask fpa46_short
      nbs put ${Cred4NoticeBoard}.miscellaneous.lincoeffs #
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.method POLYFIT
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.order 3
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.boxsize 5
      nbs put ${Cred4NoticeBoard}.reduction.to_wavelength.method ESTIMATED
      nbs put ${Cred4NoticeBoard}.miscellaneous.bias_mode BOTH
      nbs put ${Cred4NoticeBoard}.miscellaneous.specified_bias ro$env(CGS4_DATE)_oooo
      nbs put ${Cred4NoticeBoard}.miscellaneous.dark_mode BOTH
      nbs put ${Cred4NoticeBoard}.miscellaneous.specified_dark ro$env(CGS4_DATE)_oooo
      nbs put ${Cred4NoticeBoard}.miscellaneous.flat_mode BOTH
      nbs put ${Cred4NoticeBoard}.miscellaneous.specified_flat ro$env(CGS4_DATE)_oooo
      nbs put ${Cred4NoticeBoard}.miscellaneous.calib_mode BOTH
      nbs put ${Cred4NoticeBoard}.miscellaneous.specified_calib ca$env(CGS4_DATE)_oooo
      nbs put ${Cred4NoticeBoard}.miscellaneous.standard_mode BOTH
      nbs put ${Cred4NoticeBoard}.miscellaneous.specified_std st$env(CGS4_DATE)_gggg
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .cred4Dialogue
}
