proc cred4Setup {} {

# Get some default values
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box.
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Data Reduction Setup"  0 OK Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

# Create panel layout
    set top    [frame $frame.top -relief sunken -bd 2]
    pack $top -side top -fill both -expand yes

# Create frames for each setup item
    set fr1 [frame $top.fr1]
    set fr2 [frame $top.fr2]
    set fr3 [frame $top.fr3]
    set fr4 [frame $top.fr4]
    set fr5 [frame $top.fr5]
    set fr6 [frame $top.fr6]
    set fr7 [frame $top.fr7]
    set fr8 [frame $top.fr8]
    set fra [frame $top.fra]
    set frb [frame $top.frb]
    set frc [frame $top.frc]
    set frd [frame $top.frd]
    pack $fr1 $fr2 $fr3 $fr4 $fr5 $fr6 $fr7 $fr8 $fra $frb $frc $frd -in $top -expand yes -fill x

# Set labels for each item
    set lb1 [label $fr1.lb1 -text "Coadd Integrations            "]
    set lb2 [label $fr2.lb2 -text "Subtract BIAS Frame           "]
    set lb3 [label $fr3.lb3 -text "Subtract DARK Frame           "]
    set lb4 [label $fr4.lb4 -text "Normalise FLAT Field          "]
    set lb5 [label $fr5.lb5 -text "Divide by FLAT Field          "]
    set lb6 [label $fr6.lb6 -text "Add Observations into Groups  "]
    set lb7 [label $fr7.lb7 -text "Archive Observations          "]
    set lb8 [label $fr8.lb8 -text "File Observations             "]
    set lba [label $fra.lba -text "Wavelength Calibrate          "]
    set lbb [label $frb.lbb -text "Divide by STANDARD Source     "]
    set lbc [label $frc.lbc -text "Extract (Nodded) Spectrum     "]
    set lbd [label $frd.lbd -text "Automatic Line Fitting        "]

    bind $lb1 <Button-2> "cred4Update cred4Setup ALL"
    bind $lb2 <Button-2> "cred4Update cred4Setup ALL"
    bind $lb3 <Button-2> "cred4Update cred4Setup ALL"
    bind $lb4 <Button-2> "cred4Update cred4Setup ALL"
    bind $lb5 <Button-2> "cred4Update cred4Setup ALL"
    bind $lb6 <Button-2> "cred4Update cred4Setup ALL"
    bind $lb7 <Button-2> "cred4Update cred4Setup ALL"
    bind $lb8 <Button-2> "cred4Update cred4Setup ALL"
    bind $lba <Button-2> "cred4Update cred4Setup ALL"
    bind $lbb <Button-2> "cred4Update cred4Setup ALL"
    bind $lbc <Button-2> "cred4Update cred4Setup ALL"
    bind $lbd <Button-2> "cred4Update cred4Setup ALL"

    bind $lb1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lb2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lb3 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lb4 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lb5 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lb6 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lb7 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lb8 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lbb <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lbc <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $lbd <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"

# Get the defaults from nbs
    set Cred4Widgets(ADD_INT)        [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.add_int.execute]]]
    set Cred4Widgets(SUBTRACT_BIAS)  [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.subtract_bias.execute]]]
    set Cred4Widgets(SUBTRACT_DARK)  [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.subtract_dark.execute]]]
    set Cred4Widgets(NORMALISE_FF)   [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.normalise_ff.execute]]]
    set Cred4Widgets(DIVIDE_BY_FF)   [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.divide_by_ff.execute]]]
    set Cred4Widgets(ADD_OBS)        [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.add_obs.execute]]]
    set Cred4Widgets(ARCHIVE_OBS)    [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.archive_obs.execute]]]
    set Cred4Widgets(FILE_OBS)       [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.file_obs.execute]]]
    set Cred4Widgets(WAVELENGTH_CAL) [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.to_wavelength.execute]]]
    set Cred4Widgets(DIVIDE_BY_STD)  [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.divide_by_std.execute]]]
    set Cred4Widgets(EXTRACT_SPC)    [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.extract_spc.execute]]]
    set Cred4Widgets(AUTOFIT)        [string trim [string toupper [nbs get ${Cred4NoticeBoard}.reduction.autofit.execute]]]

# Set radio buttons (YES, NO, ASK) for each item
    set rb1y [radiobutton $fr1.rb1y -text "Yes" -variable Cred4Widgets(ADD_INT) -value "YES"]
    set rb1n [radiobutton $fr1.rb1n -text "No"  -variable Cred4Widgets(ADD_INT) -value "NO"]
    set rb1a [radiobutton $fr1.rb1a -text "Ask" -variable Cred4Widgets(ADD_INT) -value "ASK"]
    set rb2y [radiobutton $fr2.rb2y -text "Yes" -variable Cred4Widgets(SUBTRACT_BIAS) -value "YES"]
    set rb2n [radiobutton $fr2.rb2n -text "No" -variable Cred4Widgets(SUBTRACT_BIAS) -value "NO"]
    set rb2a [radiobutton $fr2.rb2a -text "Ask" -variable Cred4Widgets(SUBTRACT_BIAS) -value "ASK"]
    set rb3y [radiobutton $fr3.rb3y -text "Yes" -variable Cred4Widgets(SUBTRACT_DARK) -value "YES"]
    set rb3n [radiobutton $fr3.rb3n -text "No" -variable Cred4Widgets(SUBTRACT_DARK) -value "NO"]
    set rb3a [radiobutton $fr3.rb3a -text "Ask" -variable Cred4Widgets(SUBTRACT_DARK) -value "ASK"]
    set rb4y [radiobutton $fr4.rb4y -text "Yes" -variable Cred4Widgets(NORMALISE_FF) -value "YES"]
    set rb4n [radiobutton $fr4.rb4n -text "No"  -variable Cred4Widgets(NORMALISE_FF) -value "NO"]
    set rb4a [radiobutton $fr4.rb4a -text "Ask" -variable Cred4Widgets(NORMALISE_FF) -value "ASK"]
    set rb5y [radiobutton $fr5.rb5y -text "Yes" -variable Cred4Widgets(DIVIDE_BY_FF) -value "YES"]
    set rb5n [radiobutton $fr5.rb5n -text "No" -variable Cred4Widgets(DIVIDE_BY_FF) -value "NO"]
    set rb5a [radiobutton $fr5.rb5a -text "Ask" -variable Cred4Widgets(DIVIDE_BY_FF) -value "ASK"]
    set rb6y [radiobutton $fr6.rb6y -text "Yes" -variable Cred4Widgets(ADD_OBS) -value "YES"]
    set rb6n [radiobutton $fr6.rb6n -text "No"  -variable Cred4Widgets(ADD_OBS) -value "NO"]
    set rb6a [radiobutton $fr6.rb6a -text "Ask" -variable Cred4Widgets(ADD_OBS) -value "ASK"]
    set rb7y [radiobutton $fr7.rb7y -text "Yes" -variable Cred4Widgets(ARCHIVE_OBS) -value "YES"]
    set rb7n [radiobutton $fr7.rb7n -text "No"  -variable Cred4Widgets(ARCHIVE_OBS) -value "NO"]
    set rb7a [radiobutton $fr7.rb7a -text "Ask" -variable Cred4Widgets(ARCHIVE_OBS) -value "ASK"]
    set rb8y [radiobutton $fr8.rb8y -text "Yes" -variable Cred4Widgets(FILE_OBS) -value "YES"]
    set rb8n [radiobutton $fr8.rb8n -text "No"  -variable Cred4Widgets(FILE_OBS) -value "NO"]
    set rb8a [radiobutton $fr8.rb8a -text "Ask" -variable Cred4Widgets(FILE_OBS) -value "ASK"]
    set rbay [radiobutton $fra.rbay -text "Yes" -variable Cred4Widgets(WAVELENGTH_CAL) -value "YES"]
    set rban [radiobutton $fra.rban -text "No" -variable Cred4Widgets(WAVELENGTH_CAL) -value "NO"]
    set rbaa [radiobutton $fra.rbaa -text "Ask" -variable Cred4Widgets(WAVELENGTH_CAL) -value "ASK"]
    set rbby [radiobutton $frb.rbby -text "Yes" -variable Cred4Widgets(DIVIDE_BY_STD) -value "YES"]
    set rbbn [radiobutton $frb.rbbn -text "No" -variable Cred4Widgets(DIVIDE_BY_STD) -value "NO"]
    set rbba [radiobutton $frb.rbba -text "Ask" -variable Cred4Widgets(DIVIDE_BY_STD) -value "ASK"]
    set rbcy [radiobutton $frc.rbcy -text "Yes" -variable Cred4Widgets(EXTRACT_SPC) -value "YES"]
    set rbcn [radiobutton $frc.rbcn -text "No" -variable Cred4Widgets(EXTRACT_SPC) -value "NO"]
    set rbca [radiobutton $frc.rbca -text "Ask" -variable Cred4Widgets(EXTRACT_SPC) -value "ASK"]
    set rbdy [radiobutton $frd.rbdy -text "Yes" -variable Cred4Widgets(AUTOFIT) -value "YES"]
    set rbdn [radiobutton $frd.rbdn -text "No" -variable Cred4Widgets(AUTOFIT) -value "NO"]
    set rbda [radiobutton $frd.rbda -text "Ask" -variable Cred4Widgets(AUTOFIT) -value "ASK"]

    bind $rb1y <Button-2> "cred4Update cred4Setup ADD_INT"
    bind $rb2y <Button-2> "cred4Update cred4Setup SUBTRACT_BIAS"
    bind $rb3y <Button-2> "cred4Update cred4Setup SUBTRACT_DARK"
    bind $rb4y <Button-2> "cred4Update cred4Setup NORMALISE_FF"
    bind $rb5y <Button-2> "cred4Update cred4Setup DIVIDE_BY_FF"
    bind $rb6y <Button-2> "cred4Update cred4Setup ADD_OBS"
    bind $rb7y <Button-2> "cred4Update cred4Setup ARCHIVE_OBS"
    bind $rb8y <Button-2> "cred4Update cred4Setup FILE_OBS"
    bind $rbay <Button-2> "cred4Update cred4Setup WAVELENGTH_CAL"
    bind $rbby <Button-2> "cred4Update cred4Setup DIVIDE_BY_STD"
    bind $rbcy <Button-2> "cred4Update cred4Setup EXTRACT_SPC"
    bind $rbdy <Button-2> "cred4Update cred4Setup AUTOFIT"
    bind $rb1n <Button-2> "cred4Update cred4Setup ADD_INT"
    bind $rb2n <Button-2> "cred4Update cred4Setup SUBTRACT_BIAS"
    bind $rb3n <Button-2> "cred4Update cred4Setup SUBTRACT_DARK"
    bind $rb4n <Button-2> "cred4Update cred4Setup NORMALISE_FF"
    bind $rb5n <Button-2> "cred4Update cred4Setup DIVIDE_BY_FF"
    bind $rb6n <Button-2> "cred4Update cred4Setup ADD_OBS"
    bind $rb7n <Button-2> "cred4Update cred4Setup ARCHIVE_OBS"
    bind $rb8n <Button-2> "cred4Update cred4Setup FILE_OBS"
    bind $rban <Button-2> "cred4Update cred4Setup WAVELENGTH_CAL"
    bind $rbbn <Button-2> "cred4Update cred4Setup DIVIDE_BY_STD"
    bind $rbcn <Button-2> "cred4Update cred4Setup EXTRACT_SPC"
    bind $rbdn <Button-2> "cred4Update cred4Setup AUTOFIT"
    bind $rb1a <Button-2> "cred4Update cred4Setup ADD_INT"
    bind $rb2a <Button-2> "cred4Update cred4Setup SUBTRACT_BIAS"
    bind $rb3a <Button-2> "cred4Update cred4Setup SUBTRACT_DARK"
    bind $rb4a <Button-2> "cred4Update cred4Setup NORMALISE_FF"
    bind $rb5a <Button-2> "cred4Update cred4Setup DIVIDE_BY_FF"
    bind $rb6a <Button-2> "cred4Update cred4Setup ADD_OBS"
    bind $rb7a <Button-2> "cred4Update cred4Setup ARCHIVE_OBS"
    bind $rb8a <Button-2> "cred4Update cred4Setup FILE_OBS"
    bind $rbaa <Button-2> "cred4Update cred4Setup WAVELENGTH_CAL"
    bind $rbba <Button-2> "cred4Update cred4Setup DIVIDE_BY_STD"
    bind $rbca <Button-2> "cred4Update cred4Setup EXTRACT_SPC"
    bind $rbda <Button-2> "cred4Update cred4Setup AUTOFIT"

    bind $rb1y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb2y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb3y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb4y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb5y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb6y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb7y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb8y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbay <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbby <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbcy <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbdy <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb1n <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb2n <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb3n <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb4n <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb5n <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb6n <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb7n <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb8n <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rban <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbbn <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbcn <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbdn <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb1a <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb2a <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb3a <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb4a <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb5a <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb6a <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb7a <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rb8a <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbaa <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbba <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbca <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $rbda <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"

# Pack them in
    pack $lb1 -in $fr1 -side left -ipadx 15m
    pack $rb1a $rb1n $rb1y -in $fr1 -side right
    pack $lb2 -in $fr2 -side left -ipadx 15m
    pack $rb2a $rb2n $rb2y -in $fr2 -side right
    pack $lb3 -in $fr3 -side left -ipadx 15m
    pack $rb3a $rb3n $rb3y -in $fr3 -side right
    pack $lb4 -in $fr4 -side left -ipadx 15m
    pack $rb4a $rb4n $rb4y -in $fr4 -side right
    pack $lb5 -in $fr5 -side left -ipadx 15m
    pack $rb5a $rb5n $rb5y -in $fr5 -side right
    pack $lb6 -in $fr6 -side left -ipadx 15m
    pack $rb6a $rb6n $rb6y -in $fr6 -side right
    pack $lb7 -in $fr7 -side left -ipadx 15m
    pack $rb7a $rb7n $rb7y -in $fr7 -side right
    pack $lb8 -in $fr8 -side left -ipadx 15m
    pack $rb8a $rb8n $rb8y -in $fr8 -side right
    pack $lba -in $fra -side left -ipadx 15m
    pack $rbaa $rban $rbay -in $fra -side right
    pack $lbb -in $frb -side left -ipadx 15m
    pack $rbba $rbbn $rbby -in $frb -side right
    pack $lbc -in $frc -side left -ipadx 15m
    pack $rbca $rbcn $rbcy -in $frc -side right
    pack $lbd -in $frd -side left -ipadx 15m
    pack $rbda $rbdn $rbdy -in $frd -side right

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]

# If OK, set them using nbs
    if {$bv == 0} {
      cgs4drCursor watch red white
      nbs put ${Cred4NoticeBoard}.reduction.add_int.execute $Cred4Widgets(ADD_INT)
      nbs put ${Cred4NoticeBoard}.reduction.subtract_bias.execute $Cred4Widgets(SUBTRACT_BIAS)
      nbs put ${Cred4NoticeBoard}.reduction.subtract_dark.execute $Cred4Widgets(SUBTRACT_DARK)
      nbs put ${Cred4NoticeBoard}.reduction.normalise_ff.execute $Cred4Widgets(NORMALISE_FF)
      nbs put ${Cred4NoticeBoard}.reduction.divide_by_ff.execute $Cred4Widgets(DIVIDE_BY_FF)
      nbs put ${Cred4NoticeBoard}.reduction.add_obs.execute $Cred4Widgets(ADD_OBS)
      nbs put ${Cred4NoticeBoard}.reduction.archive_obs.execute $Cred4Widgets(ARCHIVE_OBS)
      nbs put ${Cred4NoticeBoard}.reduction.file_obs.execute $Cred4Widgets(FILE_OBS)
      nbs put ${Cred4NoticeBoard}.reduction.to_wavelength.execute $Cred4Widgets(WAVELENGTH_CAL)
      nbs put ${Cred4NoticeBoard}.reduction.divide_by_std.execute $Cred4Widgets(DIVIDE_BY_STD)
      nbs put ${Cred4NoticeBoard}.reduction.extract_spc.execute $Cred4Widgets(EXTRACT_SPC)
      nbs put ${Cred4NoticeBoard}.reduction.autofit.execute $Cred4Widgets(AUTOFIT)
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
}
