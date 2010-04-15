proc buildCred4Widgets w {
#+
# This procedure builds the cred4 interface widget tree. The names of the
# "active" widgets are stored in the global array "Cred4Widgets" so that
# the names of the widgets can be changed without effecting the rest of
# the application.
#
# The only widget bindings set up here are the connection between the
# message text window and its scroll bar.
#
# The return value is the name of the frame widget that contains the
# widget tree.
#-
    global Cred4NoticeBoard
    global Cred4Widgets

# Create frame for the widget tree.
    set mainFrame [frame $w.main]

# Build panel layout frames.
    set topFrame [frame $mainFrame.ft]
    set middleFrame [frame $mainFrame.fm -relief sunken -bd 2]
    set bottomFrame [frame $mainFrame.fb -relief sunken -bd 2]
    pack $topFrame $middleFrame $bottomFrame -fill both -expand yes -side top

# Create scrolling region for output. The width of the text widget is set to
# zero so that it expands to width of the top panel when it is packed.
    set Cred4Widgets(SB1) [scrollbar $topFrame.scrollbar -orient vertical -relief sunken -bd 2]
    set Cred4Widgets(OUTPUT) [text $topFrame.text -state disabled -wrap word -relief sunken -bd 2 -width 0]
    $Cred4Widgets(SB1) configure -command "$Cred4Widgets(OUTPUT) yview"
    $Cred4Widgets(OUTPUT) configure -yscroll "$Cred4Widgets(SB1) set"
    pack $Cred4Widgets(SB1) -in $topFrame -side right -fill y
    pack $Cred4Widgets(OUTPUT) -in $topFrame -side right -fill both -expand yes

# Create DR status layout.
    set lmiddleFrame [frame $middleFrame.lef]
    set rmiddleFrame [frame $middleFrame.rig]
    pack $lmiddleFrame -in $middleFrame -side left -expand yes
    pack $rmiddleFrame -in $middleFrame -side right -expand yes
    set Cred4Widgets(RSTAT) [label $rmiddleFrame.label -bg white -fg black -textvariable Cred4Widgets(REDUCTION_STATE) -width 20 \
      -font -adobe-helvetica-medium-o-normal--*-180-* -relief ridge -bd 2]
    pack $Cred4Widgets(RSTAT) -in $rmiddleFrame -side right -padx 4m -pady 2m
    set Cred4Widgets(REDUCTION_STATE) STOPPED

    set Cred4Widgets(DRPAUSE) [checkbutton $lmiddleFrame.pa -text "Pause" -variable Cred4Widgets(PAUSE)]
    set Cred4Widgets(DRSTART) [button $lmiddleFrame.st -text "START" -fg blue -relief ridge -bd 2]
    set Cred4Widgets(DRSTOP) [button $lmiddleFrame.sp -text "STOP" -fg blue -relief ridge]
    pack $Cred4Widgets(DRSTART) -in $lmiddleFrame -side left -padx 4m -pady 2m
    pack $Cred4Widgets(DRPAUSE) -in $lmiddleFrame -side right -padx 4m -pady 2m
    set Cred4Widgets(PAUSE) 0
    $Cred4Widgets(RSTAT) configure -fg black -bg red

# Create some action buttons in the bottom frame
    set tbf [frame $bottomFrame.tbf]
    set mbf [frame $bottomFrame.mbf]
    set bbf [frame $bottomFrame.bbf]
    pack $tbf $mbf $bbf -in $bottomFrame -side top -expand yes -fill both

    set Cred4Widgets(SETUP) [button $tbf.as -text "Setup"]
    set Cred4Widgets(DISPLAY) [button $tbf.di -text "Display"]
    set Cred4Widgets(CONFIGS) [button $tbf.dr -text "Configs"]
    set Cred4Widgets(DRMASKS) [button $tbf.dm -text "Masks"]
    pack $Cred4Widgets(SETUP) $Cred4Widgets(DISPLAY) $Cred4Widgets(CONFIGS) $Cred4Widgets(DRMASKS) \
      -in $tbf -expand yes -fill both -side left

    set Cred4Widgets(BIAS) [button $mbf.as -text "Bias"]
    set Cred4Widgets(DARK) [button $mbf.di -text "Dark"]
    set Cred4Widgets(FLAT) [button $mbf.dr -text "Flat"]
    set Cred4Widgets(CALIB) [button $mbf.dm -text "Calibration"]
    pack $Cred4Widgets(BIAS) $Cred4Widgets(DARK) $Cred4Widgets(FLAT) $Cred4Widgets(CALIB) \
      -in $mbf -expand yes -fill both -side left

    set Cred4Widgets(STANDARD) [button $bbf.as -text "Standard"]
    set Cred4Widgets(SKY) [button $bbf.di -text "Sky"]
    set Cred4Widgets(POLYSKY) [button $bbf.dr -text "Polysky"]
    set Cred4Widgets(EXTRACT) [button $bbf.dm -text "Extract"]
    pack $Cred4Widgets(STANDARD) $Cred4Widgets(SKY) $Cred4Widgets(POLYSKY) $Cred4Widgets(EXTRACT) \
      -in $bbf -expand yes -fill both -side left
    return $mainFrame
}
