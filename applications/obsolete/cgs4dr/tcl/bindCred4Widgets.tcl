proc bindCred4Widgets {taskname} {
#+
# This procedure binds the actions to all the "active" widgets in the
# cred4 interface.
#-
    global env
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Bind some help commands
    bind $Cred4Widgets(SB1)             <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drScroll.html"
    bind $Cred4Widgets(OUTPUT)          <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drTextpane.html"
    bind $Cred4Widgets(DRPAUSE)         <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4Drpause.html"
    bind $Cred4Widgets(RSTAT)           <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4Drstate.html"
    bind $Cred4Widgets(REDUCTION_STATE) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4Drstate.html"
    bind $Cred4Widgets(DRSTART)         <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4Drstart.html"
    bind $Cred4Widgets(DRSTOP)          <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4Drstop.html"

    bind $Cred4Widgets(SETUP)     <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SetupBox1.html"
    bind $Cred4Widgets(DISPLAY)   <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DisplayBox1.html"
    bind $Cred4Widgets(CONFIGS)   <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ConfigsBox1.html"
    bind $Cred4Widgets(DRMASKS)   <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4MasksBox1.html"
    bind $Cred4Widgets(BIAS)      <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4BiasBox1.html"
    bind $Cred4Widgets(DARK)      <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4DarkBox1.html"
    bind $Cred4Widgets(FLAT)      <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4FlatBox1.html"
    bind $Cred4Widgets(CALIB)     <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4CalibBox1.html"
    bind $Cred4Widgets(STANDARD)  <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4StandardBox1.html"
    bind $Cred4Widgets(SKY)       <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4SkyBox1.html"
    bind $Cred4Widgets(POLYSKY)   <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4PolyskyBox1.html"
    bind $Cred4Widgets(EXTRACT)   <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ExtractBox1.html"

# Command buttons.
    $Cred4Widgets(DRSTART) configure -command "cred4Drstart $taskname"
    $Cred4Widgets(DRSTOP) configure -command "cred4Drstop $taskname"
    $Cred4Widgets(SETUP) configure -command "cred4Setup"
    $Cred4Widgets(DISPLAY) configure -command "cred4Display"
    $Cred4Widgets(CONFIGS) configure -command "cred4Configs $taskname"
    $Cred4Widgets(DRMASKS) configure -command "cred4Masks $taskname"
    $Cred4Widgets(BIAS) configure -command "cred4Bias $taskname"
    $Cred4Widgets(DARK) configure -command "cred4Dark $taskname"
    $Cred4Widgets(FLAT) configure -command "cred4Flat $taskname"
    $Cred4Widgets(CALIB) configure -command "cred4Calib $taskname"
    $Cred4Widgets(STANDARD) configure -command "cred4Standard $taskname"
    $Cred4Widgets(SKY) configure -command "cred4Sky $taskname"
    $Cred4Widgets(POLYSKY) configure -command "cred4Polysky $taskname"
    $Cred4Widgets(EXTRACT) configure -command "cred4Extract $taskname"

# Traces and monitors
    nbs monitor ${Cred4NoticeBoard}.flags.pause_reduction Cred4Widgets(PAUSE)
    nbs start 100
    trace variable Cred4Widgets(PAUSE) w "cred4PauseContinue $taskname"
    trace variable Cred4Widgets(POE) w "cred4PauseOnError $taskname"
    set Cred4Widgets(POE) 1
    trace variable Cred4Widgets(VERBOSE) w "cgs4drVerbose $taskname"
    set Cred4Widgets(VERBOSE) 0
    set Cred4Widgets(DTYPE) "NONE"
}
