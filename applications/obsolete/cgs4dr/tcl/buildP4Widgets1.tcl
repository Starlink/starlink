proc buildP4Widgets1 w {
#+
# This procedure builds the p4 interface widget tree. The names of the
# "active" widgets are stored in the global array "P4Widgets" so that
# the names of the widgets can be changed without effecting the rest of
# the application.
#
# The return value is the name of the frame widget that contains the
# widget tree.
#
    global env
    global P4Widgets
    global cgs4drBitmaps
    global cgs4drHtml
    set mainFrame [frame $w.frame]
    pack $mainFrame -fill both -expand yes

# Data set name entry.
    set frame [frame $mainFrame.df]
    pack $frame -fill x -expand yes
    set label [label $frame.label -text Data -width 8]
    pack $label -side left
    set P4Widgets(DATA) [entry $frame.entry -relief sunken -bd 2]
    pack $P4Widgets(DATA) -side left -expand yes -fill x
    bind $label <Button-2> "p4Update buildP4Widgets1 ALL"
    bind $label <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4DataBox1.html"
    bind $P4Widgets(DATA) <Button-2> {$P4Widgets(DATA) delete 0 end; $P4Widgets(DATA) insert end \$RODIR/ro$env(CGS4_DATE)_oooo}
    bind $P4Widgets(DATA) <Double-Button-2> "$P4Widgets(DATA) delete 0 end"
    bind $P4Widgets(DATA) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4DataBox1.html"

# Port number
    set frame [frame $mainFrame.pf]
    pack $frame -fill x -expand yes
    set label [label $frame.label -text Port -width 8]
    pack $label -side left
    set port0 [radiobutton $frame.p0 -bitmap @$cgs4drBitmaps/port0.xbm -variable P4Widgets(PORT_NO) -value 0]
    set port1 [radiobutton $frame.p1 -bitmap @$cgs4drBitmaps/port1.xbm -variable P4Widgets(PORT_NO) -value 1]
    set port2 [radiobutton $frame.p2 -bitmap @$cgs4drBitmaps/port2.xbm -variable P4Widgets(PORT_NO) -value 2]
    set port3 [radiobutton $frame.p3 -bitmap @$cgs4drBitmaps/port3.xbm -variable P4Widgets(PORT_NO) -value 3]
    set port4 [radiobutton $frame.p4 -bitmap @$cgs4drBitmaps/port4.xbm -variable P4Widgets(PORT_NO) -value 4]
    set port5 [radiobutton $frame.p5 -bitmap @$cgs4drBitmaps/port5.xbm -variable P4Widgets(PORT_NO) -value 5]
    set port6 [radiobutton $frame.p6 -bitmap @$cgs4drBitmaps/port6.xbm -variable P4Widgets(PORT_NO) -value 6]
    set port7 [radiobutton $frame.p7 -bitmap @$cgs4drBitmaps/port7.xbm -variable P4Widgets(PORT_NO) -value 7]
    set port8 [radiobutton $frame.p8 -bitmap @$cgs4drBitmaps/port8.xbm -variable P4Widgets(PORT_NO) -value 8]
    pack $port0 $port1 $port2 $port3 $port4 $port5 $port6 $port7 $port8 -side left -ipadx 2m -fill x -expand yes
    bind $label <Button-2> "p4Update buildP4Widgets1 ALL"
    bind $label <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PortBox1.html"
    bind $port0 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PortBox1.html"
    bind $port1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PortBox1.html"
    bind $port2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PortBox1.html"
    bind $port3 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PortBox1.html"
    bind $port4 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PortBox1.html"
    bind $port5 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PortBox1.html"
    bind $port6 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PortBox1.html"
    bind $port7 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PortBox1.html"
    bind $port8 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PortBox1.html"
    bind $port0 <Button-2> "set P4Widgets(PORT_NO) 0"
    bind $port1 <Button-2> "set P4Widgets(PORT_NO) 0"
    bind $port2 <Button-2> "set P4Widgets(PORT_NO) 0"
    bind $port3 <Button-2> "set P4Widgets(PORT_NO) 0"
    bind $port4 <Button-2> "set P4Widgets(PORT_NO) 0"
    bind $port5 <Button-2> "set P4Widgets(PORT_NO) 0"
    bind $port6 <Button-2> "set P4Widgets(PORT_NO) 0"
    bind $port7 <Button-2> "set P4Widgets(PORT_NO) 0"
    bind $port8 <Button-2> "set P4Widgets(PORT_NO) 0"

# Data plane
    set frame [frame $mainFrame.plf]
    pack $frame -fill x -expand yes
    set label [label $frame.label -text Plane -width 8]
    pack $label -side left
    set data    [radiobutton $frame.data    -text "Data"    -value DATA    -variable P4Widgets(DISPLAY_PLANE) -width 8]
    set errors  [radiobutton $frame.errors  -text "Error"   -value ERRORS  -variable P4Widgets(DISPLAY_PLANE) -width 8]
    set quality [radiobutton $frame.quality -text "Quality" -value QUALITY -variable P4Widgets(DISPLAY_PLANE) -width 8]
    pack $data $errors $quality -side left
    bind $label <Button-2> "p4Update buildP4Widgets ALL"
    bind $data <Button-2> "set P4Widgets(DISPLAY_PLANE) DATA"
    bind $errors <Button-2> "set P4Widgets(DISPLAY_PLANE) DATA"
    bind $quality <Button-2> "set P4Widgets(DISPLAY_PLANE) DATA"
    bind $label <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PlaneBox1.html"
    bind $data <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PlaneBox1.html"
    bind $errors <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PlaneBox1.html"
    bind $quality <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PlaneBox1.html"

# Plot options
    set wa [checkbutton $frame.wa -variable P4Widgets(PLOT_WHOLE) -text "Whole Array"]
    set as [checkbutton $frame.as -variable P4Widgets(AUTOSCALE) -text "Autoscale"]
    pack $wa $as -side right
    bind $wa <Button-2> "set P4Widgets(PLOT_WHOLE) 1"
    bind $wa <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4WholeBox1.html"
    bind $as <Button-2> "set P4Widgets(AUTOSCALE) 1"
    bind $as <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4AutoscaleBox1.html"

# Panel for flipout
    set flipFrame1 [frame $mainFrame.flipFrame1 -bd 2 -relief sunken]
    pack $flipFrame1 -fill x -expand yes
    bind $flipFrame1 <Button-2> "p4Update buildP4Widgets1 ALL"
    bind $flipFrame1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4FlipBox1.html"

# Autoscale and Whole Array flip-out values
    set limitsFlipOut1 [frame $flipFrame1.wh]
    set xslab [label $limitsFlipOut1.xslab -text "X start"]
    set P4Widgets(XSTART) [entry $limitsFlipOut1.xs -width 5]
    set xelab [label $limitsFlipOut1.xelab -text "X end"]
    set P4Widgets(XEND) [entry $limitsFlipOut1.xe -width 5]
    set yslab [label $limitsFlipOut1.yslab -text "Y start"]
    set P4Widgets(YSTART) [entry $limitsFlipOut1.ys -width 5]
    set yelab [label $limitsFlipOut1.yelab -text "Y end"]
    set P4Widgets(YEND) [entry $limitsFlipOut1.ye -width 5]
    pack $P4Widgets(YEND) $yelab $P4Widgets(YSTART) $yslab $P4Widgets(XEND) $xelab $P4Widgets(XSTART) $xslab -side right
    bind $xslab <Button-2> "p4Update buildP4Widgets1 WHOLE"
    bind $xelab <Button-2> "p4Update buildP4Widgets1 WHOLE"
    bind $yslab <Button-2> "p4Update buildP4Widgets1 WHOLE"
    bind $yelab <Button-2> "p4Update buildP4Widgets1 WHOLE"
    bind $P4Widgets(XSTART) <Button-2> "$P4Widgets(XSTART) delete 0 end; $P4Widgets(XSTART) insert 0 1"
    bind $P4Widgets(XEND) <Button-2> "$P4Widgets(XEND) delete 0 end; $P4Widgets(XEND) insert 0 256"
    bind $P4Widgets(YSTART) <Button-2> "$P4Widgets(YSTART) delete 0 end; $P4Widgets(YSTART) insert 0 1"
    bind $P4Widgets(YEND) <Button-2> "$P4Widgets(YEND) delete 0 end; $P4Widgets(YEND) insert 0 256"
    bind $P4Widgets(XSTART) <Double-Button-2> "$P4Widgets(XSTART) delete 0 end"
    bind $P4Widgets(XEND) <Double-Button-2> "$P4Widgets(XEND) delete 0 end"
    bind $P4Widgets(YSTART) <Double-Button-2> "$P4Widgets(YSTART) delete 0 end"
    bind $P4Widgets(YEND) <Double-Button-2> "$P4Widgets(YEND) delete 0 end"
    bind $xslab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4WholeBox1.html"
    bind $xelab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4WholeBox1.html"
    bind $yslab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4WholeBox1.html"
    bind $yelab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4WholeBox1.html"
    bind $P4Widgets(XSTART) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4WholeBox1.html"
    bind $P4Widgets(XEND) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4WholeBox1.html"
    bind $P4Widgets(YSTART) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4WholeBox1.html"
    bind $P4Widgets(YEND) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4WholeBox1.html"

    set limitsFlipOut2 [frame $flipFrame1.as]
    set high [label $limitsFlipOut2.hl -text High]
    set P4Widgets(HIGH) [entry $limitsFlipOut2.high]
    set low [label $limitsFlipOut2.ll -text Low]
    set P4Widgets(LOW) [entry $limitsFlipOut2.low]
    pack $low $P4Widgets(LOW) $high $P4Widgets(HIGH) -side left
    bind $high <Button-2> "p4Update buildP4Widgets1 SCALE"
    bind $low <Button-2> "p4Update buildP4Widgets1 SCALE"
    bind $P4Widgets(HIGH) <Button-2> "$P4Widgets(HIGH) delete 0 end; $P4Widgets(HIGH) insert 0 1000.0"
    bind $P4Widgets(LOW) <Button-2> "$P4Widgets(LOW) delete 0 end; $P4Widgets(LOW) insert 0 0.0"
    bind $P4Widgets(HIGH) <Double-Button-2> "$P4Widgets(HIGH) delete 0 end"
    bind $P4Widgets(LOW) <Double-Button-2> "$P4Widgets(LOW) delete 0 end"
    bind $high <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4AutoscaleBox1.html"
    bind $low <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4AutoscaleBox1.html"
    bind $P4Widgets(HIGH) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4AutoscaleBox1.html"
    bind $P4Widgets(LOW) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4AutoscaleBox1.html"

# Set a trace on Autoscale and Whole Array
    #trace variable P4Widgets(PLOT_WHOLE) w "SetFlipOut $limitsFlipOut1"
    #trace variable P4Widgets(AUTOSCALE)  w "SetFlipOut $limitsFlipOut2"
    trace variable P4Widgets(PLOT_WHOLE) w "SetFlipOut $flipFrame1"
    trace variable P4Widgets(AUTOSCALE)  w "SetFlipOut $flipFrame1"

# Plot style
    set frame [frame $mainFrame.st]
    pack $frame -fill x -expand yes
    set label [label $frame.label -text Type -width 8]
    pack $label -side left
    set image [radiobutton $frame.image -text Image -value IMAGE -variable P4Widgets(DISPLAY_TYPE) -width 8]
    set graph [radiobutton $frame.graph -text Graph -value GRAPH  -variable P4Widgets(DISPLAY_TYPE) -width 8]
    set histogram [radiobutton $frame.histogram -text Histogram -value HISTOGRAM  -variable P4Widgets(DISPLAY_TYPE) -width 8]
    set overgraph [radiobutton $frame.overgraph -text Overgraph -value OVERGRAPH  -variable P4Widgets(DISPLAY_TYPE) -width 8]
    set surface [radiobutton $frame.surface -text Surface -value SURFACE  -variable P4Widgets(DISPLAY_TYPE) -width 8]
    set contour [radiobutton $frame.contour -text "Contour" -value CONTOUR  -variable P4Widgets(DISPLAY_TYPE) -width 8]
    pack $image $graph $histogram $overgraph $surface $contour  -side left -ipadx 2m -fill x -expand yes
    bind $label <Button-2> "p4Update buildP4Widgets1 ALL"
    bind $image <Button-2> "set P4Widgets(DISPLAY_TYPE) IMAGE"
    bind $graph <Button-2> "set P4Widgets(DISPLAY_TYPE) IMAGE"
    bind $overgraph <Button-2> "set P4Widgets(DISPLAY_TYPE) IMAGE"
    bind $histogram <Button-2> "set P4Widgets(DISPLAY_TYPE) IMAGE"
    bind $surface <Button-2> "set P4Widgets(DISPLAY_TYPE) IMAGE"
    bind $contour <Button-2> "set P4Widgets(DISPLAY_TYPE) IMAGE"
    bind $label <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4TypeBox1.html"
    bind $image <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4TypeBox1.html"
    bind $graph <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4TypeBox1.html"
    bind $overgraph <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4TypeBox1.html"
    bind $histogram <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4TypeBox1.html"
    bind $surface <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4TypeBox1.html"
    bind $contour <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4TypeBox1.html"

# Panel for flipouts
    set flipFrame2 [frame $mainFrame.flipFrame2 -bd 2 -relief sunken]
    pack $flipFrame2 -fill x -expand yes
    bind $flipFrame2 <Button-2> "p4Update buildP4Widgets1 ALL"
    bind $flipFrame2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4FlipBox1.html"

# Scrolling region for output
    set frame [frame $mainFrame.of]
    pack $frame -fill x -expand yes
    set scrollbar [scrollbar $frame.scrollbar -orient vertical -relief sunken -bd 2]
    set P4Widgets(OUTPUT) [text $frame.text -state disabled -wrap word -relief sunken -bd 2 -height 12 -width 0]
    $scrollbar configure -command "$P4Widgets(OUTPUT) yview"
    $P4Widgets(OUTPUT) configure -yscroll "$scrollbar set"
    pack $scrollbar -side right -fill y
    pack $P4Widgets(OUTPUT) -side right -fill x -expand yes
    bind $P4Widgets(OUTPUT) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drTextpane.html"
    bind $scrollbar <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cgs4drScroll.html"

# Action buttons
    set frame [frame $mainFrame.act]
    pack $frame -fill x -expand yes
    set P4Widgets(PLOT) [button $frame.plot -text Plot -fg blue -relief ridge -bd 2]
    #set P4Widgets(CLEAR) [button $frame.clear -text "Clear Port"]
    set P4Widgets(CURSOR) [button $frame.clear -text "Cursor"]
    set P4Widgets(SET) [button $frame.set -text "Set Port"]
    set P4Widgets(CONFIGS) [button $frame.attrib -text "Configs"]
    pack $P4Widgets(PLOT) $P4Widgets(CURSOR) $P4Widgets(SET) $P4Widgets(CONFIGS) -expand yes -side left -pady 2m
    bind $P4Widgets(PLOT) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4PlotBox1.html"
    #bind $P4Widgets(CLEAR) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ClearBox1.html"
    bind $P4Widgets(CURSOR) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4CursorBox1.html"
    bind $P4Widgets(SET) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4SetBox1.html"
    bind $P4Widgets(CONFIGS) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ConfigsBox1.html"

# Histogram attributes
    set histogramFlipOut [frame $flipFrame2.hflip]
    set xlabel [label $histogramFlipOut.xlab -text "X step"]
    set P4Widgets(HISTOGRAM_XSTEP) [entry $histogramFlipOut.xstep -width 5]
    set ylabel [label $histogramFlipOut.ylab -text "Y step"]
    set P4Widgets(HISTOGRAM_YSTEP) [entry $histogramFlipOut.ystep -width 5]
    set bitlab [label $histogramFlipOut.bitlab -text "Number of Bins"]
    set P4Widgets(HISTOGRAM_BINS) [entry $histogramFlipOut.bit -width 5]
    set smlab [label $histogramFlipOut.smlab -text "Smoothing Box"]
    set P4Widgets(HIST_SMOOTH) [entry $histogramFlipOut.smooth -width 5]
    pack $xlabel $P4Widgets(HISTOGRAM_XSTEP) $ylabel $P4Widgets(HISTOGRAM_YSTEP) \
	 $bitlab $P4Widgets(HISTOGRAM_BINS) $smlab $P4Widgets(HIST_SMOOTH) -side left -fill x -expand yes
    bind $xlabel <Button-2> "p4Update buildP4Widgets1 HISTOGRAM"
    bind $ylabel <Button-2> "p4Update buildP4Widgets1 HISTOGRAM"
    bind $bitlab <Button-2> "p4Update buildP4Widgets1 HISTOGRAM"
    bind $smlab  <Button-2> "p4Update buildP4Widgets1 HISTOGRAM"
    bind $P4Widgets(HISTOGRAM_XSTEP)  <Button-2> "$P4Widgets(HISTOGRAM_XSTEP) delete 0 end; $P4Widgets(HISTOGRAM_XSTEP) insert 0 1"
    bind $P4Widgets(HISTOGRAM_YSTEP)  <Button-2> "$P4Widgets(HISTOGRAM_YSTEP) delete 0 end; $P4Widgets(HISTOGRAM_YSTEP) insert 0 1"
    bind $P4Widgets(HISTOGRAM_BINS)  <Button-2> "$P4Widgets(HISTOGRAM_BINS) delete 0 end; $P4Widgets(HISTOGRAM_BINS) insert 0 50"
    bind $P4Widgets(HIST_SMOOTH)  <Button-2> "$P4Widgets(HIST_SMOOTH) delete 0 end; $P4Widgets(HIST_SMOOTH) insert 0 1"
    bind $P4Widgets(HISTOGRAM_XSTEP)  <Double-Button-2> "$P4Widgets(HISTOGRAM_XSTEP) delete 0 end"
    bind $P4Widgets(HISTOGRAM_YSTEP)  <Double-Button-2> "$P4Widgets(HISTOGRAM_YSTEP) delete 0 end"
    bind $P4Widgets(HISTOGRAM_BINS)  <Double-Button-2> "$P4Widgets(HISTOGRAM_BINS) delete 0 end"
    bind $P4Widgets(HIST_SMOOTH)  <Double-Button-2> "$P4Widgets(HIST_SMOOTH) delete 0 end"
    bind $P4Widgets(HISTOGRAM_XSTEP) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4HistBox1.html"
    bind $P4Widgets(HISTOGRAM_YSTEP) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4HistBox1.html"
    bind $P4Widgets(HISTOGRAM_BINS) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4HistBox1.html"
    bind $P4Widgets(HIST_SMOOTH) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4HistBox1.html"
    bind $xlabel <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4HistBox1.html"
    bind $ylabel <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4HistBox1.html"
    bind $bitlab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4HistBox1.html"
    bind $smlab <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4HistBox1.html"

# Contour attributes
    set contourFlipOut [frame $flipFrame2.cflip]
    set lab1 [label $contourFlipOut.l1 -text "Method" -width 8]
    set lin  [radiobutton $contourFlipOut.lin -text "Lin" -variable P4Widgets(CONTOUR_TYPE) -value LIN -width 4]
    set sig  [radiobutton $contourFlipOut.sig -text "Sig" -variable P4Widgets(CONTOUR_TYPE) -value SIG -width 4]
    set magi [radiobutton $contourFlipOut.magi -text "MagI" -variable P4Widgets(CONTOUR_TYPE) -value MAGI -width 4]
    set magd [radiobutton $contourFlipOut.magd -text "MagD" -variable P4Widgets(CONTOUR_TYPE) -value MAGD -width 4]
    set lni  [radiobutton $contourFlipOut.lni -text "LnI" -variable P4Widgets(CONTOUR_TYPE) -value LNI -width 4]
    set lnd  [radiobutton $contourFlipOut.lnd -text "LnD" -variable P4Widgets(CONTOUR_TYPE) -value LND -width 4]
    set logi [radiobutton $contourFlipOut.logi -text "LogI" -variable P4Widgets(CONTOUR_TYPE) -value LOGI -width 4]
    set logd [radiobutton $contourFlipOut.logd -text "LogD" -variable P4Widgets(CONTOUR_TYPE) -value LOGD -width 4]
    set lab2 [label $contourFlipOut.lab2 -text "Nlevels"]
    set P4Widgets(CONTOUR_LEVELS) [entry $contourFlipOut.n -width 5]
    pack $lab2 $P4Widgets(CONTOUR_LEVELS) $lab1 $lin $sig $magi $magd $lni $lnd $logi $logd -side left -fill x -expand yes
    bind $lab1 <Button-2> "p4Update buildP4Widgets1 CONTOUR"
    bind $lab2 <Button-2> "p4Update buildP4Widgets1 CONTOUR"
    bind $lab1 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $lab2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $P4Widgets(CONTOUR_LEVELS) <Button-2> "$P4Widgets(CONTOUR_LEVELS) delete 0 end; $P4Widgets(CONTOUR_LEVELS) insert 0 10"
    bind $P4Widgets(CONTOUR_LEVELS) <Double-Button-2> "$P4Widgets(CONTOUR_LEVELS) delete 0 end"
    bind $P4Widgets(CONTOUR_LEVELS) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $lin <Button-2> "set P4Widgets(CONTOUR_TYPE) LIN"
    bind $sig <Button-2> "set P4Widgets(CONTOUR_TYPE) LIN"
    bind $magi <Button-2> "set P4Widgets(CONTOUR_TYPE) LIN"
    bind $magd <Button-2> "set P4Widgets(CONTOUR_TYPE) LIN"
    bind $logi <Button-2> "set P4Widgets(CONTOUR_TYPE) LIN"
    bind $logd <Button-2> "set P4Widgets(CONTOUR_TYPE) LIN"
    bind $lni <Button-2> "set P4Widgets(CONTOUR_TYPE) LIN"
    bind $lnd <Button-2> "set P4Widgets(CONTOUR_TYPE) LIN"
    bind $lin <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $sig <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $magi <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $magd <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $logi <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $logd <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $lni <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $lnd <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"

# Graph/Overgraph attributes
    set graphFlipOut [frame $flipFrame2.gflip]
    set ploterr [checkbutton $graphFlipOut.pe -text "Errors" -variable P4Widgets(PLOT_ERRORS)]
    set cutx [radiobutton $graphFlipOut.cutx -text X-cut -variable P4Widgets(CUT_DIRECTION) -value X -width 6]
    set cuty [radiobutton $graphFlipOut.cuty -text Y-cut -variable P4Widgets(CUT_DIRECTION) -value Y -width 6]
    set lab2 [label $graphFlipOut.l2 -text "Start"]
    set P4Widgets(SLICE_START) [entry $graphFlipOut.ss -width 5]
    set lab3 [label $graphFlipOut.l3 -text "End"]
    set P4Widgets(SLICE_END) [entry $graphFlipOut.se -width 5]
    set bl [radiobutton $graphFlipOut.bl -bitmap @$cgs4drBitmaps/patch.xbm \
	-variable P4Widgets(OVERCOLOUR) -foreground black -activeforeground black -value BLACK]
    set wh [radiobutton $graphFlipOut.wh -bitmap @$cgs4drBitmaps/patch.xbm \
	-variable P4Widgets(OVERCOLOUR) -foreground white  -activeforeground white -value WHITE]
    set r [radiobutton $graphFlipOut.r -bitmap @$cgs4drBitmaps/patch.xbm \
	-variable P4Widgets(OVERCOLOUR) -foreground red  -activeforeground red -value RED]
    set o [radiobutton $graphFlipOut.o -bitmap @$cgs4drBitmaps/patch.xbm \
	-variable P4Widgets(OVERCOLOUR) -foreground orange  -activeforeground orange -value ORANGE]
    set y [radiobutton $graphFlipOut.y -bitmap @$cgs4drBitmaps/patch.xbm \
	-variable P4Widgets(OVERCOLOUR) -foreground yellow  -activeforeground yellow -value YELLOW]
    set g [radiobutton $graphFlipOut.g -bitmap @$cgs4drBitmaps/patch.xbm \
	-variable P4Widgets(OVERCOLOUR) -foreground green  -activeforeground green -value GREEN]
    set b [radiobutton $graphFlipOut.b -bitmap @$cgs4drBitmaps/patch.xbm \
	-variable P4Widgets(OVERCOLOUR) -foreground blue -activeforeground blue -value BLUE]
    pack $ploterr $cutx $cuty $lab2 $P4Widgets(SLICE_START) $lab3 $P4Widgets(SLICE_END) \
         $bl $wh $r $o $y $g $b -side left -fill x -expand yes
    bind $lab2 <Button-2> "p4Update buildP4Widgets1 GRAPH"
    bind $lab3 <Button-2> "p4Update buildP4Widgets1 GRAPH"
    bind $lab2 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $lab3 <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $P4Widgets(SLICE_START) <Button-2> "$P4Widgets(SLICE_START) delete 0 end; $P4Widgets(SLICE_START) insert 0 29.0"
    bind $P4Widgets(SLICE_START) <Double-Button-2> "$P4Widgets(SLICE_START) delete 0 end"
    bind $P4Widgets(SLICE_START) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $P4Widgets(SLICE_END) <Button-2> "$P4Widgets(SLICE_END) delete 0 end; $P4Widgets(SLICE_END) insert 0 29.0"
    bind $P4Widgets(SLICE_END) <Double-Button-2> "$P4Widgets(SLICE_END) delete 0 end"
    bind $P4Widgets(SLICE_END) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $ploterr <Button-2> "set P4Widgets(PLOT_ERRORS) 1"
    bind $ploterr <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $cutx <Button-2> "set P4Widgets(CUT_DIRECTION) X"
    bind $cutx <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $cuty <Button-2> "set P4Widgets(CUT_DIRECTION) X"
    bind $cuty <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $bl <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $bl <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $wh <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $wh <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $r <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $r <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $o <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $o <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $y <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $y <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $g <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $g <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"
    bind $b <Button-2> "set P4Widgets(OVERCOLOUR) RED"
    bind $b <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ContourBox1.html"

# Set a trace on the display type that flips the appropriate attribute panel out
    global typelist
    set typelist [list IMAGE GRAPH HISTOGRAM OVERGRAPH SURFACE CONTOUR]
    global widgetlist
    set widgetlist [list {} $graphFlipOut $histogramFlipOut $graphFlipOut {} $contourFlipOut]
    trace variable P4Widgets(DISPLAY_TYPE) w "NewDisplayType $flipFrame2"
    return $mainFrame
}

proc NewDisplayType {flipFrame args} {
  global P4Widgets
  global typelist
  global widgetlist
  set i [lsearch -exact $typelist $P4Widgets(DISPLAY_TYPE)]
  NewFlipOut $flipFrame [lindex $widgetlist $i]
}

proc NewFlipOut {master slave} {
#+
#  Displays a flip out panel after unmapping any existing one.
#-
    set currentSlave [pack slaves $master]
    if {$currentSlave != ""} {pack forget $currentSlave}
    pack forget $master
    if {$slave != ""} {
      pack $master -fill x -expand yes -after [winfo parent ${master}].st
      pack $slave -side left
    }
}

proc SetFlipOut {master name el op} {
  global ${name}
  set currentSlaves [pack slaves $master]
  foreach field $currentSlaves {pack forget $field}

  set plot_whole [set ${name}(PLOT_WHOLE)]
  set autoscale [set ${name}(AUTOSCALE)]

  pack forget $master
  if {$plot_whole==0 && $autoscale==0} {
    pack $master -fill x -expand yes -after [winfo parent ${master}].plf
    pack ${master}.wh -side right -fill x
    pack ${master}.as -side left -fill x
  } elseif {$plot_whole==1 && $autoscale==0} {
    pack $master -fill x -expand yes -after [winfo parent ${master}].plf
    pack ${master}.as -side left -fill x
  } elseif {$plot_whole==0 && $autoscale==1} {
    pack $master -fill x -expand yes -after [winfo parent ${master}].plf
    pack ${master}.wh -side right -fill x
  }
}

#proc SetFlipOut {w c name el op} {
#  global ${name}
#  puts "Window=$w Slave=$c Name=$name Element=$el Operation=$op"
#  set slaves [pack slaves [winfo parent $c]]
#  puts "Slaves=$slaves"
#  set val [set ${name}($el)]
#  puts "Value=$val"
#  if {$val == 0} {
#    if {$slaves == ""} {
#      pack $c -fill x
#    } else {
#      if {$el == "PLOT_WHOLE"} {
#        pack $c -side right -fill x
#        pack $slaves -side left -fill x
#      } elseif {$el == "AUTOSCALE"} {
#        pack $c -side left -fill x
#        pack $slaves -side right -fill x
#      }
#    }
#  } else {
#    pack forget $c
#    pack forget $w
#  }
#}

