proc red4CreThreshMask {taskname array} {
#+
# Creates a dialog box for red4 action
#-
    global env
    global cgs4drBitmaps
    global Red4Widgets
    global P4NoticeBoard
    global P4Task
    global done_cval
    global cursor_status
    global x
    global tlow
    global thigh

# Check to see if NBS exists
    set status [cgs4drCheckNbs p4]
    if {$status!=0} {return}

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 Create Threshold Bad Pixel Mask ($array array)" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set top [frame $frame.top]
    set mid [frame $frame.mid]
    set bot [frame $frame.bot]
    pack $top $mid $bot -in $frame -side top -expand yes -fill both

    set Red4Widgets(TM_LAB01) [label $top.l1 -text "Filename"]
    set Red4Widgets(TM_ENT01) [entry $top.e1 -width 60]
    pack $Red4Widgets(TM_LAB01) $Red4Widgets(TM_ENT01) -in $top -side left
    $Red4Widgets(TM_ENT01) insert end [string trim $Red4Widgets(RO)]

    set Red4Widgets(TM_LAB02) [label $mid.l2 -text "Maskname"]
    set Red4Widgets(TM_ENT02) [entry $mid.e2 -width 60]
    pack $Red4Widgets(TM_LAB02) $Red4Widgets(TM_ENT02) -in $mid -side left
    $Red4Widgets(TM_ENT02) insert end $Red4Widgets(MBPM)

    set Red4Widgets(TM_LAB03) [label $bot.l3 -text "Port"]
    set p0 [radiobutton $bot.p0 -bitmap @$cgs4drBitmaps/port0.xbm -variable Red4Widgets(TM_RDISP) -value 0]
    set p1 [radiobutton $bot.p1 -bitmap @$cgs4drBitmaps/port1.xbm -variable Red4Widgets(TM_RDISP) -value 1]
    set p2 [radiobutton $bot.p2 -bitmap @$cgs4drBitmaps/port2.xbm -variable Red4Widgets(TM_RDISP) -value 2]
    set p3 [radiobutton $bot.p3 -bitmap @$cgs4drBitmaps/port3.xbm -variable Red4Widgets(TM_RDISP) -value 3]
    set p4 [radiobutton $bot.p4 -bitmap @$cgs4drBitmaps/port4.xbm -variable Red4Widgets(TM_RDISP) -value 2]
    set p5 [radiobutton $bot.p5 -bitmap @$cgs4drBitmaps/port5.xbm -variable Red4Widgets(TM_RDISP) -value 5]
    set p6 [radiobutton $bot.p6 -bitmap @$cgs4drBitmaps/port6.xbm -variable Red4Widgets(TM_RDISP) -value 6]
    set p7 [radiobutton $bot.p7 -bitmap @$cgs4drBitmaps/port7.xbm -variable Red4Widgets(TM_RDISP) -value 7]
    set p8 [radiobutton $bot.p8 -bitmap @$cgs4drBitmaps/port8.xbm -variable Red4Widgets(TM_RDISP) -value 8]
    pack $Red4Widgets(TM_LAB03) $p0 $p1 $p2 $p3 $p4 $p5 $p6 $p7 $p8 -in $bot -side left -expand yes -fill x
    set Red4Widgets(TM_RDISP) 0

# Bind defaults to button
    bind $Red4Widgets(TM_LAB01) <Button-2> "red4Update red4CreThreshMask ALL"
    bind $Red4Widgets(TM_LAB02) <Button-2> "red4Update red4CreThreshMask ALL"
    bind $Red4Widgets(TM_LAB03) <Button-2> "red4Update red4CreThreshMask ALL"
    bind $Red4Widgets(TM_ENT01) <Button-2> "red4Update red4CreThreshMask TM_ENT01"
    bind $Red4Widgets(TM_ENT02) <Button-2> "red4Update red4CreThreshMask TM_ENT02"
    bind $Red4Widgets(TM_ENT01) <Double-Button-2> "$Red4Widgets(TM_ENT01) delete 0 end"
    bind $Red4Widgets(TM_ENT02) <Double-Button-2> "$Red4Widgets(TM_ENT02) delete 0 end"
    bind $p0 <Button-2> "red4Update red4CreThreshMask TM_RDISP"
    bind $p1 <Button-2> "red4Update red4CreThreshMask TM_RDISP"
    bind $p2 <Button-2> "red4Update red4CreThreshMask TM_RDISP"
    bind $p3 <Button-2> "red4Update red4CreThreshMask TM_RDISP"
    bind $p4 <Button-2> "red4Update red4CreThreshMask TM_RDISP"
    bind $p5 <Button-2> "red4Update red4CreThreshMask TM_RDISP"
    bind $p6 <Button-2> "red4Update red4CreThreshMask TM_RDISP"
    bind $p7 <Button-2> "red4Update red4CreThreshMask TM_RDISP"
    bind $p8 <Button-2> "red4Update red4CreThreshMask TM_RDISP"

# Show the dialog box and get the dataset and port
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set Red4Widgets(RO) [string trim [$Red4Widgets(TM_ENT01) get]]
      set Red4Widgets(MBPM) [string trim [$Red4Widgets(TM_ENT02) get]]
      set port $Red4Widgets(TM_RDISP)
      destroy .red4Dialogue

#   Return if no data specified
      if {$Red4Widgets(RO)=="" || $Red4Widgets(RO)==$Red4Widgets(DRO) || $Red4Widgets(MBPM)=="" || $Red4Widgets(MBPM)=="#"} {
        set Red4Widgets(RO) $Red4Widgets(DRO)
        cgs4drClear $taskname
        cgs4drInform $taskname "red4CreThreshMask error : A dataset has not been specified properly!"
        cgs4drCursor arrow green black
        return
      }

#   Display the dataset
      set message "Plotting $Red4Widgets(RO) ($array array) in port $port as a HISTOGRAM"
      cgs4drInform $taskname $message
      nbs put ${P4NoticeBoard}.port_${port}.display_type HISTOGRAM
      nbs put ${P4NoticeBoard}.port_${port}.display_plane [string trim [string toupper $array]]
      set status [catch {$P4Task obey display "data=$Red4Widgets(RO) port=$port" -inform "cgs4drInform $taskname %V"}]
      if {$status!=0} {
        cgs4drCursor arrow green black
        return
      }

#   Do the cursoring and get the result (will hang if task dies!)
      set message "Click on LOW data-point with MB1; Use MB3 to abort"
      cgs4drInform $taskname $message
      set done_cval 0
      $P4Task obey cursor "port=$port" -inform "cgs4drInform $taskname %V" -endmsg {set done_cval 1}
      tkwait variable done_cval
      set cursor_status -1
      $P4Task get cursor_status -getresponse "red4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
      tkwait variable cursor_status
      set x -1
      $P4Task get x -getresponse "red4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
      tkwait variable x
      set tlow $x

#   Do it again
      if {$cursor_status==0} {
        set message "Click on HIGH data-point with MB1; Use MB3 to abort"
        cgs4drInform $taskname $message
        set done_cval 0
        $P4Task obey cursor "port=$port" -inform "cgs4drInform $taskname %V" -endmsg {set done_cval 1}
        tkwait variable done_cval
        set cursor_status -1
        $P4Task get cursor_status -getresponse "red4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
        tkwait variable cursor_status
        set x -1
        $P4Task get x -getresponse "red4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
        tkwait variable x
        set thigh $x
        if {$cursor_status==0} {
          $taskname obey cre_thresh_mask "input=$Red4Widgets(RO) mask=$Red4Widgets(MBPM) tlow=$tlow thigh=$thigh" -inform "cgs4drInform $taskname %V"
        } else {
          set message "Application terminated by non-zero cursor status"
          cgs4drInform $taskname $message
        }
      } else {
        set message "Application terminated by non-zero cursor status"
        cgs4drInform $taskname $message
      }

# Any other button response kills the dialogue box
    } else {
      destroy .red4Dialogue
    }

# Change the cursor and exit
    cgs4drCursor arrow green black
}
