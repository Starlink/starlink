proc red4EditMask {taskname qval} {
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
    global mask_x
    global mask_y

# Check to see if NBS exists
    set status [cgs4drCheckNbs p4]
    if {$status!=0} {return}

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 Edit Bad Pixel Mask" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set top [frame $frame.top]
    set bot [frame $frame.bot]
    pack $top $bot -in $frame -side top -expand yes -fill both

    set Red4Widgets(EM_LAB01) [label $top.l1 -text "Mask"]
    set Red4Widgets(EM_ENT01) [entry $top.e1 -width 60]
    pack $Red4Widgets(EM_LAB01) $Red4Widgets(EM_ENT01) -in $top -side left
    $Red4Widgets(EM_ENT01) insert end $Red4Widgets(MASK)

    set Red4Widgets(EM_LAB02) [label $bot.l3 -text "Port"]
    set p0 [radiobutton $bot.p0 -bitmap @$cgs4drBitmaps/port0.xbm -variable Red4Widgets(EM_RDISP) -value 0]
    set p1 [radiobutton $bot.p1 -bitmap @$cgs4drBitmaps/port1.xbm -variable Red4Widgets(EM_RDISP) -value 1]
    set p2 [radiobutton $bot.p2 -bitmap @$cgs4drBitmaps/port2.xbm -variable Red4Widgets(EM_RDISP) -value 2]
    set p3 [radiobutton $bot.p3 -bitmap @$cgs4drBitmaps/port3.xbm -variable Red4Widgets(EM_RDISP) -value 3]
    set p4 [radiobutton $bot.p4 -bitmap @$cgs4drBitmaps/port4.xbm -variable Red4Widgets(EM_RDISP) -value 2]
    set p5 [radiobutton $bot.p5 -bitmap @$cgs4drBitmaps/port5.xbm -variable Red4Widgets(EM_RDISP) -value 5]
    set p6 [radiobutton $bot.p6 -bitmap @$cgs4drBitmaps/port6.xbm -variable Red4Widgets(EM_RDISP) -value 6]
    set p7 [radiobutton $bot.p7 -bitmap @$cgs4drBitmaps/port7.xbm -variable Red4Widgets(EM_RDISP) -value 7]
    set p8 [radiobutton $bot.p8 -bitmap @$cgs4drBitmaps/port8.xbm -variable Red4Widgets(EM_RDISP) -value 8]
    pack $Red4Widgets(EM_LAB02) $p0 $p1 $p2 $p3 $p4 $p5 $p6 $p7 $p8 -in $bot -side left -expand yes -fill x
    set Red4Widgets(EM_RDISP) 0

# Bind the defaults to Button-2
    bind $Red4Widgets(EM_LAB01) <Button-2> "red4Update red4EditMask ALL"
    bind $Red4Widgets(EM_LAB02) <Button-2> "red4Update red4EditMask ALL"
    bind $Red4Widgets(EM_ENT01) <Button-2> "red4Update red4EditMask EM_ENT01"
    bind $Red4Widgets(EM_ENT01) <Double-Button-2> "$Red4Widgets(EM_ENT01) delete 0 end"
    bind $p0 <Button-2> "red4Update red4EditMask EM_RDISP"
    bind $p1 <Button-2> "red4Update red4EditMask EM_RDISP"
    bind $p2 <Button-2> "red4Update red4EditMask EM_RDISP"
    bind $p3 <Button-2> "red4Update red4EditMask EM_RDISP"
    bind $p4 <Button-2> "red4Update red4EditMask EM_RDISP"
    bind $p5 <Button-2> "red4Update red4EditMask EM_RDISP"
    bind $p6 <Button-2> "red4Update red4EditMask EM_RDISP"
    bind $p7 <Button-2> "red4Update red4EditMask EM_RDISP"
    bind $p8 <Button-2> "red4Update red4EditMask EM_RDISP"

# Show the dialog box and get the dataset and port
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set Red4Widgets(MASK) [string trim [$Red4Widgets(EM_ENT01) get]]
      set port $Red4Widgets(EM_RDISP)
      destroy .red4Dialogue

#   Return if no data specified
      if {$Red4Widgets(MASK)=="" || $Red4Widgets(MASK)=="#"} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4EditMask error : A dataset has not been specified properly!"
        cgs4drCursor arrow green black
        return
      }

#   Display the dataset
      if {[file exists $Red4Widgets(MASK)*]==0} {set Red4Widgets(MASK) \$CGS4_MASKS/$Red4Widgets(MASK)}
      cgs4drInform $taskname "Plotting $Red4Widgets(MASK) in port $port as an image"
      nbs put ${P4NoticeBoard}.port_${port}.display_type IMAGE
      set status [catch {$P4Task obey display "data=$Red4Widgets(MASK) port=$port" -inform "cgs4drInform $taskname %V"}]
      if {$status!=0} {
        cgs4drCursor arrow green black
        return
      }

#   Loop to do edits
      set loop_status 0
      while {$loop_status==0} {

#     Issue a message
        cgs4drInform $taskname "Click on data-point with MB1; Use MB3 to abort"

#     Do the cursor
        set done_cval 0
        $P4Task obey cursorval "port=$port" -inform "cgs4drInform $taskname %V" -endmsg {set done_cval 1}
        tkwait variable done_cval

#     Get the returns (this will hang if task dies for any reason!)
        set cursor_status -1
        $P4Task get cursor_status -getresponse "red4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
        tkwait variable cursor_status
        set mask_x -1
        $P4Task get mask_x -getresponse "red4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
        tkwait variable mask_x
        set mask_y -1
        $P4Task get mask_y -getresponse "red4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
        tkwait variable mask_y

#     Edit the mask
        if {$cursor_status==0} {
          $taskname obey edit_mask "loop=FALSE mask=$Red4Widgets(MASK) ipos=$mask_x jpos=$mask_y qval=$qval" -inform "cgs4drInform $taskname %V"
        } else {
          cgs4drInform $taskname "Application terminated by non-zero cursor status"
          set loop_status 1
        }
      }

# Any other button response kills the dialogue box
    } else {
      destroy .red4Dialogue
    }

# Change the cursor and exit
    cgs4drCursor arrow green black
}
