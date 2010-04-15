proc red4TwoDLineFit {taskname} {
#+
# Creates a dialog box for red4 action
#-
    global env
    global Red4Widgets

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Check to see figaro1 tasks is loaded
   if {[info commands figaro1*] == ""} {
     set message "Loading figaro1"
     cgs4drInform $taskname $message
     adamtask figaro1 /star/bin/figaro/figaro1
   }

# Check to see figaro2 tasks is loaded
   if {[info commands figaro2*] == ""} {
     set message "Loading figaro2"
     cgs4drInform $taskname $message
     adamtask figaro2 /star/bin/figaro/figaro2
   }

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 Two-D Line Fit" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set toptop [frame $frame.tt]
    set midtop [frame $frame.mt]
    set bottop [frame $frame.bt]
    pack $toptop $midtop $bottop -in $frame -side top

# Create, pack and bind filename widget
    set Red4Widgets(TWD_LAB01) [label $toptop.lb -text "Filename"]
    set Red4Widgets(TWD_ENT01) [entry $toptop.e0 -width 40]
    pack $Red4Widgets(TWD_LAB01) $Red4Widgets(TWD_ENT01) -in $toptop -side left
    $Red4Widgets(TWD_ENT01) insert end $Red4Widgets(RO)
    bind $Red4Widgets(TWD_LAB01) <Button-2> "red4Update red4TwoDLineFit ALL"
    bind $Red4Widgets(TWD_ENT01) <Button-2> "red4Update red4TwoDLineFit TWD_ENT01"
    bind $Red4Widgets(TWD_ENT01) <Double-Button-2> "$Red4Widgets(TWD_ENT01) delete 0 end"

# Create, pack and bind row/col widget
    set Red4Widgets(TWD_LAB02) [label $midtop.lb2 -text "X Start"]
    set Red4Widgets(TWD_ENT02) [entry $midtop.e2 -width 10]
    set Red4Widgets(TWD_LAB03) [label $midtop.lb3 -text "X End"]
    set Red4Widgets(TWD_ENT03) [entry $midtop.e3 -width 10]
    $Red4Widgets(TWD_ENT02) insert end Red4Widgets(TWD_DXST)
    $Red4Widgets(TWD_ENT03) insert end Red4Widgets(TWD_DXEN)
    pack $Red4Widgets(TWD_LAB02) $Red4Widgets(TWD_ENT02) -in $midtop -side left -pady 2m
    pack $Red4Widgets(TWD_ENT03) $Red4Widgets(TWD_LAB03) -in $midtop -side right -pady 2m
    bind $Red4Widgets(TWD_LAB02) <Button-2> "red4Update red4TwoDLineFit ALL"
    bind $Red4Widgets(TWD_ENT02) <Button-2> "red4Update red4TwoDLineFit TWD_ENT02"
    bind $Red4Widgets(TWD_ENT02) <Double-Button-2> "$Red4Widgets(TWD_ENT02) delete 0 end"
    bind $Red4Widgets(TWD_LAB03) <Button-2> "red4Update red4TwoDLineFit ALL"
    bind $Red4Widgets(TWD_ENT03) <Button-2> "red4Update red4TwoDLineFit TWD_ENT03"
    bind $Red4Widgets(TWD_ENT03) <Double-Button-2> "$Red4Widgets(TWD_ENT03) delete 0 end"

# Create, pack and bind row/col widget
    set Red4Widgets(TWD_LAB04) [label $bottop.lb2 -text "Y Start"]
    set Red4Widgets(TWD_ENT04) [entry $bottop.e2 -width 10]
    set Red4Widgets(TWD_LAB05) [label $bottop.lb3 -text "Y End"]
    set Red4Widgets(TWD_ENT05) [entry $bottop.e3 -width 10]
    $Red4Widgets(TWD_ENT04) insert end Red4Widgets(TWD_DYST)
    $Red4Widgets(TWD_ENT05) insert end Red4Widgets(TWD_DYEN)
    pack $Red4Widgets(TWD_LAB04) $Red4Widgets(TWD_ENT04) -in $bottop -side left -pady 2m
    pack $Red4Widgets(TWD_ENT05) $Red4Widgets(TWD_LAB05) -in $bottop -side right -pady 2m
    bind $Red4Widgets(TWD_LAB04) <Button-2> "red4Update red4TwoDLineFit ALL"
    bind $Red4Widgets(TWD_ENT04) <Button-2> "red4Update red4TwoDLineFit TWD_ENT04"
    bind $Red4Widgets(TWD_ENT04) <Double-Button-2> "$Red4Widgets(TWD_ENT04) delete 0 end"
    bind $Red4Widgets(TWD_LAB05) <Button-2> "red4Update red4TwoDLineFit ALL"
    bind $Red4Widgets(TWD_ENT05) <Button-2> "red4Update red4TwoDLineFit TWD_ENT05"
    bind $Red4Widgets(TWD_ENT05) <Double-Button-2> "$Red4Widgets(TWD_ENT05) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]

# If CANCEL blow the box away and return
    if {$bv != 0} {
      cgs4drCursor arrow green black
      destroy .red4Dialogue
      return
    }

# Get the values from the dialogue box
    set Red4Widgets(RO)       [string trim [$Red4Widgets(TWD_ENT01) get]]
    set Red4Widgets(TWD_DXST) [string trim [$Red4Widgets(TWD_ENT02) get]]
    set Red4Widgets(TWD_DXEN) [string trim [$Red4Widgets(TWD_ENT03) get]]
    set Red4Widgets(TWD_DYST) [string trim [$Red4Widgets(TWD_ENT04) get]]
    set Red4Widgets(TWD_DYEN) [string trim [$Red4Widgets(TWD_ENT05) get]]

# Abort if dataset is garbage
   set uspos [string first "_" $Red4Widgets(RO)]
   if {$uspos>0} {
     set number [string range $Red4Widgets(RO) [expr $uspos + 1] end]
   } else {
     set number -1
   }
   set status [catch {incr number 0}]
   if {$Red4Widgets(RO)=="" || $Red4Widgets(RO)==$Red4Widgets(DRO) || $status!=0 || $number<=0} {
     cgs4drClear $taskname
     cgs4drInform $taskname "red4TwoDLineFit error : A dataset has not been specified properly!"
     destroy .red4Dialogue
     cgs4drCursor arrow green black
     return
   } else {

# Remove the dialog box otherwise Figaro SPLOT can't grab input focus!
     destroy .red4Dialogue
   }

# Open the engineering log file
   set message "Opening engineering log file"
   cgs4drInform $taskname $message
   $taskname obey open_log "" -inform "cgs4drInform $taskname %V" -endmsg {set open_log_done 1}
   tkwait variable open_log_done

# Extract the first spectrum
   set message "Extracting spectrum from rows $Red4Widgets(TWD_DYST) to $Red4Widgets(TWD_DYEN)"
   set message "$message in $Red4Widgets(RO) to $Red4Widgets(RO)_spc"
   cgs4drInform $taskname $message
   set param "image=$Red4Widgets(RO) ystart=$Red4Widgets(TWD_DYST) yend=$Red4Widgets(TWD_DYEN) spectrum=$Red4Widgets(RO)_spc"
   $taskname obey extract4 "$param" -inform "cgs4drInform $taskname %V" -endmsg {set ext1_done 1}
   tkwait variable ext1_done

# Do EMLT etc on first spectrum
   if {[file exists emlt.lis] == 1} {exec /usr/bin/rm -rf emlt.lis}
   set message "Running EMLT on $Red4Widgets(RO)_spc"
   cgs4drInform $taskname $message
   set param "spectrum=$Red4Widgets(RO)_spc xstart=$Red4Widgets(TWD_DXST) xend=$Red4Widgets(TWD_DXEN) lines=0 moments=T"
   figaro2 obey emlt "$param" -inform "cgs4drInform $taskname %V" -endmsg {set emlt1_done 1}
   tkwait variable emlt1_done

# Log some comments to file
   set message "Logging some comments in the engineering log file"
   cgs4drInform $taskname $message
   $taskname obey log_comment "comment=' '" -inform "cgs4drInform $taskname %V" -endmsg {set log_comment1_done 1}
   tkwait variable log_comment1_done
   set comment "Searching for lines in $Red4Widgets(RO)"
   $taskname obey log_comment "comment='$comment'" -inform "cgs4drInform $taskname %V" -endmsg {set log_comment2_done 1}
   tkwait variable log_comment2_done

   if {[file exists emlt.lis] == 1} {
     catch {exec /usr/bin/cp emlt.lis $env(CGS4_ENG)/emlt.lis}
     $taskname obey copy_to_log "source_file='emlt.lis'" -inform "cgs4drInform $taskname %V" -endmsg {set copy1_done 1}
     tkwait variable copy1_done
     set param "xmin=$Red4Widgets(TWD_DXST) xmax=$Red4Widgets(TWD_DXEN)"
     $taskname obey read_emlt "$param" -inform "cgs4drInform $taskname %V" -endmsg {set remlt1_done 1}
     tkwait variable remlt1_done
     set ycen [expr [expr $Red4Widgets(TWD_DYST) + $Red4Widgets(TWD_DYEN)] / 2.0]
     $taskname obey log_emlt "ycen=$ycen" -inform "cgs4drInform $taskname %V" -endmsg {set lemlt1_done 1}
     tkwait variable lemlt1_done
     $taskname obey report_emlt "" -inform "cgs4drInform $taskname %V" -endmsg {set remlt2_done 1}
     tkwait variable remlt2_done
   } else {
     set message "red4TwoDLineFit error : Unable to locate emlt.lis file!"
     cgs4drInform $taskname $message
   }

# Issue message
    set message "Two line fitting function complete"
    cgs4drInform $taskname $message
    cgs4drCursor arrow green black
}
