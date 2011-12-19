proc red4TwoLineFit {taskname type} {
#+
# Creates a dialog box for red4 action
#-
    global env
    global Red4Widgets
    set type [string trim [string tolower $type]]

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
    set frame [dialogStart .red4Dialogue "Red4 Two Line Fit ($type)" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set toptop [frame $frame.tt]
    set midtop [frame $frame.mt]
    set bottop [frame $frame.bt]
    set basetop [frame $frame.at]
    pack $toptop $midtop $bottop $basetop -in $frame -side top

# Create, pack and bind filename widget
    set Red4Widgets(TLF_LAB01) [label $toptop.lb -text "Filename"]
    set Red4Widgets(TLF_ENT01) [entry $toptop.e0 -width 40]
    pack $Red4Widgets(TLF_LAB01) $Red4Widgets(TLF_ENT01) -in $toptop -side left
    $Red4Widgets(TLF_ENT01) insert end $Red4Widgets(RO)
    bind $Red4Widgets(TLF_LAB01) <Button-2> "red4Update red4TwoLineFit ALL"
    bind $Red4Widgets(TLF_ENT01) <Button-2> "red4Update red4TwoLineFit TLF_ENT01"
    bind $Red4Widgets(TLF_ENT01) <Double-Button-2> "$Red4Widgets(TLF_ENT01) delete 0 end"

# Create, pack and bind row/col widget
    if {$type == "row"} {
      set Red4Widgets(TLF_LAB02) [label $midtop.lb2 -text "First Row"]
      set Red4Widgets(TLF_ENT02) [entry $midtop.e2 -width 10]
      set Red4Widgets(TLF_LAB03) [label $midtop.lb3 -text "Second Row"]
      set Red4Widgets(TLF_ENT03) [entry $midtop.e3 -width 10]
    } else {
      set Red4Widgets(TLF_LAB02) [label $midtop.lb2 -text "First Column"]
      set Red4Widgets(TLF_ENT02) [entry $midtop.e2 -width 10]
      set Red4Widgets(TLF_LAB03) [label $midtop.lb3 -text "Second Column"]
      set Red4Widgets(TLF_ENT03) [entry $midtop.e3 -width 10]
    }
    $Red4Widgets(TLF_ENT02) insert end $Red4Widgets(TLF_DRS)
    $Red4Widgets(TLF_ENT03) insert end $Red4Widgets(TLF_DRE)
    pack $Red4Widgets(TLF_LAB02) $Red4Widgets(TLF_ENT02) -in $midtop -side left -pady 2m
    pack $Red4Widgets(TLF_ENT03) $Red4Widgets(TLF_LAB03) -in $midtop -side right -pady 2m
    bind $Red4Widgets(TLF_LAB02) <Button-2> "red4Update red4TwoLineFit ALL"
    bind $Red4Widgets(TLF_ENT02) <Button-2> "red4Update red4TwoLineFit TLF_ENT02"
    bind $Red4Widgets(TLF_LAB03) <Button-2> "red4Update red4TwoLineFit ALL"
    bind $Red4Widgets(TLF_ENT03) <Button-2> "red4Update red4TwoLineFit TLF_ENT03"
    bind $Red4Widgets(TLF_ENT02) <Double-Button-2> "$Red4Widgets(TLF_ENT02) delete 0 end"
    bind $Red4Widgets(TLF_ENT03) <Double-Button-2> "$Red4Widgets(TLF_ENT03) delete 0 end"

# Create radiobutton for number of rows/cols to fit
    if {$type == "row"} {
      set Red4Widgets(TLF_LAB04) [label $bottop.lb2 -text "Rows to Average"]
    } else {
      set Red4Widgets(TLF_LAB04) [label $bottop.lb2 -text "Columns to Average"]
    }
    set rb1 [radiobutton $bottop.rb1 -text 1 -width 5 -variable Red4Widgets(TLF_RAD01) -value 1]
    set rb2 [radiobutton $bottop.rb2 -text 3 -width 5 -variable Red4Widgets(TLF_RAD01) -value 3]
    set rb3 [radiobutton $bottop.rb3 -text 5 -width 5 -variable Red4Widgets(TLF_RAD01) -value 5]
    set rb4 [radiobutton $bottop.rb4 -text 7 -width 5 -variable Red4Widgets(TLF_RAD01) -value 7]
    set rb5 [radiobutton $bottop.rb5 -text 9 -width 5 -variable Red4Widgets(TLF_RAD01) -value 9]
    set rb6 [radiobutton $bottop.rb6 -text 11 -width 5 -variable Red4Widgets(TLF_RAD01) -value 11]
    set rb7 [radiobutton $bottop.rb7 -text 13 -width 5 -variable Red4Widgets(TLF_RAD01) -value 13]
    set rb8 [radiobutton $bottop.rb8 -text 15 -width 5 -variable Red4Widgets(TLF_RAD01) -value 15]
    pack $Red4Widgets(TLF_LAB04) $rb1 $rb2 $rb3 $rb4 $rb5 $rb6 $rb7 $rb8 -in $bottop -side left
    bind $Red4Widgets(TLF_LAB04) <Button-2> "red4Update red4TwoLineFit ALL"
    bind $rb1 <Button-2> "red4Update red4TwoLineFit TLF_RAD01"
    bind $rb2 <Button-2> "red4Update red4TwoLineFit TLF_RAD01"
    bind $rb3 <Button-2> "red4Update red4TwoLineFit TLF_RAD01"
    bind $rb4 <Button-2> "red4Update red4TwoLineFit TLF_RAD01"
    bind $rb5 <Button-2> "red4Update red4TwoLineFit TLF_RAD01"
    bind $rb6 <Button-2> "red4Update red4TwoLineFit TLF_RAD01"
    bind $rb7 <Button-2> "red4Update red4TwoLineFit TLF_RAD01"
    bind $rb8 <Button-2> "red4Update red4TwoLineFit TLF_RAD01"

# Create, pack and bind row/col widget
    if {$type == "row"} {
      set Red4Widgets(TLF_LAB05) [label $basetop.lb2 -text "X Start"]
      set Red4Widgets(TLF_ENT04) [entry $basetop.e2 -width 10]
      set Red4Widgets(TLF_LAB06) [label $basetop.lb3 -text "X End"]
      set Red4Widgets(TLF_ENT05) [entry $basetop.e3 -width 10]
    } else {
      set Red4Widgets(TLF_LAB05) [label $basetop.lb2 -text "Y Start"]
      set Red4Widgets(TLF_ENT04) [entry $basetop.e2 -width 10]
      set Red4Widgets(TLF_LAB06) [label $basetop.lb3 -text "Y End"]
      set Red4Widgets(TLF_ENT05) [entry $basetop.e3 -width 10]
    }
    $Red4Widgets(TLF_ENT04) insert end $Red4Widgets(TLF_DXS)
    $Red4Widgets(TLF_ENT05) insert end $Red4Widgets(TLF_DXE)
    pack $Red4Widgets(TLF_LAB05) $Red4Widgets(TLF_ENT04) -in $basetop -side left -pady 2m
    pack $Red4Widgets(TLF_ENT05) $Red4Widgets(TLF_LAB06) -in $basetop -side right -pady 2m
    bind $Red4Widgets(TLF_LAB05) <Button-2> "red4Update red4TwoLineFit ALL"
    bind $Red4Widgets(TLF_ENT04) <Button-2> "red4Update red4TwoLineFit TLF_ENT04"
    bind $Red4Widgets(TLF_LAB06) <Button-2> "red4Update red4TwoLineFit ALL"
    bind $Red4Widgets(TLF_ENT05) <Button-2> "red4Update red4TwoLineFit TLF_ENT05"
    bind $Red4Widgets(TLF_ENT04) <Double-Button-2> "$Red4Widgets(TLF_ENT04) delete 0 end"
    bind $Red4Widgets(TLF_ENT05) <Double-Button-2> "$Red4Widgets(TLF_ENT05) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]

# If CANCEL blow the box away and return
    if {$bv != 0} {
      cgs4drCursor arrow green black
      destroy .red4Dialogue
      return
    }

# Get the values from the dialogue box
    set Red4Widgets(RO)      [string trim [$Red4Widgets(TLF_ENT01) get]]
    set Red4Widgets(TLF_DRS) [string trim [$Red4Widgets(TLF_ENT02) get]]
    set Red4Widgets(TLF_DRE) [string trim [$Red4Widgets(TLF_ENT03) get]]
    set Red4Widgets(TLF_DXS) [string trim [$Red4Widgets(TLF_ENT04) get]]
    set Red4Widgets(TLF_DXE) [string trim [$Red4Widgets(TLF_ENT05) get]]

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
     cgs4drInform $taskname "red4TwoLineFit error : A dataset has not been specified properly!"
     destroy .red4Dialogue
     cgs4drCursor arrow green black
     return
   } else {

# Remove the dialog box otherwise Figaro SPLOT can't grab input focus!
     destroy .red4Dialogue
   }
   set Red4Widgets(RO) $Red4Widgets(RO)

# Open the engineering log file
   set message "Opening engineering log file"
   cgs4drInform $taskname $message
   $taskname obey open_log "" -inform "cgs4drInform $taskname %V" -endmsg {set open_log_done 1}
   tkwait variable open_log_done

# Reset the engineering functions
   set message "Resetting engineering functions in reduction monolith"
   cgs4drInform $taskname $message
   $taskname obey eng_reset "" -inform "cgs4drInform $taskname %V" -endmsg {set eng_reset_done 1}
   tkwait variable eng_reset_done

# Log some comments to file
   set message "Logging some comments in the engineering log file"
   cgs4drInform $taskname $message
   $taskname obey log_comment "comment=' '" -inform "cgs4drInform $taskname %V" -endmsg {set log_comment1_done 1}
   tkwait variable log_comment1_done
   if {$type == "row"} {
      set comment "Analysing slit alignment in $Red4Widgets(RO)"
   } else {
      set comment "Analysing dispersion alignment in $Red4Widgets(RO)"
   }
   $taskname obey log_comment "comment='$comment'" -inform "cgs4drInform $taskname %V" -endmsg {set log_comment2_done 1}
   tkwait variable log_comment2_done

# Extract the first spectrum
   set nrows [expr $Red4Widgets(TLF_RAD01) / 2]
   set exts [expr {(1 > [expr $Red4Widgets(TLF_DRS) - $nrows]) ? 1 : [expr $Red4Widgets(TLF_DRS) - $nrows]}]
   set exte [expr {(256 < [expr $Red4Widgets(TLF_DRS) + $nrows]) ? 256 : [expr $Red4Widgets(TLF_DRS) + $nrows]}]
   if {$type == "row"} {
     set message "Extracting spectrum from rows $exts to $exte in $Red4Widgets(RO) to $Red4Widgets(RO)_spc1"
     cgs4drInform $taskname $message
     $taskname obey extract4 "image=$Red4Widgets(RO) ystart=$exts yend=$exte spectrum=$Red4Widgets(RO)_spc1" \
        -inform "cgs4drInform $taskname %V" -endmsg {set ext1_done 1}
     tkwait variable ext1_done
   } else {
     set message "Extracting spectrum from columns $exts to $exte in $Red4Widgets(RO) to $Red4Widgets(RO)_spc1"
     cgs4drInform $taskname $message
     figaro1 obey ystract "image=$Red4Widgets(RO) xstart=$exts xend=$exte spectrum=$Red4Widgets(RO)_spc1" \
        -inform "cgs4drInform $taskname %V" -endmsg {set ext1_done 1}
     tkwait variable ext1_done
   }

# Extract the second spectrum
   set exts [expr {(1 > [expr $Red4Widgets(TLF_DRE) - $nrows]) ? 1 : [expr $Red4Widgets(TLF_DRE) - $nrows]}]
   set exte [expr {(256 < [expr $Red4Widgets(TLF_DRE) + $nrows]) ? 256 : [expr $Red4Widgets(TLF_DRE) + $nrows]}]
   if {$type == "row"} {
     set message "Extracting spectrum from rows $exts to $exte in $Red4Widgets(RO) to $Red4Widgets(RO)_spc2"
     cgs4drInform $taskname $message
     $taskname obey extract4 "image=$Red4Widgets(RO) ystart=$exts yend=$exte spectrum=$Red4Widgets(RO)_spc2" \
       -inform "cgs4drInform $taskname %V" -endmsg {set ext2_done 1}
     tkwait variable ext2_done
   } else {
     set message "Extracting spectrum from columns $exts to $exte in $Red4Widgets(RO) to $Red4Widgets(RO)_spc2"
     cgs4drInform $taskname $message
     figaro1 obey ystract "image=$Red4Widgets(RO) xstart=$exts xend=$exte spectrum=$Red4Widgets(RO)_spc2" \
        -inform "cgs4drInform $taskname %V" -endmsg {set ext2_done 1}
     tkwait variable ext2_done
   }

# Run the figaro SPLOT function on spectrum 1
   set message "Plotting first extracted spectrum in red"
   cgs4drInform $taskname $message
   set params "soft='xwindows;$env(PID)xwin' spectrum=$Red4Widgets(RO)_spc1 whole=F xstart=$Red4Widgets(TLF_DXS) xend=$Red4Widgets(TLF_DXE) autoscale=T axes=T erase=T lines=T hardcopy=F colour='red'"
   if {$type == "row"} {
     set params "$params label='Row 1'"
   } else {
     set params "$params label='Column 1'"
   }
   figaro1 obey splot $params -inform "cgs4drInform $taskname %V" -endmsg {set splot1_done 1}
   tkwait variable splot1_done

# Run the figaro SPLOT function on spectrum 2
   set message "Plotting second extracted spectrum in yellow"
   cgs4drInform $taskname $message
   set params "soft='xwindows;$env(PID)xwin' spectrum=$Red4Widgets(RO)_spc2 whole=F xstart=$Red4Widgets(TLF_DXS) xend=$Red4Widgets(TLF_DXE) autoscale=T axes=T erase=F lines=T hardcopy=F colour='yellow'"
   if {$type == "row"} {
     set params "$params label='Row 2'"
   } else {
     set params "$params label='Column 2'"
   }
   figaro1 obey splot $params -inform "cgs4drInform $taskname %V" -endmsg {set splot2_done 1}
   tkwait variable splot2_done

# Do EMLT etc on first spectrum
   if {[file exists emlt.lis] == 1} {exec /usr/bin/rm -rf emlt.lis}
   set message "Running EMLT on first spectrum"
   cgs4drInform $taskname $message
   figaro2 obey emlt "spectrum=$Red4Widgets(RO)_spc1 xstart=$Red4Widgets(TLF_DXS) xend=$Red4Widgets(TLF_DXE) lines=0 moments=F" \
     -inform "cgs4drInform $taskname %V" -endmsg {set emlt1_done 1}
   tkwait variable emlt1_done

   if {[file exists emlt.lis] == 1} {
     catch {exec /usr/bin/cp emlt.lis $env(CGS4_ENG)/emlt.lis}
     $taskname obey copy_to_log "source_file='emlt.lis'" -inform "cgs4drInform $taskname %V" -endmsg {set copy1_done 1}
     tkwait variable copy1_done
     $taskname obey read_emlt "xmin=$Red4Widgets(TLF_DXS) xmax=$Red4Widgets(TLF_DXE)" -inform "cgs4drInform $taskname %V" -endmsg {set remlt1_done 1}
     tkwait variable remlt1_done
     $taskname obey log_emlt "ycen=$Red4Widgets(TLF_DRS)" -inform "cgs4drInform $taskname %V" -endmsg {set lemlt1_done 1}
     tkwait variable lemlt1_done
     $taskname obey report_emlt "" -inform "cgs4drInform $taskname %V" -endmsg {set remlt2_done 1}
     tkwait variable remlt2_done
   } else {
     set message "red4TwoLineFit error : Unable to locate emlt.lis file!"
     cgs4drInform $taskname $message
     cgs4drCursor arrow green black
     return
   }

# Do EMLT etc on second spectrum
   if {[file exists emlt.lis] == 1} {exec /usr/bin/rm -rf emlt.lis}
   set message "Running EMLT on second spectrum"
   cgs4drInform $taskname $message
   figaro2 obey emlt "spectrum=$Red4Widgets(RO)_spc2 xstart=$Red4Widgets(TLF_DXS) xend=$Red4Widgets(TLF_DXE) lines=0 moments=F" \
     -inform "cgs4drInform $taskname %V" -endmsg {set emlt2_done 1}
   tkwait variable emlt2_done

   if {[file exists emlt.lis] == 1} {
     catch {exec /usr/bin/cp emlt.lis $env(CGS4_ENG)/emlt.lis}
     $taskname obey copy_to_log "source_file='emlt.lis'" -inform "cgs4drInform $taskname %V" -endmsg {set copy2_done 1}
     tkwait variable copy2_done
     $taskname obey read_emlt "xmin=$Red4Widgets(TLF_DXS) xmax=$Red4Widgets(TLF_DXE)" -inform "cgs4drInform $taskname %V" -endmsg {set remlt3_done 1}
     tkwait variable remlt3_done
     $taskname obey log_emlt "ycen=$Red4Widgets(TLF_DRE)" -inform "cgs4drInform $taskname %V" -endmsg {set lemlt2_done 1}
     tkwait variable lemlt2_done
     $taskname obey report_emlt "" -inform "cgs4drInform $taskname %V" -endmsg {set remlt4_done 1}
     tkwait variable remlt4_done
   } else {
     set message "red4TwoLineFit error : Unable to locate emlt.lis file!"
     cgs4drInform $taskname $message
     cgs4drCursor arrow green black
     return
   }

# Calculate the slit angle
    set message "Calculating slit angle"
    cgs4drInform $taskname $message
    $taskname obey calc_slit_angle "ycen_p=$Red4Widgets(TLF_DRS) ycen=$Red4Widgets(TLF_DRE)" -inform "cgs4drInform $taskname %V" -endmsg {set cslit_done 1}
    tkwait variable cslit_done
    $taskname obey report_slit "ycen_p=$Red4Widgets(TLF_DRS) ycen=$Red4Widgets(TLF_DRE)" -inform "cgs4drInform $taskname %V" -endmsg {set rslit_done 1}
    tkwait variable rslit_done
    $taskname obey log_slit "ycen_p=$Red4Widgets(TLF_DRS) ycen=$Red4Widgets(TLF_DRE)" -inform "cgs4drInform $taskname %V" -endmsg {set lslit_done 1}
    tkwait variable lslit_done
    $taskname obey read_obs "observation=$Red4Widgets(RO)" -inform "cgs4drInform $taskname %V" -endmsg {set robs_done 1}
    tkwait variable robs_done
    $taskname obey log_type1 "" -inform "cgs4drInform $taskname %V" -endmsg {set ltype_done 1}
    tkwait variable ltype_done

# Close the engineering log file
   #set message "Closing engineering log file"
   #cgs4drInform $taskname $message
   #$taskname obey close_log "" -inform "cgs4drInform $taskname %V" -endmsg {set close_log_done 1}
   #tkwait variable close_log_done

# Issue message
    set message "Two line fitting function complete"
    cgs4drInform $taskname $message
    cgs4drCursor arrow green black
}
