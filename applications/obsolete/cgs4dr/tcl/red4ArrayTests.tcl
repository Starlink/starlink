proc red4ArrayTests {taskname} {
#+
# Creates a dialog box for red4 action
#-
   global env
   global Red4Widgets
   global stats:sigma
   global stats:median
   global stats:mode

# Check to see if task is busy
   set status [cgs4drCheckTask red4]
   if {$status!=0} {return}

# Create dialog box
   if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
   set frame [dialogStart .red4Dialogue "Red4 Array Tests" 0 OK Cancel]
   cgs4drCursor pirate orange black
   .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
   set Red4Widgets(AT_LAB01) [label $frame.tl1 -text "Start Observation Number"]
   set Red4Widgets(AT_ENT01) [entry $frame.te1 -width 10]
   pack $Red4Widgets(AT_LAB01) $Red4Widgets(AT_ENT01) -side left
   $Red4Widgets(AT_ENT01) insert end $Red4Widgets(ATD)

# Bind the defaults to button-2
   bind $Red4Widgets(AT_LAB01) <Button-2> "$Red4Widgets(AT_ENT01) delete 0 end; $Red4Widgets(AT_ENT01) insert end 1"
   bind $Red4Widgets(AT_ENT01) <Button-2> "$Red4Widgets(AT_ENT01) delete 0 end; $Red4Widgets(AT_ENT01) insert end 1"
   bind $Red4Widgets(AT_ENT01) <Double-Button-2> "$Red4Widgets(AT_ENT01) delete 0 end"

# Show the dialog box and return if OK was not pressed
   set bv [dialogShow .red4Dialogue .red4Dialogue]
   cgs4drCursor watch red white
   if {$bv != 0} {
     cgs4drCursor arrow green black
     destroy .red4Dialogue
     return
   }

# Get the observation number etc
   set Red4Widgets(ATD)  [string trim [$Red4Widgets(AT_ENT01) get]]
   destroy .red4Dialogue
   if {$Red4Widgets(ATD) <=0 } {
     cgs4drClear $taskname
     cgs4drInform $taskname "red4ArrayTests error : Input number must be positive!"
     cgs4drCursor arrow green black
     return
   }

# Print out some messages
   cgs4drInform $taskname "Array Tests Analysis for $env(CGS4_DATE)"

# Subtract two images
   set in1 "\$RODIR/ro$env(CGS4_DATE)_[expr $Red4Widgets(ATD) + 2]"
   set in2 "\$RODIR/ro$env(CGS4_DATE)_[expr $Red4Widgets(ATD) + 3]"
   set out "\$RODIR/im3minus4"
   cgs4drInform $taskname "Subtracting $in2 from $in1 to create $out"
   $taskname obey isub4 "image1=$in1 image2=$in2 output=$out errors='GAUSSIAN'" -inform "cgs4drInform $taskname %V" -endmsg {set subdone 1}
   tkwait variable subdone

# Do the stats on the subtracted image
   set param "data=$out plane='DATA' WHOLE='T' AUTOSCALE='F' LOW=-50 HIGH=50 MEAN=0 SIGMA=0 MEDIAN=0 MODE=0"
   cgs4drInform $taskname "Evaluating statistics on $out"
   $taskname obey stats "$param" -inform "cgs4drInform $taskname %V" -endmsg {set statdone 1}
   tkwait variable statdone

# Now get the standard deviation
   set stats:sigma -1
   $taskname get stats:sigma -getresponse "red4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
   tkwait variable stats:sigma

# Subtract two images
   set in1 "\$RODIR/ro$env(CGS4_DATE)_[expr $Red4Widgets(ATD) + 5]"
   set in2 "\$RODIR/ro$env(CGS4_DATE)_[expr $Red4Widgets(ATD) + 4]"
   set out "\$RODIR/im6minus5"
   cgs4drInform $taskname "Subtracting $in2 from $in1 to create $out"
   $taskname obey isub4 "image1=$in1 image2=$in2 output=$out errors='GAUSSIAN'" -inform "cgs4drInform $taskname %V" -endmsg {set newsubdone 1}
   tkwait variable newsubdone

# Do the stats on the subtracted image
   set param "data=$out plane='DATA' WHOLE='T' AUTOSCALE='T' LOW=0 HIGH=100000.0 MEAN=0 SIGMA=0 MEDIAN=0 MODE=0"
   cgs4drInform $taskname "Evaluating statistics on $out"
   $taskname obey stats "$param" -inform "cgs4drInform $taskname %V" -endmsg {set newstatdone 1}
   tkwait variable newstatdone

# Now get the median and mode
   set stats:median -1
   $taskname get stats:median -getresponse "red4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
   tkwait variable stats:median
   set stats:mode -1
   $taskname get stats:mode -getresponse "red4GetResponse $taskname %N %V %S" -inform "cgs4drInform $taskname %V"
   tkwait variable stats:mode

# Output the results
  set stare_rn  [expr [expr ${stats:sigma} / sqrt(2)] * 6.0]
  set median_dc [expr [expr ${stats:median} / 55.0] * 6.0]
  set modal_dc  [expr [expr ${stats:mode} / 55.0] * 6.0]
  set message "Double correlated read noise = $stare_rn electrons"
  if {$stare_rn < 30.0} {
    cgs4drInform $taskname "$message is LOW"
  } elseif {$stare_rn > 50.0} {
    cgs4drInform $taskname "$message is HIGH"
  } else {
    cgs4drInform $taskname "$message is NOMINAL"
  }
  set message "Median dark current = $median_dc electrons/second"
  if {$median_dc < -0.5} {
    cgs4drInform $taskname "$message is LOW"
  } elseif {$median_dc > 2.0} {
    cgs4drInform $taskname "$message is HIGH"
  } else {
    cgs4drInform $taskname "$message is NOMINAL"
  }
  set message "Modal dark current = $modal_dc electrons/second"
  if {$modal_dc < -0.5} {
    cgs4drInform $taskname "$message is LOW"
  } elseif {$modal_dc > 2.0} {
    cgs4drInform $taskname "$message is HIGH"
  } else {
    cgs4drInform $taskname "$message is NOMINAL"
  }

# If at UKIRT, write the results to the file
  if {$env(DOMAIN) == "ukirt.jach.hawaii.edu."} {
    set fid   [open /ukirt_sw/logs/cgs4_array_tests.log a+]
    puts $fid [format "%6d\t\t %8.4f\t %8.4f" $env(CGS4_DATE) $stare_rn $median_dc]
    close $fid
  }
  cgs4drCursor arrow green black
}
