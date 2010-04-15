proc red4Calibrate {taskname lamp} {
#+
# Creates a dialog box for red4 action
#-
   global env
   global Red4Widgets

# Check to see if task is busy
   set status [cgs4drCheckTask red4]
   if {$status!=0} {return}

# Check to see figaro1 and figaro3 tasks are loaded
   if {[info commands figaro1*] == ""} {
     set message "Loading figaro1"
     cgs4drInform $taskname $message
     adamtask figaro1 /star/bin/figaro/figaro1
   }
   if {[info commands figaro3*] == ""} {
     set message "Loading figaro3"
     cgs4drInform $taskname $message
     adamtask figaro3 /star/bin/figaro/figaro3
   }

# Create dialog box
   set hlamp [string trim [string toupper $lamp]]
   if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
   set frame [dialogStart .red4Dialogue "Red4 Wavelength Calibrate using $hlamp" 0 OK Cancel]
   cgs4drCursor pirate orange black
   .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
   set top [frame $frame.top]
   set mid [frame $frame.mid]
   set bot [frame $frame.bot]
   pack $top $bot $mid -in $frame -side top

   set Red4Widgets(WC_LAB01) [label $top.tl1 -text "Filename"]
   set Red4Widgets(WC_ENT01) [entry $top.te1 -width 40]
   pack $Red4Widgets(WC_LAB01) $Red4Widgets(WC_ENT01) -in $top -side left
   $Red4Widgets(WC_ENT01) insert end $Red4Widgets(RO)

   set Red4Widgets(WC_LAB02) [label $bot.bl1 -text "Extract Row Start"]
   set Red4Widgets(WC_ENT02) [entry $bot.be1]
   pack $Red4Widgets(WC_LAB02) $Red4Widgets(WC_ENT02) -in $bot -side left
   $Red4Widgets(WC_ENT02) insert end $Red4Widgets(CAL_RST)

   set Red4Widgets(WC_LAB03) [label $bot.bl2 -text "Extract Row End"]
   set Red4Widgets(WC_ENT03) [entry $bot.be2]
   pack $Red4Widgets(WC_ENT03) $Red4Widgets(WC_LAB03) -in $bot -side right
   $Red4Widgets(WC_ENT03) insert end $Red4Widgets(CAL_REN)

   set Red4Widgets(WC_LAB04) [label $mid.ml1 -text "Polynomial Order"]
   set Red4Widgets(WC_ENT04) [entry $mid.me1]
   pack $Red4Widgets(WC_LAB04) $Red4Widgets(WC_ENT04) -in $mid -side left
   $Red4Widgets(WC_ENT04) insert end $Red4Widgets(CAL_ORD)

   set Red4Widgets(WC_LAB05) [label $mid.ml2 -text "Arcline Sigma"]
   set Red4Widgets(WC_ENT05) [entry $mid.me2]
   pack $Red4Widgets(WC_ENT05) $Red4Widgets(WC_LAB05) -in $mid -side right
   $Red4Widgets(WC_ENT05) insert end $Red4Widgets(CAL_SIG)

# Bind the defaults to button-2
   bind $Red4Widgets(WC_LAB01) <Button-2> "red4Update red4Calibrate ALL"
   bind $Red4Widgets(WC_LAB02) <Button-2> "red4Update red4Calibrate ALL"
   bind $Red4Widgets(WC_LAB03) <Button-2> "red4Update red4Calibrate ALL"
   bind $Red4Widgets(WC_LAB04) <Button-2> "red4Update red4Calibrate ALL"
   bind $Red4Widgets(WC_LAB05) <Button-2> "red4Update red4Calibrate ALL"
   bind $Red4Widgets(WC_ENT01) <Button-2> "red4Update red4Calibrate WC_ENT01"
   bind $Red4Widgets(WC_ENT02) <Button-2> "red4Update red4Calibrate WC_ENT02"
   bind $Red4Widgets(WC_ENT03) <Button-2> "red4Update red4Calibrate WC_ENT03"
   bind $Red4Widgets(WC_ENT04) <Button-2> "red4Update red4Calibrate WC_ENT04"
   bind $Red4Widgets(WC_ENT05) <Button-2> "red4Update red4Calibrate WC_ENT05"
   bind $Red4Widgets(WC_ENT01) <Double-Button-2> "$Red4Widgets(WC_ENT01) delete 0 end"
   bind $Red4Widgets(WC_ENT02) <Double-Button-2> "$Red4Widgets(WC_ENT02) delete 0 end"
   bind $Red4Widgets(WC_ENT03) <Double-Button-2> "$Red4Widgets(WC_ENT03) delete 0 end"
   bind $Red4Widgets(WC_ENT04) <Double-Button-2> "$Red4Widgets(WC_ENT04) delete 0 end"
   bind $Red4Widgets(WC_ENT05) <Double-Button-2> "$Red4Widgets(WC_ENT05) delete 0 end"

# Show the dialog box and return if OK was not pressed
   set bv [dialogShow .red4Dialogue .red4Dialogue]
   cgs4drCursor watch red white
   if {$bv != 0} {
     cgs4drCursor arrow green black
     destroy .red4Dialogue
     return
   }

# Get the observation number etc
   set Red4Widgets(RO)      [string trim [$Red4Widgets(WC_ENT01) get]]
   set Red4Widgets(CAL_RST) [string trim [$Red4Widgets(WC_ENT02) get]]
   set Red4Widgets(CAL_REN) [string trim [$Red4Widgets(WC_ENT03) get]]
   set Red4Widgets(CAL_ORD) [string trim [$Red4Widgets(WC_ENT04) get]]
   set Red4Widgets(CAL_SIG) [string trim [$Red4Widgets(WC_ENT05) get]]

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
     cgs4drInform $taskname "red4Calibrate error : A dataset has not been specified properly!"
     destroy .red4Dialogue
     cgs4drCursor arrow green black
     return
   } else {

# Remove the dialog box otherwise Figaro ARC can't grab input focus!
     destroy .red4Dialogue
   }

# Set some default filenames
   set Red4Widgets(SP) $Red4Widgets(RO)_spc
   set Red4Widgets(OB) \$ODIR/o$env(CGS4_DATE)_$number
   set Red4Widgets(CA) \$RODIR/ca$env(CGS4_DATE)_$number

# Extract the spectrum
   set message "Extracting spectrum from $Red4Widgets(RO) to $Red4Widgets(SP)"
   cgs4drInform $taskname $message
   set param "image=$Red4Widgets(RO) ystart=$Red4Widgets(CAL_RST) yend=$Red4Widgets(CAL_REN) spectrum=$Red4Widgets(SP)"
   $taskname obey extract4 "$param" -inform "cgs4drInform $taskname %V" -endmsg {set ext_done 1}
   tkwait variable ext_done

# Setup a soft device
   set message "Setting soft device to xwindows"
   figaro1 obey soft "softdev='xwindows'" -inform "cgs4drInform $taskname %V" -endmsg {set soft_done 1}
   tkwait variable soft_done

# Run the figaro ARC function
   set message "Starting figaro ARC function"
   cgs4drInform $taskname $message
   set arc cgs4_${lamp}.arc
   set param "spectrum=$Red4Widgets(SP) arctype=$arc sigma=$Red4Widgets(CAL_SIG) order=$Red4Widgets(CAL_ORD)"
   set param "$param output=$Red4Widgets(CA) previous=F"
   figaro3 obey arc "$param" -inform "cgs4drInform $taskname %V" -endmsg {set arc_done 1}
   tkwait variable arc_done

# Divide by 000 and file as a calibration
   set message "Changing from angstroms to microns"
   cgs4drInform $taskname $message
   set param "image=$Red4Widgets(CA) factor=10000.0 output=$Red4Widgets(CA)"
   figaro1 obey xcdiv "$param" -inform "cgs4drInform $taskname %V" -endmsg {set xcd_done 1}
   tkwait variable xcd_done

# Now file it as a calibration
   set message "Filing $Red4Widgets(CA) as a calibration"
   cgs4drInform $taskname $message
   set param "observation=$Red4Widgets(OB) change_label=TRUE newlabel='Wavelength' newunits='Microns'"
   $taskname obey file_calib "$param" -inform "cgs4drInform $taskname %V" -endmsg {set cal_done 1}
   tkwait variable cal_done

# Issue message
   set message "Calibration procedure complete"
   cgs4drInform $taskname $message
   cgs4drCursor arrow green black
}
