proc red4FileObs {taskname type} {
#+
# Creates a dialog box for red4 action
#-
    global env
    global Red4Widgets

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    set htype [string trim [string toupper $type]]
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 File Observation as $htype" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set Red4Widgets(FO_LABEL) [label $frame.lb -text "Filename"]
    set Red4Widgets(FO_ENTRY) [entry $frame.en -width 40]
    pack $Red4Widgets(FO_LABEL) $Red4Widgets(FO_ENTRY) -in $frame -side left
    $Red4Widgets(FO_ENTRY) insert end $Red4Widgets(OB)

# Bind the defaults
    bind $Red4Widgets(FO_LABEL) <Button-2> "red4Update red4FileObs FO_ENTRY"
    bind $Red4Widgets(FO_ENTRY) <Button-2> "red4Update red4FileObs FO_ENTRY"
    bind $Red4Widgets(FO_ENTRY) <Double-Button-2> "$Red4Widgets(FO_ENTRY) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white
      set obs [string trim [$Red4Widgets(FO_ENTRY) get]]
      if {$obs=="" || $obs==$Red4Widgets(DOB)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4FileObs error : A dataset has not been specified properly!"
      } else {

#     If type is whatever or bad, just do it
        if {$type=="whatever_it_is" || $type=="bad"} {
          set Red4Widgets(OB) $obs
          $taskname obey file_obs "observation=$obs type=$type" -inform "cgs4drInform $taskname %V"

#     Else file as calib if required
        } elseif {$type=="calib"} {
          $taskname obey file_calib \
            "observation=$obs change_label='TRUE' newlabel='Wavelength' newunits='Microns'" \
            -inform "cgs4drInform $taskname %V"

#     Otherwise we need to change a fits item
        } else {
          set uspos [string first "_" $obs]
          if {$uspos>0} {
            set number [string range $obs [expr $uspos + 1] end]
          } else {
            set number -1
          }
          set status [catch {incr number 0}]
          if {$status==0 && $number>0} {
            set Red4Widgets(OB) $obs
            set Red4Widgets(RO) \$RODIR/ro$env(CGS4_DATE)_$number
            $taskname obey poke_fits "data=$Red4Widgets(RO) item='OBSTYPE' method='WRITE' \
              mode='SILENT' access='C' cvalue=$type" -inform "cgs4drInform $taskname %V" -endmsg {set done 1}
            tkwait variable done
            $taskname obey file_obs "observation=$obs type=$type" -inform "cgs4drInform $taskname %V"
          } else {
            cgs4drClear $taskname
            cgs4drInform $taskname "red4FileObs error : Observation number must be a positive integer!"
          }
        }
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
