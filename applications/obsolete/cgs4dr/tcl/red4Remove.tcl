proc red4Remove {taskname type} {
#+
# Creates a dialog box for red4 action
#-
    global env
    global Red4Widgets

# Check to see if task is busy
    set status [cgs4drCheckTask red4]
    if {$status!=0} {return}

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    if {$type=="obs"} {
      set frame [dialogStart .red4Dialogue "Red4 Delete Reduced Observation File" 0 OK Cancel]
    } elseif {$type=="grp"} {
      set frame [dialogStart .red4Dialogue "Red4 Delete Reduced Group File" 0 OK Cancel]
    }
    cgs4drCursor pirate orange black
   .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set Red4Widgets(RM_LABEL) [label $frame.lb -text "Filename"]
    set Red4Widgets(RM_ENTRY) [entry $frame.en -width 40]
    pack $Red4Widgets(RM_LABEL) $Red4Widgets(RM_ENTRY) -in $frame -side left
    if {$type=="obs" && [string trim [$Red4Widgets(RM_ENTRY) get]]==""} {
      $Red4Widgets(RM_ENTRY) insert end $Red4Widgets(RO)
    } elseif {$type=="grp" && [string trim [$Red4Widgets(RM_ENTRY) get]]==""} {
      $Red4Widgets(RM_ENTRY) insert end $Red4Widgets(RG)
    }

# Bind defaults to button (different from rest!)
    bind $Red4Widgets(RM_LABEL) <Button-2> "red4Update red4Remove $type"
    bind $Red4Widgets(RM_ENTRY) <Button-2> "red4Update red4Remove $type"
    bind $Red4Widgets(RM_ENTRY) <Double-Button-2> "$Red4Widgets(RM_ENTRY) delete 0 end"

# Show the dialog box
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white

    # Get the entry and strip off the number
      set obs [string trim [$Red4Widgets(RM_ENTRY) get]]
      set uspos [string first "_" $obs]
      if {$uspos > 0} {
        set number [string range $obs [expr $uspos + 1] end]
      } else {
        set number -1
      }

    # If it doesn't match our criteria signal an error and abort
      if {$obs=="" || $obs==$Red4Widgets(DRG) || $obs==$Red4Widgets(DRO) || $obs==$Red4Widgets(DST) || $number<=0} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Remove error : A dataset has not been specified properly!"
      } else {

    # OK we have a cosher RO or RG or ST but don't know if it's a DST or SDF so try both
        if {$type=="obs" && [string first "ro$env(CGS4_DATE)" [string tolower $obs]]!=-1} {
          set Red4Widgets(RO) $obs
          set sdf_file [string trim $env(RODIR)/ro$env(CGS4_DATE)_${number}.sdf]
          set dst_file [string trim $env(RODIR)/ro$env(CGS4_DATE)_${number}.dst]
        } elseif {$type=="grp" && [string first "rg$env(CGS4_DATE)" [string tolower $obs]]!=-1} {
          set Red4Widgets(RG) $obs
          set sdf_file [string trim $env(RGDIR)/rg$env(CGS4_DATE)_${number}.sdf]
          set dst_file [string trim $env(RGDIR)/rg$env(CGS4_DATE)_${number}.dst]
        } elseif {$type=="grp" && [string first "st$env(CGS4_DATE)" [string tolower $obs]]!=-1} {
          set Red4Widgets(RG) $obs
          set sdf_file [string trim $env(RGDIR)/st$env(CGS4_DATE)_${number}.sdf]
          set dst_file [string trim $env(RGDIR)/st$env(CGS4_DATE)_${number}.dst]
        }
        set status -1
        if {[file exists $sdf_file]==1} {set status [catch {exec /usr/bin/rm -rf ${sdf_file}}]}
        if {[file exists $dst_file]==1} {set status [catch {exec /usr/bin/rm -rf ${dst_file}}]}
        if {$status!=0} {
          cgs4drClear $taskname
          cgs4drInform $taskname "red4Remove error : Unable to delete file ${obs}!"
        } else {
          cgs4drInform $taskname "Deleted file ${obs}"
        }
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
