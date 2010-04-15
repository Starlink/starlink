proc cred4Configs {taskname} {
#-
# Saves and restores configs
#-

# Get some default values
    global env
    global config_file
    global Cred4NoticeBoard
    global Cred4Widgets
    global cgs4drHtml

# Create dialog box.
    if {[winfo exists .cred4Dialogue]} {destroy .cred4Dialogue}
    set frame [dialogStart .cred4Dialogue "Cred4 Configuration Files" 0 Save Restore Cancel]
    cgs4drCursor pirate orange black
    .cred4Dialogue config -cursor {arrow green black}

    set l1 [label $frame.l1 -text "Filename"]
    set Cred4Widgets(CONFIG) [entry $frame.config -width 60 -relief sunken -bd 2 -textvariable config_file]
    pack $l1 $Cred4Widgets(CONFIG) -in $frame -side left

    set config_file [string trim $config_file]
    if {$config_file == ""} {
	$Cred4Widgets(CONFIG) delete 0 end
	$Cred4Widgets(CONFIG) insert 0 [string trim $env(CGS4_CONFIG)/default.cred4]
    }

# Bind the defaults and on-line help
    bind $l1                   <Button-2> "cred4Update cred4Configs ALL"
    bind $Cred4Widgets(CONFIG) <Button-2> "cred4Update cred4Configs ALL"
    bind $Cred4Widgets(CONFIG) <Double-Button-2> "$Cred4Widgets(CONFIG) delete 0 end"
    bind $l1                   <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ConfigsBox1.html"
    bind $Cred4Widgets(CONFIG) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/cred4ConfigsBox1.html"

# Show the dialog box
    set bv [dialogShow .cred4Dialogue .cred4Dialogue]

# Do something in response to SAVE button
    if {$bv == 0} {
      cgs4drCursor watch red white
      set config_file [string trim [$Cred4Widgets(CONFIG) get]]
      if {[file extension ${config_file}] == ""} {set config_file "${config_file}.cred4"}
      if {$config_file == ""} {
        set message "cred4Configs error : You must specify a configuration filename!"
        cgs4drInform $taskname $message
      } else {
        if {[file exists $config_file] == 1 } {
          if {[winfo exists .cred4File]} {destroy .cred4File}
          set frame2 [dialogStart .cred4File "Cred4 Configuration Files" 0 OK Cancel]
          set label2 [label $frame2.lab2 -text "File $config_file already exists!" -width 60]
          set label3 [label $frame2.lab3 -text "Overwrite it?" -width 60]
          pack $label2 $label3 -in $frame2 -side top
          .cred4Dialogue config -cursor {pirate orange black}
          .cred4File config -cursor {arrow green black}
          set bv2 [dialogShow .cred4File .cred4File]
	  if {$bv2==0} {
            exec /usr/bin/rm -f $config_file
            set save_done -1
            $taskname obey save_config "config_file=$config_file" -inform "cgs4drInform $taskname %V" -endmsg {set save_done 1}
            tkwait variable save_done
          }
          destroy .cred4File
	} else {
          set save_done -1
          $taskname obey save_config "config_file=$config_file" -inform "cgs4drInform $taskname %V" -endmsg {set save_done 1}
          tkwait variable save_done
	}
      }

# Do something in response to RESTORE button
    } elseif {$bv == 1} {
      cgs4drCursor watch red white
      set config_file [string trim [$Cred4Widgets(CONFIG) get]]
      if {[file extension ${config_file}] == ""} {set config_file "${config_file}.cred4"}
      if {$config_file == ""} {
        set message "cred4Configs error : You must specify a configuration filename!"
        cgs4drInform $taskname $message
      } else {
        if {[file exists $config_file] == 0 } {
          set message "cred4Configs error : File $config_file does not exist!"
          cgs4drInform $taskname $message
	} else {
          set rest_done -1
          $taskname obey restore_config "config_file=$config_file" -inform "cgs4drInform $taskname %V" -endmsg {set rest_done 1}
          tkwait variable rest_done
	}
      }

# Restore the configuration in response to CANCEL button
    } else {
      cgs4drCursor watch red white
      set config_file [string trim [$Cred4Widgets(CONFIG) get]]
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .cred4Dialogue
}
