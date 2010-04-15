proc p4Configs {taskname} {
#-
# Saves and restores configs
#-

# Get some default values
    global env
    global p4config_file
    global P4NoticeBoard
    global P4Widgets
    global cgs4drHtml

# Create dialog box.
    if {[winfo exists .p4Dialogue]} {destroy .p4Dialogue}
    set frame [dialogStart .p4Dialogue "P4 Configuration Files" 0 Save Restore Cancel]
    cgs4drCursor pirate orange black
    .p4Dialogue config -cursor {arrow green black}

    set l1 [label $frame.l1 -text "Filename"]
    set P4Widgets(CONFIG) [entry $frame.config -width 60 -relief sunken -bd 2 -textvariable p4config_file]
    pack $l1 $P4Widgets(CONFIG) -in $frame -side left

    set p4config_file [string trim $p4config_file]
    if {$p4config_file == ""} {
	$P4Widgets(CONFIG) delete 0 end
	$P4Widgets(CONFIG) insert 0 [string trim $env(P4_CONFIG)/default.p4]
    }

# Bind the defaults and on-line help
    bind $l1                <Button-2> "$P4Widgets(CONFIG) delete 0 end; $P4Widgets(CONFIG) insert 0 $env(P4_CONFIG)/default.p4"
    bind $P4Widgets(CONFIG) <Button-2> "$P4Widgets(CONFIG) delete 0 end; $P4Widgets(CONFIG) insert 0 $env(P4_CONFIG)/default.p4"
    bind $P4Widgets(CONFIG) <Double-Button-2> "$P4Widgets(CONFIG) delete 0 end"
    bind $l1                <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ConfigsBox1.html"
    bind $P4Widgets(CONFIG) <Button-3> "cgs4drHelpDialog .helpDialog $cgs4drHtml/p4ConfigsBox1.html"

# Show the dialog box
    set bv [dialogShow .p4Dialogue .p4Dialogue]

# Do something in response to SAVE button
    if {$bv == 0} {
      cgs4drCursor watch red white
      set p4config_file [string trim [$P4Widgets(CONFIG) get]]
      if {[file extension ${p4config_file}] == ""} {set p4config_file "${p4config_file}.p4"}
      if {$p4config_file == ""} {
        set message "p4Configs error : You must specify a configuration filename!"
        cgs4drInform $taskname $message
      } else {
        if {[file exists $p4config_file] == 1 } {
          if {[winfo exists .p4File]} {destroy .p4File}
          set frame2 [dialogStart .p4File "P4 Configuration Files" 0 OK Cancel]
          set label2 [label $frame2.lab2 -text "File $p4config_file already exists!" -width 60]
          set label3 [label $frame2.lab3 -text "Overwrite it?" -width 60]
          pack $label2 $label3 -in $frame2 -side top
          .p4Dialogue config -cursor {pirate orange black}
          .p4File config -cursor {arrow green black}
          set bv2 [dialogShow .p4File .p4File]
	  if {$bv2==0} {
            exec /usr/bin/rm -f $p4config_file
            set save_done -1
            $taskname obey save "file=$p4config_file port=-1" -inform "cgs4drInform $taskname %V" -endmsg {set save_done 1}
            tkwait variable save_done
          }
          destroy .p4File
	} else {
          set save_done -1
          $taskname obey save "file=$p4config_file port=-1" -inform "cgs4drInform $taskname %V" -endmsg {set save_done 1}
          tkwait variable save_done
	}
      }

# Do something in response to RESTORE button
    } elseif {$bv == 1} {
      cgs4drCursor watch red white
      set p4config_file [string trim [$P4Widgets(CONFIG) get]]
      if {[file extension ${p4config_file}] == ""} {set p4config_file "${p4config_file}.p4"}
      if {$p4config_file == ""} {
        set message "p4Configs error : You must specify a configuration filename!"
        cgs4drInform $taskname $message
      } else {
        if {[file exists $p4config_file] == 0 } {
          set message "p4Configs error : File $p4config_file does not exist!"
          cgs4drInform $taskname $message
	} else {
          set rest_done -1
          $taskname obey restore "file=$p4config_file port=-1" -inform "cgs4drInform $taskname %V" -endmsg {set rest_done 1}
          tkwait variable rest_done

# Reset device name as appropriate
          set port 0
          while {$port <= 8} {
            set device [string trim [string tolower [nbs get ${P4NoticeBoard}.port_${port}.device_name]]]
            set scpos [string first ";" $device]
            if {$scpos > 0} {set device [string range $device 0 [expr $scpos - 1]]}
            if {[string match *window* $device] == 1} {
              set new_device "$device;$env(PID)xwin"
              nbs put ${P4NoticeBoard}.port_${port}.device_name $new_device
              set message "Resetting $device to $new_device for port $port"
              cgs4drInform $taskname $message
            }
            incr port
          }
	}
      }

# Restore the configuration in response to CANCEL button
    } else {
      cgs4drCursor watch red white
      set p4config_file [string trim [$P4Widgets(CONFIG) get]]
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .p4Dialogue
}
