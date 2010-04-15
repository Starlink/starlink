proc red4Package {taskname args} {
#+
# Creates a dialog box for red4 action
#-
    global env
    global Red4Widgets

# Create dialog box
    if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
    set frame [dialogStart .red4Dialogue "Red4 External Application ($args)" 0 OK Cancel]
    cgs4drCursor pirate orange black
    .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
    set Red4Widgets(PK_LABEL) [label $frame.lb -text "Filename"]
    set Red4Widgets(PK_ENTRY) [entry $frame.en -width 40]
    pack $Red4Widgets(PK_LABEL) $Red4Widgets(PK_ENTRY) -in $frame -side left
    $Red4Widgets(PK_ENTRY) insert end $Red4Widgets(RO)

# Bind the defaults button
   bind $Red4Widgets(PK_LABEL) <Button-2> "red4Update red4Package PK_ENTRY"
   bind $Red4Widgets(PK_ENTRY) <Button-2> "red4Update red4Package PK_ENTRY"
   bind $Red4Widgets(PK_ENTRY) <Double-Button-2> "$Red4Widgets(PK_ENTRY) delete 0 end"

# Show the dialog box
    set args [string trim [string tolower $args]]
    set bv [dialogShow .red4Dialogue .red4Dialogue]
    if {$bv==0} {
      cgs4drCursor watch red white

      set obs [string trim [$Red4Widgets(PK_ENTRY) get]]
      if {$obs=="" || $obs==$Red4Widgets(DRO)} {
        cgs4drClear $taskname
        cgs4drInform $taskname "red4Package error : A dataset has not been specified properly!"
      } else {

#       Check to see if action is known and, if not, load the task
        set pack [string trim [string tolower [lindex $args 0]]]
        if {[info commands $pack*] == ""} {
          set message "Loading ${pack}"
          cgs4drInform $taskname $message
          if {$pack == "ndfpack_mon"} {
            adamtask ndfpack_mon /star/bin/kappa/ndfpack_mon
          } elseif {$pack == "figaro1"} {
            adamtask figaro1 /star/bin/figaro/figaro1
          } elseif {$pack == "figaro2"} {
            adamtask figaro2 /star/bin/figaro/figaro2
          } elseif {$pack == "figaro3"} {
            adamtask figaro3 /star/bin/figaro/figaro3
          } elseif {$pack == "hdstrace"} {
            adamtask hdstrace /star/bin/hdstrace
          }
          set count 0
          while {[$pack path] == 0} {
            after 100
            incr count
            if {$count >100} {
              cgs4drClear $taskname
              cgs4drInform $taskname "red4Package error : Failed to start ${pack}!"
              $pack kill
              cgs4drCursor arrow green black
              return
            }
          }
        }

        set Red4Widgets(RO) $obs
        if {$args=="ndfpack_mon fitslist screen"} {
          set message "Listing FITS file headers for $obs to screen"
          cgs4drInform $taskname $message
          ndfpack_mon obey fitslist "in=$obs logfile=!" -inform "cgs4drInform $taskname %V"
        } elseif {$args=="ndfpack_mon fitslist file"} {
          set spos [string last "/" $obs]
          if {$spos>0} {
            set out [string range $obs [expr $spos + 1] end]
            set out ${out}.fits
          } else {
            set out ${obs}.fits
          }
          set message "Listing FITS file headers for $obs to file ${out}"
          cgs4drInform $taskname $message
          if {[file exists $out] == 1} {exec /usr/bin/rm -rf $out}
          ndfpack_mon obey fitslist "in=$obs logfile=$out" -inform "cgs4drInform $taskname %V"
        #} elseif {$args=="figaro1 exam"} {
        #  set message "Examining data structure $obs"
        #  cgs4drInform $taskname $message
        #  figaro1 obey exam "object=$obs" -inform "cgs4drInform $taskname %V"
        } elseif {$args=="hdstrace"} {
          if {[string first "." $obs] != -1} {
            set message "Tracing data structure $obs"
            cgs4drInform $taskname $message
            hdstrace obey hdstrace @"$obs" -inform "cgs4drInform $taskname %V"
          } else {
            set format [string tolower $env(CGS4_FORMAT)]
            if {$format == "ndf"} {set format "sdf"}
            set message "Tracing data structure $obs.${format}"
            cgs4drInform $taskname $message
            hdstrace obey hdstrace @"$obs.$format" -inform "cgs4drInform $taskname %V"
          }
        }
      }
    }

# Remove the dialog box
    cgs4drCursor arrow green black
    destroy .red4Dialogue
}
