proc red4Cgs4List {taskname} {
#+
# Creates a dialog box for red4 action
#-
    global env
    global Red4Widgets

# Check to see if ukirtfig is loaded
   if {[info commands ukirtfig*] == ""} {
     set message "Loading ukirtfig applications monolith"
     cgs4drInform $taskname $message
     adamtask ukirtfig $env(CGS4DR_ROOT)/ukirtfig
   }

# Create dialog box
   if {[winfo exists .red4Dialogue]} {destroy .red4Dialogue}
   set frame [dialogStart .red4Dialogue "Red4 List CGS4 Headers" 0 OK Cancel]
   cgs4drCursor pirate orange black
   .red4Dialogue config -cursor {arrow green black}

# Create and pack dialog box widgets
   set Red4Widgets(LH_LABEL) [label $frame.lb -text "Directory"]
   set Red4Widgets(LH_ENTRY) [entry $frame.en -width 40]
   pack $Red4Widgets(LH_LABEL) $Red4Widgets(LH_ENTRY) -in $frame -side left
   $Red4Widgets(LH_ENTRY) insert end $Red4Widgets(CDIR)

# Bind the defaults buttons
   bind $Red4Widgets(LH_LABEL) <Button-2> "red4Update red4Cgs4List LH_ENTRY"
   bind $Red4Widgets(LH_ENTRY) <Button-2> "red4Update red4Cgs4List LH_ENTRY"
   bind $Red4Widgets(LH_ENTRY) <Double-Button-2> "$Red4Widgets(LH_ENTRY) delete 0 end"

# Cancel the dialog box
   set bv [dialogShow .red4Dialogue .red4Dialogue]
   if {$bv==1} {
     destroy .red4Dialogue

# Show the dialog box
   } elseif {$bv==0} {
     cgs4drCursor watch red white
     set Red4Widgets(CDIR) [string trim [$Red4Widgets(LH_ENTRY) get]]
     destroy .red4Dialogue

#   Now loop and do the cgs4list
     foreach file [lsort [glob $Red4Widgets(CDIR)/o*.sdf $Red4Widgets(CDIR)/o*.dst]] {
       set message "cgs4list: listing ${file}"
       cgs4drInform $taskname $message
       ukirtfig obey cgs4list "filename='${file}' listing='temp.out' full=Y" \
         -inform "cgs4drInform $taskname %V" -endmsg {set ldone 1}
       tkwait variable ldone
     }

#   Now sort the output file
     cgs4drClear $taskname
     set status [catch {exec $env(CGS4DR_ROOT)/datasort}]
     if {$status==0} {
       set status [catch {exec /usr/bin/mv ukirt_data.out $env(CGS4_INDEX)/cgs4list_$env(CGS4_DATE).lis}]
       if {$status==0} {
         exec /usr/bin/rm -f temp.out

#       Write output to textpane
         set fid [open $env(CGS4_INDEX)/cgs4list_$env(CGS4_DATE).lis r]
         while {[gets ${fid} line] >= 0} {cgs4drInform $taskname $line}
       } else {
         set message "Error sorting data ... check temp.out"
         cgs4drInform $taskname $message
       }
     }
   }
   cgs4drCursor arrow green black
}
