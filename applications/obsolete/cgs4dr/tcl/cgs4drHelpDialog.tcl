proc cgs4drHelpDialog {w file} {

# Destroy old window, create new one and change cursor
    catch {destroy $w}
    toplevel $w -class Dialog
    $w config -cursor {pencil blue yellow}

# Create top and bottom frame and pack them
    frame $w.top -relief raised -bd 1
    pack $w.top -side top -fill both
    frame $w.bot -relief raised -bd 1
    pack $w.bot -side bottom -fill both

    frame $w.top.r -relief flat -width 50
    pack $w.top.r -in $w.top -side right -fill both
    frame $w.top.l -relief flat -width 10
    pack $w.top.l -in $w.top -side left -fill both

# Fill in title, text and bitmap
    set title "Help Dialogue Box"
    if {[file exists ${file}]==1} {
      set title "Help on [file rootname [file tail [string trim ${file}]]]"
      set i 0
      set j 0
      set k 0
      set fid [open [string trim $file] r]
      while {[gets $fid line] >= 0} {

#      Include text only after "<-- START" line
        if {[string compare "<!-- START" [string trim ${line}]]==0} {
          set k 1
          while {[gets $fid line]>=0 && $j==0} {

#      Include text only before "<-- END" line
            if {[string compare "<!-- END" [string trim ${line}]]==0} {
              incr j
            } else {

#            Get text and remove HTML commands
              regsub -nocase "<dir>" ${line} "" line
              regsub -nocase "</dir>" ${line} "" line
              regsub -nocase "<strong>" ${line} "" line
              regsub -nocase "</strong>" ${line} "" line
              regsub -nocase "<dd>" ${line} "" line
              regsub -nocase "<dt>" ${line} "" line
              if {[string first ": " ${line}]==-1} {
                label $w.top.r.msg$i -text "${line}" -fg blue
              } else {
                label $w.top.r.msg$i -text "${line}" -fg red
              }
              pack $w.top.r.msg$i -in $w.top.r -side top -anchor w
              incr i
            }
          }
          break
        }
      }
      close $fid
      if {$k==0} {
        label $w.top.r.msga -text " "
        pack $w.top.r.msga -in $w.top.r -side top -anchor w
        label $w.top.r.msgb -text "Unable to locate help text!" -fg blue
        pack $w.top.r.msgb -in $w.top.r -side top -anchor w
        label $w.top.r.msgc -text "Have you tried the WWW?" -fg red
        pack $w.top.r.msgc -in $w.top.r -side top -anchor w
        label $w.top.r.msgd -text " "
        pack $w.top.r.msgd -in $w.top.r -side top -anchor w
      }
    } else {
      label $w.top.r.msga -text " "
      pack $w.top.r.msga -in $w.top.r -side top -anchor w
      label $w.top.r.msgb -text "Unable to locate help file!" -fg blue
      pack $w.top.r.msgb -in $w.top.r -side top -anchor w
      label $w.top.r.msgc -text "Have you tried the WWW?" -fg red
      pack $w.top.r.msgc -in $w.top.r -side top -anchor w
      label $w.top.r.msgd -text " "
      pack $w.top.r.msgd -in $w.top.r -side top -anchor w
    }
    label $w.bitmap -bitmap info -relief ridge
    pack $w.bitmap -in $w.top.l -side left -ipadx 3m -ipady 3m -padx 3m -pady 3m

# Put some wm defaults up
    wm title $w $title
    wm iconname $w Dialog
    wm protocol $w WM_DELETE_WINDOW { }
    wm transient $w [winfo toplevel [winfo parent $w]]

# Create the dismiss button
    button $w.button -text Dismiss -command "set buttonval 0"
    pack $w.button -in $w.bot -side left -expand 1 -pady 3m

# Withdraw the window, then update all the geometry information
    wm withdraw $w
    update idletasks
    set x [expr [winfo screenwidth $w]/2 - [winfo reqwidth $w]/2 \
	    - [winfo vrootx [winfo parent $w]]]
    set y [expr [winfo screenheight $w]/2 - [winfo reqheight $w]/2 \
	    - [winfo vrooty [winfo parent $w]]]
    wm geom $w +$x+$y
    wm deiconify $w

# Set a grab and claim the focus too
    set oldFocus [focus]
    set oldGrab [grab current $w]
    if {$oldGrab != ""} {set grabStatus [grab status $oldGrab]}
    grab $w
    focus $w

# Wait for the user to respond, then restore the focus
    tkwait variable buttonval
    catch {focus $oldFocus}
    destroy $w
    if {$oldGrab != ""} {
	if {$grabStatus == "global"} {
	    grab -global $oldGrab
	} else {
	    grab $oldGrab
	}
    }
}
