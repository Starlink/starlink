proc cgs4drHelp {file} {

# Destroy previous dialogue box
  if {[winfo exists .helpDialogue]} {destroy .helpDialogue}

# Create the dialogue box
  if {[file exists ${file}]==1} {
    #set frame [dialogStart .helpDialogue "Help on [file rootname [file tail [string trim ${file}]]]" info Dismiss]
    set title "Help on [file rootname [file tail [string trim ${file}]]]"
    cgs4drCursor pirate orange black
    #.helpDialogue config -cursor {pencil blue yellow}

# Fill it with text from file
    set i 0
    set text ""
    set fid [open [string trim $file] r]
    while {[gets $fid line] >= 0} {
      #set label [label $frame.l$i -text "  [string trim ${line}]  " -fg blue]
      #pack $label -side top -anchor w
      set text "${text}\n${line}"
      incr i
    }

# File does not exist, so return no info message
  } else {
    #set frame [dialogStart .helpDialogue "Help Dialogue Box" error Dismiss]
    set title "Help Dialogue Box"
    cgs4drCursor pirate orange black
    #.helpDialogue config -cursor {arrow blue yellow}
    #set label [label $frame.l1 -text "No information available!" -fg red]
    #pack $label -side top -anchor w
    set text "No information available!"
  }

# Show and destroy it
  #dialogShow .helpDialogue .helpDialogue
  cgs4drDialog .helpDialogue "${title}" "${text}" info -1 Dismiss
  if {[winfo exists .helpDialogue]} {destroy .helpDialogue}

# If not invoked by another dialogue box revert to green arrow cursor
  if {[winfo exists .cgs4drDialogue]} {
    focus .cgs4drDialogue
  } elseif {[winfo exists .cred4Dialogue]} {
    focus .cred4Dialogue
  } elseif {[winfo exists .red4Dialogue]} {
    focus .red4Dialogue
  } elseif {[winfo exists .p4Dialogue]} {
    focus .p4Dialogue
  } elseif {[winfo exists .qmanDialogue]} {
    focus .qmanDialogue
  } else {
    cgs4drCursor arrow green black
  }
}
