#!/bin/sh
# The next line is not visible to Tcl/Tk \
exec wish $0
#option add *font -adobe-helvetica-medium-r-normal--10-100-75-75-p-56-iso8859-1
option add *highlightThickness 0

# Find the name of the configuration file
#
set ConfigFile [glob ~]/.browser.config

#
# Functions to tell about this program
#
proc AboutMessage {w text} {
  label $w.title -text {The Tk File Browser} \
    -font -adobe-times-bold-i-normal--24-240-75-75-p-128-iso8859-1
  pack $w.title -side top -pady 10
  message $w.msg -width 10c -text "
By D. Richard Hipp
Hipp, Wyrick & Company, Inc.
6200 Maple Cove Lane
Charlotte, NC 28269
704.948.4565
drh@vnet.net" \
    -font -adobe-times-medium-r-normal--14-140-75-75-p-74-iso8859-1
  pack $w.msg -padx 15 -pady 15
}
proc HelpAboutThisProgram {} {
  ModalDialogBox . {About} AboutMessage {} {{Dismiss {}}}
}

# Construct the menu bar
#
set title "[exec hostname] - browser"
wm title . $title
wm iconname . $title
wm minsize . 240 240
wm protocol . WM_DELETE_WINDOW {destroy .}
frame .mbar -bd 2 -relief raised
pack .mbar -side top -fill x
foreach i {File Actions Applications Places Help} {
  set name [string toupper [string range $i 0 0]][string range $i 1 end]
  menubutton .mbar.x$i -text $name -underline 0 -menu .mbar.x$i.menu \
    -font -adobe-helvetica-bold-r-normal-*-12-120-75-75-p-70-iso8859-1 \
    -pady 0
  pack .mbar.x$i -side left -padx 6
}
.mbar.xActions config -underline 1
eval tk_menuBar .mbar [winfo children .mbar]
pack .mbar -side top -fill x
menu .mbar.xFile.menu
.mbar.xFile.menu add command -label {New Browser} -underline 0 \
  -command {exec $cmd_dir/$cmd_name &}
.mbar.xFile.menu add command -label {Remember this Place} -command {
  if {![info exists place($DirName)]} {
    set place($DirName) 1
    UpdateConfiguration
    WriteConfigurationFile
  }
} -underline 0
.mbar.xFile.menu add command -label {Forget this Place} -command {
  if {[info exists place($DirName)]} {
    unset place($DirName)
    UpdateConfiguration
    WriteConfigurationFile
  }
} -underline 0
.mbar.xFile.menu add command -label {Reread Configuration File} -command {
  ReadConfigurationFile
  UpdateConfiguration
} -underline 7
.mbar.xFile.menu add separator
.mbar.xFile.menu add command -label Exit -command {destroy .} -underline 1
menu .mbar.xHelp.menu
.mbar.xHelp.menu add command -label {About this program...} \
  -command HelpAboutThisProgram

# Construct the speed button bar
#
frame .sbar -bd 1 -relief raised
pack .sbar -side top -fill x -expand 1
foreach i {Edit Delete Shell Back Forward} {
  button .sbar.b$i -text $i -command "eval \$action($i)" \
    -font -adobe-helvetica-bold-r-normal-*-10-100-75-75-p-60-iso8859-1 \
    -pady 0 -padx 3 -underline 0
  pack .sbar.b$i -side left -pady 2 -padx 2
}
.sbar.bBack config -state disabled
.sbar.bForward config -state disabled -underline 3


# Construct the status bar at the bottom of the browser
#
label .stat -font fixed -width 54
pack .stat -side bottom -anchor w -padx 5 
label .name -relief flat -bd 1 -font fixed -width 54 -anchor e
pack .name -side bottom -anchor w -padx 5
bind .name <1> {
  .name config -relief raised -bg skyblue1
  selection own .
  selection handle . ReturnName
}
proc ReturnName {offset max} {
  set name [lindex [.name config -text] 4]
  return [string range $name $offset [expr $offset+$max]]
}

# Construct the listboxes used to hold the list of directories
# and files
#
frame .dir -bd 2 -relief groove
pack .dir -side left -fill both -expand 1 -padx 5 -pady 5
label .dir.l -text Directories
pack .dir.l -side top 
frame .dir.f
pack .dir.f -side bottom -fill both -expand 1 -padx 5 -pady 5
listbox .dir.f.lb -yscrollcommand {.dir.f.sb set} -bd 2 -relief sunken \
  -exportselection 0 -width 15 -height 25 \
  -font -adobe-times-medium-r-normal-*-12-120-75-75-p-64-iso8859-1
pack .dir.f.lb -side left -fill both -expand 1
scrollbar .dir.f.sb -command {.dir.f.lb yview} -orient vertical
pack .dir.f.sb -side left -fill y 
frame .file -bd 2 -relief groove
pack .file -side left -fill both -expand 1 -padx 5 -pady 5
label .file.l -text Files
pack .file.l -side top
frame .file.f
pack .file.f -side bottom -fill both -expand 1 -padx 5 -pady 5
listbox .file.f.lb -yscrollcommand {.file.f.sb set} -bd 2 -relief sunken \
  -exportselection 0 -width 15 -height 25 \
  -font -adobe-times-medium-r-normal-*-12-120-75-75-p-64-iso8859-1
pack .file.f.lb -side left -fill both -expand 1
scrollbar .file.f.sb -command {.file.f.lb yview} -orient vertical
pack .file.f.sb -side left -fill y 

# The routine re-reads the configuration information.
#
proc ReadConfigurationFile {} {
  global action place application DirName

  # Put the default actions into the action database
  #
  catch {unset action place application}
  set action(Delete) DeleteFile
  set action(Edit) EditFile
  set action(Print) PrintFile
  set action(Run) ExecuteFile
  set action(Back) {JumpBack}
  set action(Forward) {JumpForward}
  set action(Shell) {exec tkterm &}
  set application(Shell) {exec tkterm &}
  set place($DirName) 1

  # Scan the the user database configuration file for new, user-defined
  # actions.
  #
  global ConfigFile
  catch {source $ConfigFile}
}

# Use the information in the configuration arrays to reconstruct
# the associated menus.
#
proc UpdateConfiguration {} {
  global action application place

  # Construct the action menu
  #
  if {[winfo exists .mbar.xActions.menu]} {
    destroy .mbar.xActions.menu
  }
  menu .mbar.xActions.menu
  foreach btn [lsort [array names action]] {
    .mbar.xActions.menu add command -label $btn \
      -command "eval \$action($btn)"
  }

  # Construct the application menu
  #
  if {[winfo exists .mbar.xApplications.menu]} {
    destroy .mbar.xApplications.menu
  }
  menu .mbar.xApplications.menu
  foreach btn [lsort [array names application]] {
    .mbar.xApplications.menu add command -label $btn \
      -command "eval $application($btn)"
  }

  # Construct the places menu
  #
  if {[winfo exists .mbar.xPlaces.menu]} {
    destroy .mbar.xPlaces.menu
  }
  menu .mbar.xPlaces.menu
  foreach i [lsort [array names place]] {
    .mbar.xPlaces.menu add command -label $i -command "JumpTo $i"
  }
}

# Write configuration information back to the configuration file
#
proc WriteConfigurationFile {} {
  global action application place
  set out {}
  foreach i [array names action] {
    append out "set [list action($i)] [list $action($i)]\n"
  }
  foreach i [array names application] {
    append out "set [list application($i)] [list $application($i)]\n"
  }
  foreach i [array names place] {
    append out "set [list place($i)] 1\n"
  }
  global ConfigFile
  catch {exec echo $out >$ConfigFile}
}
  

set DirName [pwd]
set DirStack {}
set DirForwardStack {}
set FileName .
set FileSel {}
set DirSel {}

#
# Rework the bindings on the list boxes
#
bindtags .dir.f.lb {.dir.f.lb .}
bindtags .file.f.lb {.file.f.lb .}
bind .dir.f.lb <1> {
  global FileName FileSel DirSel
  .name config -relief flat -bg [.stat cget -bg]
  selection clear .
  set i [.dir.f.lb nearest %y]
  .dir.f.lb select clear 0 end
  .dir.f.lb select set $i
  .file.f.lb select clear 0 end
  set FileName [.dir.f.lb get $i]
  set FileSel {}
  set DirSel $i
  SetNameAndStat
}
bind .dir.f.lb <Double-1> ChangeDirectory
bind .file.f.lb <1> {
  global FileName FileSel DirSel
  .name config -relief flat -bg [.stat cget -bg]
  selection clear .
  set i [.file.f.lb nearest %y]
  .file.f.lb select clear 0 end
  .file.f.lb select set $i
  .dir.f.lb select clear 0 end
  set FileName [.file.f.lb get $i]
  set FileSel $i
  set DirSel {}
  SetNameAndStat
}
bind .file.f.lb <Double-1> EditFile
bind . <Return> {
  if {"$DirSel"!=""} {
     ChangeDirectory
  } elseif {"$FileSel"!=""} {
     EditFile
  }
}
bind . <Up> {MoveUpDown -1}
bind . <Down> {MoveUpDown 1}
bind . <Prior> {MoveUpDown -25}
bind . <Next> {MoveUpDown 25}
bind . <Home> {MoveUpDown -1000}
bind . <End> {MoveUpDown 1000}
proc MoveUpDown {cnt} {
  global DirSel FileSel
  if {"$DirSel"!=""} {
     incr DirSel $cnt
     if {$DirSel<0} {
       set DirSel 0
     } elseif {$DirSel>=[.dir.f.lb index end]} {
       set DirSel [expr [.dir.f.lb index end]-1]
     }
     UpdateDirSelect
  } elseif {"$FileSel"!=""} {
     incr FileSel $cnt
     if {$FileSel<0} {
       set FileSel 0
     } elseif {$FileSel>=[.file.f.lb index end]} {
       set FileSel [expr [.file.f.lb index end]-1]
     }
     UpdateFileSelect
  }
}
bind . <Left> {
  if {"$FileSel"!=""} {
    set DirSel $FileSel
    set FileSel {}
    if {$DirSel>=[.dir.f.lb index end]} {
      set DirSel [expr [.dir.f.lb index end]-1]
    }
    UpdateDirSelect
  }
}
bind . <Right> {
  if {"$DirSel"!=""} {
    set FileSel $DirSel
    set DirSel {}
    if {$FileSel>=[.file.f.lb index end]} {
      set FileSel [expr [.file.f.lb index end]-1]
    }
    UpdateFileSelect
  }
}
proc UpdateDirSelect {} {
  selection clear .
  global DirSel FileName
  .file.f.lb select clear 0 end
  .dir.f.lb select clear 0 end
  .dir.f.lb select set $DirSel
  set FileName [.dir.f.lb get $DirSel]
  .dir.f.lb see $DirSel
  SetNameAndStat
}
proc UpdateFileSelect {} {
  selection clear .
  global FileSel FileName
  .dir.f.lb select clear 0 end
  .file.f.lb select clear 0 end
  .file.f.lb select set $FileSel
  .file.f.lb see $FileSel
  set FileName [.file.f.lb get $FileSel]
  SetNameAndStat
}
#bind . <KeyPress> {tkTraverseToMenu %W %A}

# Get the name of the current working directory
#
proc CurrentDirectoryName {} {
  global DirName
  set name $DirName
  while {![file isdirectory $name]} {
    set name [file dirname $name]
  }
  return $name
}

# Push the current directory onto the backup stack.
#
proc JumpTo {newdir} {
  global DirStack DirForwardStack DirName
  lappend DirStack $DirName
  if {[llength $DirStack]>10} {
    set DirStack [lrange $DirStack 1 end]
  }
  set DirForwardStack {}
  .sbar.bBack config -state normal
  .sbar.bForward config -state disabled
  SaveListboxState
  set DirName $newdir
  FillBoxes
  RestoreListboxState
}

# Pop a directory off of the backup stack
#
proc JumpBack {} {
  global DirStack DirForwardStack DirName
  set newdir [lindex $DirStack end]
  set n [llength $DirStack]
  incr n -2
  if {$n>=0} {
    set DirStack [lrange $DirStack 0 $n]
  } else {
    set DirStack {}
    .sbar.bBack config -state disabled
  }
  lappend DirForwardStack $DirName
  .sbar.bForward config -state normal
  SaveListboxState
  set DirName $newdir
  FillBoxes
  RestoreListboxState
}

# Pop an element off of the foward stack
#
proc JumpForward {} {
  global DirStack DirForwardStack DirName
  set newdir [lindex $DirForwardStack end]
  set n [llength $DirForwardStack]
  incr n -2
  if {$n>=0} {
    set DirForwardStack [lrange $DirForwardStack 0 $n]
  } else {
    set DirForwardStack {}
    .sbar.bForward config -state disabled
  }
  lappend DirStack $DirName
  .sbar.bBack config -state normal
  SaveListboxState
  set DirName $newdir
  FillBoxes
  RestoreListboxState
}


# Change directories to the directory specified by FileName
#
proc ChangeDirectory {} {
  global FileName DirName DirStack DirForwardStack
  .name config -relief flat -bg [.stat cget -bg]
  selection clear .
  lappend DirStack $DirName
  if {[llength $DirStack]>10} {
    set DirStack [lrange $DirStack 1 end]
  }
  set DirForwardStack {}
  .sbar.bBack config -state normal
  .sbar.bForward config -state disabled
  SaveListboxState
  if {"$FileName"==".."} {
    set DirName [file dirname [CurrentDirectoryName]]
  } elseif {"$FileName"!="."} {
    set dir [CurrentDirectoryName]
    if {"$dir"=="/"} {
      set DirName /$FileName
    } else {
      set DirName $dir/$FileName
    }
  }
  FillBoxes
  RestoreListboxState
}

# Fill the directory and file name list boxes with appropriate information
# based on the current working directory specified by the FileName variable.
#
set NameField [expr [llength [exec /bin/ls -ld .] ]-1]
proc FillBoxes {} {
  global FileName FileDB NameField MTime DirSel FileSel
  set dir [CurrentDirectoryName]
  .dir.f.lb delete 0 end
  .file.f.lb delete 0 end
  cd $dir
  catch {unset FileDB}
  set MTime(dir) [file mtime .]
  set MTime(interval) 500
  foreach line [split [exec /bin/ls -la | /usr/bin/sort -f +$NameField] \n] {
     if {[llength $line]<5} continue
     set file [lindex $line $NameField]
     set FileDB($file) $line
     switch -- [string index $line 0] {
        "-"  {.file.f.lb insert end $file}
        "d"  {.dir.f.lb insert end $file}
        "l"  {
               if [file isdirectory $file] {
                 .dir.f.lb insert end $file
               } else {
                 .file.f.lb insert end $file
               }
             }
     }
  }
  set FileName .
  .dir.f.lb select clear 0 end
  .dir.f.lb select set 0
  set DirSel 0
  set FileSel {}
  SetNameAndStat
}

# Check to see if the directory or the status line for the
# selected file needs to be updated.  The update interval begins
# at every 500 milli-seconds, but backs off exponentially to 4 seconds
# if the directory and file are unchanged.
#
proc CheckMTime {} {
  global MTime FileName
  if {![file exists .] || [file mtime .]>$MTime(dir)} {
    SaveListboxState
    FillBoxes
    RestoreListboxState
  } elseif {[file mtime $FileName]>$MTime(file)} {
    SetNameAndStat
  } elseif {$MTime(interval)<4000} {
    set MTime(interval) [expr int($MTime(interval)*2)]
  }
  after $MTime(interval) CheckMTime
}
set MTime(interval) 500
set MTime(dir) 0
set MTime(file) 0

#
# Fill in the Name and Status lines using information about the object
# named "$DirName/$FileName"
#
proc SetNameAndStat {} {
  global DirName FileName FileDB NameField MTime
  set name $DirName/$FileName
  set FileDB($FileName) [exec /bin/ls -ld $FileName]
  set MTime(file) [file mtime $FileName]
  set MTime(interval) 500
  if [info exists FileDB($FileName)] {
    .stat config -text [lrange $FileDB($FileName) 0 [expr $NameField-1]]
  } else {
    .stat config -text {}
  }
  .name config -text $name
}

# Save the current position of the listboxes in the global variable
# ListboxState so that the position can be restored later (after
# a call to FillBoxes.)
#
proc SaveListboxState {} {
  global ListboxState DirName
  set ListboxState($DirName) [list \
    [.dir.f.lb curselection] \
    [.file.f.lb curselection] \
    [.dir.f.lb nearest 0] \
    [.file.f.lb nearest 0]]
}

# Restore the state of the list boxes
#
proc RestoreListboxState {} {
  global ListboxState FileName DirName DirSel FileSel
  if {![info exists ListboxState($DirName)]} return
  set dir_sel [lindex $ListboxState($DirName) 0]
  if {"$dir_sel"==""} {
    set file_sel [lindex $ListboxState($DirName) 1]
    if {[.file.f.lb index end]<$file_sel} {
      .file.f.lb select clear 0 end
      .dir.f.lb select clear 0 end
      .dir.f.lb select set 0
      set FileName .
    } else {
      .file.f.lb select clear 0 end
      .file.f.lb select set $file_sel
      .dir.f.lb select clear 0 end
      set FileName [.file.f.lb get $file_sel]
    }
  } else {
    if {[.dir.f.lb index end]<$dir_sel} {
      .file.f.lb select clear 0 end
      .dir.f.lb select clear 0 end
      .dir.f.lb select set 0
      set FileName .
    } else {
      .file.f.lb select clear 0 end
      .dir.f.lb select clear 0 end
      .dir.f.lb select set $dir_sel
      set FileName [.dir.f.lb get $dir_sel]
    }
  }
  .dir.f.lb yview [lindex $ListboxState($DirName) 2]
  .file.f.lb yview [lindex $ListboxState($DirName) 3]
  set DirSel [.dir.f.lb curselection]
  set FileSel [.file.f.lb curselection]
  SetNameAndStat
}

#
# The procedure defined below implements a generic dialog box.  The
# arguments are as follows:
#
#   position      The new dialog box is centered over the window given
#                 by this argument
#
#   title         This is the title for the dialog box
#
#   build         This procedure is called to construct the top-most
#                 panel of the dialog box.  The first argument to the
#                 procedure is the name of the frame widget which
#                 is the top panel.  Subsequent arguments are given by
#                 the "buildargs" parameter.
#
#   buildargs     This is arguments to the "build" command which come
#                 after the name of the top panel widget.
#
#   btns          This is a list of button descriptions.  Each button
#                 description consists of the name of the button and
#                 some text to be displayed beside that button.
#
# The procedure builds a model dialog box and waits for a button to be
# pressed.  When a button is pressed, the dialog box goes away and the
# procedure returns an integer which is the index of the selected button.
# The first button is numbered 0.
#
proc ModalDialogBox {pos title build buildargs btns} {
  global dialog_button
  if [winfo exists .d] {destroy .d}
  toplevel .d -class Dialog
  wm title .d $title
  wm iconname .d Dialog
  frame .d.msg -relief raised -bd 1
  $build .d.msg $buildargs
  pack .d.msg -side top -fill both -expand 1
  set cnt -1
  foreach btn $btns {
    incr cnt
    set btnname [lindex $btn 0]
    set btntext [lindex $btn 1]
    frame .d.x$cnt -relief raised -bd 1
    if [llength $btn]==3 {
      set cmd "[lindex $btn 2] .d; set dialog_button $cnt"
    } else {
      set cmd "set dialog_button $cnt"
    }
    button .d.x$cnt.btn -text $btnname -command $cmd -width 9 -underline 0
    bindtags .d.x$cnt.btn Button
    pack .d.x$cnt.btn -side left -padx 5 -pady 5
    message .d.x$cnt.msg -text $btntext -width 10c
    pack .d.x$cnt.msg -anchor w -padx 5 -pady 5
    pack .d.x$cnt -side top -fill x
  }
  wm withdraw .d
  update idletasks
  set x [expr [winfo rootx $pos] + ([winfo width $pos]-[winfo reqwidth .d])/2]
  set y [expr [winfo rooty $pos] + ([winfo height $pos]-[winfo reqheight .d])/2]
  wm geometry .d +$x+$y
  wm deiconify .d
  set old_focus [focus]
  focus .d
  grab set .d
  bind .d <1> {
    wm withdraw .d
    wm deiconify .d
    bell
  }
  bind .d <Any-KeyPress> {tkTraverseToMenu %W %A}
  tkwait variable dialog_button
  grab release .d
  focus $old_focus
  destroy .d
  return $dialog_button
}

#
# The following procedures are used to construct a dialog box header which
# contains an icon and a message.  The difference is in the icon.
#
proc QuestionMessage {w text} {
  label $w.icon -bitmap info
  pack $w.icon -side left -padx 15 -pady 15
  message $w.msg -text $text -width 10c
  pack $w.msg -padx 15 -pady 15 -anchor w
}
proc ErrorMessage {w text} {
  label $w.icon -bitmap error
  pack $w.icon -side left -padx 15 -pady 15
  message $w.msg -text $text -width 10c
  pack $w.msg -padx 15 -pady 15 -anchor w
}

#
# Prompt the user about whether or not to delete the object $FileName
#
proc DeleteFile {} {
  global DirName FileName
  if {"$FileName"=="." || "$FileName"==".."} {
    ErrorBox "Can't delete directory \"$FileName\"!"
    return
  }
  set result [ModalDialogBox . {Delete?} QuestionMessage \
    "Are you sure you want to delete \"$FileName\"" [format {
      {Yes  {Delete %s}}
      {No   {Don't delete %s}}
    } $FileName $FileName]
  ]
  if $result==0 {
    exec rm -rf $FileName
    SaveListboxState
    FillBoxes
    RestoreListboxState
  }
}

# Invoke the text editor on the selected file
#
proc EditFile {} {
  global DirName FileName
  if {![file isdirectory $DirName/$FileName]} {
    exec tkedit $DirName/$FileName &
  } else {
    ErrorBox {Can't edit a directory!}
  }
}

# Print the selected file
#
proc PrintFile {} {
  global DirName FileName
  if {![file isdirectory $DirName/$FileName]} {
    exec lpr $DirName/$FileName &
  } else {
    ErrorBox {Can't print a directory!}
  }
}

# Attempt to run the selected file
#
proc ExecuteFile {} {
  global DirName FileName
  set f $DirName/$FileName
  if {![file isdirectory $f] && [file executable $f]} {
    exec $DirName/$FileName &
  } else {
    ErrorBox "Can't execute $f"
  }
}

proc ErrorBox text {
  ModalDialogBox . {Error} ErrorMessage $text {{Dismiss {}}}
}
# tkMenuFind --
# This procedure searches the entire window hierarchy under w for
# a menubutton that isn't disabled and whose underlined character
# is "char".  It returns the name of that window, if found, or an
# empty string if no matching window was found.  If "char" is an
# empty string then the procedure returns the name of the first
# menubutton found that isn't disabled.
#
# If a third argument is provided, it is used as a classname pattern
# for the window to search for.  Be default, this pattern is
# MenuButton, meaning that this routine will find only menubuttons.
# But if you change the class pattern to "*utton", the routine will
# find the first button of any type.
#
# Arguments:
# w -				Name of window where key was typed.
# char -			Underlined character to search for;
#				may be either upper or lower case, and
#				will match either upper or lower case.

proc tkMenuFind {w char {pattern Menubutton}} {
    global tkPriv
    set char [string tolower $char]
    set action [format {
        %s {
            set char2 [string index [$child cget -text] \
		[$child cget -underline]]
	    if {([string compare $char [string tolower $char2]] == 0)
		|| ($char == "")} {
  	        if {[$child cget -state] != "disabled"} {
		    return $child
		}
	    }
	}

	default {
	    set match [tkMenuFind $child $char %s]
	    if {$match != ""} {
		return $match
	    }
	}
    } $pattern $pattern]

    foreach child [winfo child $w] {
	switch -glob [winfo class $child] $action
    }
    return {}
}

# tkTraverseToMenu --
# This procedure implements keyboard traversal of menus.  Given an
# ASCII character "char", it looks for a menubutton with that character
# underlined.  If one is found, it posts the menubutton's menu.
#
# The routine will also look for buttons to invoke.  If a button is
# found that contains the given character, then that button is invoked.tkp
#
# Arguments:
# w -				Window in which the key was typed (selects
#				a toplevel window).
# char -			Character that selects a menu.  The case
#				is ignored.  If an empty string, nothing
#				happens.

proc tkTraverseToMenu {w char} {
    if ![winfo exists $w] return
    global tkPriv
    if {$char == ""} {
	return
    }
    while {[winfo class $w] == "Menu"} {
	if {$tkPriv(postedMb) == ""} {
	    return
	}
	set w [winfo parent $w]
    }
    set w [tkMenuFind [winfo toplevel $w] $char *utton]
    if {$w != ""} {
        switch [winfo class $w] {
            Menubutton {
   	        tkMbPost $w
	        tkMenuFirstEntry [$w cget -menu]
            }

            Button {
                tkButtonInvoke $w
            }

            Checkbutton {
                tkCheckRadioInvoke $w
            }

            Radiobutton {
                tkCheckRadioInvoke $w
            }
        }
    }
}

ReadConfigurationFile
UpdateConfiguration
FillBoxes
SetNameAndStat
CheckMTime
