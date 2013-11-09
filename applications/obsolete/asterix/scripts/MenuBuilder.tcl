proc ParseAmpName {name} {

# Does name contain ampersand?
  set ampos [string first & $name]

# Remove ampersand if present
  if {$ampos == -1} {
    set ename $name
  } else {
    regsub -all {\&} $name {} ename
    }

  return [list $ampos $ename]
  }


#
# Add menu command
#
proc MkMenuCmd {menu args} {

# Extract arguments
  set name [lindex $args 0]
  set cmd [lindex $args 1]

# Does name contain ampersand?
  set pdata [ParseAmpName $name]
  set ampos [lindex $pdata 0]
  set sname [lindex $pdata 1]

# Make menu button, underlining if required
  $menu add command -label $sname -command $cmd -underline $ampos

# Options present?
  set copts [lrange $args 2 end]
  if {$copts != {}} { 
    set ccmd "$menu entryconfigure last $copts"
    eval $ccmd
    }
  }

#
# Add menu separator
#
proc MkMenuSep {menu args} {
  $menu add separator
  }

#
# Make a menu
#
proc MkMenu {barname name args} {

# Does name contain ampersand?
  set pdata [ParseAmpName $name]
  set ampos [lindex $pdata 0]
  set sname [lindex $pdata 1]

# Make menu button
  set wname [string tolower $sname]
  set bname [menubutton $barname.$wname -text "$sname " \
            -menu $barname.$wname.menu -underline $ampos]

# Make the menu itself
  set menuname [menu $bname.menu]

# Loop over items in the menu stuff, executing commands and configuring 
# with menu options
  foreach arg $args {

# Get the item name
    set item [lindex $arg 0]

# Menu add command?
    if {[string range $item 0 1] == "Mk"} {
     
#   Insert menu name and execute
      set cmd [linsert $arg 1 $menuname]
      eval $cmd

# Must be menu button option
    } else {
      lappend copts $item
      }
    }

# Configure with accumulated options if they exist
  if {[info exists copts]} {
    set cmd "$bname configure $copts"
    eval $cmd
    }

  return $bname
  }


#
# Make a menu bar
#
proc MkMenuBar {w args} {

# Initial pack side
  set pside left

# Make the top level menu bar frame
  set mbar [
    frame $w -relief raised -bd 2
    ]
  set barargs [list $mbar]

# Loop over items in the optional arguments
  foreach arg $args {

# Get the item name
    set item [lindex $arg 0]

# Pack order change to right?
    if { $item == ">" } {
      set pside right

# Pack order change to left?
    } elseif { $item == "<" } {
      set pside left

# Build menu item?
    } elseif { $item == "MkMenu" } {
      set cmd [linsert $arg 1 $mbar]
      set men [eval $cmd]
      lappend barargs $men
      pack $men -side $pside

    } else {
      lappend copts $item
      }
    }

# Configure with accumulated options if they exist
  if {[info exists copts]} {
    set cmd "$bname configure $copts"
    eval $cmd
    }

# Set up menu bar
  eval "tk_menuBar $barargs"

  return $mbar
  }
