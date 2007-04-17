# dialog.tcl - general purpose dialogs
#
# Copyright (C) 1994 Allan Brighton (abrighto@eso.org)
# "@(#) $Id: udialog.tcl,v 1.1.1.1 2006/01/12 16:40:53 abrighto Exp $"


# Get a file name from the user and return it or the empty string.
# The optional arguments are the directory (first time only),
# the filter value, the parent widget and an optional list of
# file types (suffixes) to display in a menu.

proc filename_dialog {{dir "."} {filter "*"} {parent ""} {types ""}} {
   set w .fs
   if {![winfo exists $w]} {
      FileSelect $w -dir $dir -filter $filter -transient 1 \
         -withdraw 1 -filter_types "$types"
   } else {
      $w config -filter $filter -filter_types "$types"
   }
   if {"$parent" != ""} {
      wm transient $w [winfo toplevel $parent]
   }
   if {[$w activate]} {
      return [$w get]
   }
}



# error message routine with exit button

proc errexit_dialog {msg {parent ""}} {
    if {"$parent" != ""} {
	if {"[set parent [winfo toplevel $parent]]" == "."} {
	    set parent ""
	}
    }
    set w $parent.errexit_dialog
    catch {destroy $w}
    set d [util::DialogWidget $w \
	       -title Error \
	       -text "Error: $msg" \
	       -bitmap error \
	       -transient 1 \
	       -buttons {Continue Exit}]
    if {[$d activate] == 1} {
	exit 1
    }
}


# error  message routine

proc error_dialog {msg {parent ""}} {
    if {"$parent" != ""} {
	if {"[set parent [winfo toplevel $parent]]" == "."} {
	    set parent ""
	}
    }
    set w $parent.error_dialog[clock clicks]

    catch {destroy $w} 
    [util::DialogWidget $w \
	 -title Error \
	 -text "Error: $msg" \
	 -transient 1 \
	 -bitmap error] activate
}


# warning message routine

proc warning_dialog {msg {parent ""}} {
    if {"$parent" != ""} {
	if {"[set parent [winfo toplevel $parent]]" == "."} {
	    set parent ""
	}
    }
    set w $parent.warning_dialog
    catch {destroy $w}
    [util::DialogWidget $w \
	 -title Warning \
	 -text "Warning: $msg" \
	 -transient 1 \
	 -bitmap warning] activate
}


# info message routine

proc info_dialog {msg {parent ""}} {
    if {"$parent" != ""} {
	if {"[set parent [winfo toplevel $parent]]" == "."} {
	    set parent ""
	}
    }
    set w $parent.info_dialog
    catch {destroy $w}
    [util::DialogWidget $w \
	 -title Information \
	 -text "$msg" \
	 -transient 1 \
	 -bitmap info] activate
}


# Ask the user to do something or press cancel.
# If the answer is OK, return 1, otherwise 0

proc action_dialog {msg {parent ""}} {
    if {"$parent" != ""} {
	if {"[set parent [winfo toplevel $parent]]" == "."} {
	    set parent ""
	}
    }
    set w $parent.action_dialog
    catch {destroy $w}
    set d [util::DialogWidget $w \
	       -title Action \
	       -text $msg \
	       -bitmap info \
	       -transient 1 \
	       -default 0 \
	       -buttons {OK Cancel}]
    return [expr {[$d activate] == 0}]
}


# Ask the user to make a choice from a list of items and return the item chosen.
# $msg is the message to display, buttons is a list of choices to display as buttons,
# default_button is highlighted as the default, parent is the parent window (toplevel).

proc choice_dialog {msg {buttons "OK Cancel"} {default_button "OK"} {parent ""}} {
    if {"$parent" != ""} {
	if {"[set parent [winfo toplevel $parent]]" == "."} {
	    set parent ""
	}
    }
    set w $parent.choice_dialog
    catch {destroy $w}
    
    set n -1
    set def 0
    foreach button $buttons {
	set idx([incr n]) $button
	if {"$button" == "$default_button"} {
	    set def $n
	}
    }

    set d [util::DialogWidget $w \
	       -title Choice \
	       -text $msg \
	       -bitmap info \
	       -transient 1 \
	       -default $def \
	       -messagewidth 4i \
	       -buttons $buttons]
    return $idx([$d activate])
}



# Question the user and get the answer
# If the answer is yes, return 1, otherwise 0

proc confirm_dialog {msg {parent ""}} {
    if {"$parent" != ""} {
	if {"[set parent [winfo toplevel $parent]]" == "."} {
	    set parent ""
	}
    }
    set w $parent.confirm_dialog
    catch {destroy $w}
    set d [util::DialogWidget $w \
	       -title Confirm \
	       -text $msg \
	       -bitmap questhead \
	       -transient 1 \
	       -default 0 \
	       -buttons {Yes Cancel}]
    return [expr {[$d activate] == 0}]
}


# Get some text input from the user and return it
# or an empty string (in case the user cancels the operation)

proc input_dialog {msg {parent ""}} {
    if {"$parent" != ""} {
	if {"[set parent [winfo toplevel $parent]]" == "."} {
	    set parent ""
	}
    }
    set w $parent.input_dialog
    catch {destroy $w}
    set d [InputDialog $w \
	       -title Input \
	       -text $msg \
	       -bitmap questhead \
	       -transient 1 \
	       -messagewidth 4i \
	       -default 0 \
	       -buttons {OK Cancel}]
    return [$d activate]
}


# Get a username and passwd from the user and return a list
# {username passwd} or an empty string (in case the user cancels 
# the operation)

proc passwd_dialog {msg {parent ""}} {
    if {"$parent" != ""} {
	if {"[set parent [winfo toplevel $parent]]" == "."} {
	    set parent ""
	}
    }
    set w $parent.input_dialog
    catch {destroy $w}
    set d [PasswdDialog $w \
	       -title Input \
	       -text $msg \
	       -bitmap questhead \
	       -transient 1 \
	       -messagewidth 4i \
	       -default 0 \
	       -buttons {OK Cancel}]
    return [$d activate]
}

