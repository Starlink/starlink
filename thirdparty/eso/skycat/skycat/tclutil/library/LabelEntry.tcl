# E.S.O. - VLT project/ ESO Archive
# "@(#) $Id: LabelEntry.tcl,v 1.6 2006/05/04 12:04:33 pwd Exp $"
#
# LabelEntry.tcl - Itk widget for displaying a labeled entry
#
#  Copyright:
#     Copyright (C) 1998-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.
#
#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  01 Jun 94  Created
# Peter W. Draper 11 Aug 98  Expanded real definition to allow 1.0e6
#                            type values.
#                 26 Feb 99  Merged tcl8 changes from real tclutil version.

itk::usual LabelEntry {}

# This widget displays a label and an entry and implements convenient
# methods for accessing and modifying the label and the value.

itcl::class util::LabelEntry {
    inherit util::LabelWidget

    # constructor: create a new LabelEntry widget

    constructor {args} {
	# Tk entry widget.
	itk_component add entry {
	    entry $w_.entry
	} {
	    keep -textvariable -relief -borderwidth -show
	    rename -font -valuefont valueFont ValueFont
	}

	eval itk_initialize $args
    }


    #  Get the value in the entry

    public method get {} {
	return [$itk_component(entry) get]
    }


    # select the contents of the entry

    public method select {} {
	$itk_component(entry) select range 0 end
    }


    # called for traces on textvariable

    private method trace_ {args} {
	if {!$notrace_} {
	    command_proc_ $itk_option(-changecmd)
	}
    }


    #  called for return or keypress in entry, calls command proc with new value

    protected method command_proc_ {cmd} {
	lappend cmd [$itk_component(entry) get]
	eval $cmd
    }


    # The peek procedure returns the value of the entry with the
    # char inserted at the insert position. (ripped from iwidgets::entryfield)

    private method peek_ {char} {
	set str [get]

	set insertPos [$itk_component(entry) index insert]
	set firstPart [string range $str 0 [expr $insertPos - 1]]
	set lastPart [string range $str $insertPos end]

	append rtnVal $firstPart $char $lastPart
	return $rtnVal
    }


    #  called for keypress events when validation is on (based on code from iwidgets)

    private method validate_ {char sym} {
	set cmd $validate_cmd_

	if {"$cmd" == "" || "$itk_option(-validate)" == ""} {
	    return
	}

	# pass these on to other bindings...
	if {$sym == "Return" ||
	    $sym == "Tab" ||
	    $sym == "BackSpace" ||
	    $sym == "Delete" ||
	    $char == ""} {
	    return
	}

	regsub -all "%W" $cmd $itk_component(hull) cmd
	regsub -all "%P" $cmd [peek_ $char] cmd
	regsub -all "%S" $cmd [get] cmd

        if {$char == "\\"} {
            regsub -all "%c" $cmd {\\\\} cmd
        } elseif {$char == "&"} {
            regsub -all "%c" $cmd {\&} cmd
        } else {
            regsub "\"|\\\[|\\\]|{|}| " $char {\\\0} char
            regsub -all "%c" $cmd $char cmd
        }

	set valid [eval LabelEntry::$cmd]

	if {($valid == "") || (! $valid)} {
	    eval $itk_option(-invalid)
	    return -code break
	}
    }


    # validation methods used for -validate option (from iwidgets::entryfield class)

    protected proc numeric {char} {
	return [regexp {[0-9]} $char]
    }
    protected proc integer {string} {
	return [regexp {^[-+]?[0-9]*$} $string]
    }
    protected proc alphanumeric {char} {
	return [regexp -nocase {[0-9a-z]} $char]
    }
    protected proc alphabetic {char} {
	return [regexp -nocase {[a-z]} $char]
    }
    protected proc hexidecimal {string} {
	return [regexp {^(0x)?[0-9a-fA-F]*$} $string]
    }
    protected proc real {string} {
#	return [regexp {^\-?[0-9]*\.?[0-9]*$} $string]
        return [regexp -nocase {^[-+]?[0-9]*\.?[0-9]*([0-9]\.?e[-+]?[0-9]*)?$} $string]
    }

    # -- options --

    # set the value displayed in the entry
    itk_option define -value value Value {} {
	set prev_state [$itk_component(entry) cget -state]
	$itk_component(entry) config -state normal
	set notrace_ 1
	$itk_component(entry) delete 0 end
	$itk_component(entry) insert 0 $itk_option(-value)
	if {"$itk_option(-justify)" == "right"} {
	    $itk_component(entry) icursor end
	    $itk_component(entry) xview moveto 1
	}
	$itk_component(entry) config -state $prev_state
	set notrace_ 0
    }

    # set the width of the value displayed
    itk_option define -valuewidth valueWidth ValueWidth {15} {
	$itk_component(entry) config -width $itk_option(-valuewidth)
    }

    # set the state to normal or disabled (greyed out)
    itk_option define -state state State normal {
	$itk_component(entry) config -state $itk_option(-state)
	if {"$itk_option(-state)" == "normal"} {
	    $itk_component(entry) config -foreground $itk_option(-foreground)
	} else {
	    $itk_component(entry) config -foreground $itk_option(-disabledforeground)
	}
    }

    # the command for <Return> in the entry, called with the new value
    itk_option define -command command Command {} {
	if {"$itk_option(-command)" != ""} {
	    bind $itk_component(entry) <Return> [code $this command_proc_ $itk_option(-command)]
	}
    }

    # alternative name for use when -command is already used by a derived class
    itk_option define -entrycommand entryCommand EntryCommand {} {
	if {"$itk_option(-entrycommand)" != ""} {
	    bind $itk_component(entry) <Return> \
		[code $this LabelEntry::command_proc_ $itk_option(-entrycommand)]
	}
    }

    # set to "right" to make sure the end of the entry is visible
    itk_option define -justify justify Justify {left}

    # commands to evaluate whenever the entry value changes
    itk_option define -changecmd changecmd Changecmd {} {
	if {"$itk_option(-changecmd)" != ""} {
	    if {"$itk_option(-textvariable)" == ""} {
		config -textvariable $w_
	    }
	    set var $itk_option(-textvariable)
	    global ::$var
	    trace variable $var w [code $this trace_]
	}
    }


    # widget orientation: horizontal or vertical
    itk_option define -orient orient Orient {horizontal} {
	if {"$itk_option(-valuewidth)" == "" || $itk_option(-valuewidth) == 0} {
	    set expand 0
	} else {
	    set expand 1
	}
	pack $itk_component(entry) \
	    -side $side_ -expand $expand -fill x -padx 1m -ipadx 1m
    }


    # valiadation of entry fields (based on iwidgets entryfield class)
    # numeric, alphabetic, integer, hexidecimal, real, and alphanumeric
    itk_option define -validate validate Validate {""} {
	if {"$itk_option(-validate)" != ""} {
	    set validate_cmd_ ""
	    switch $itk_option(-validate) {
		numeric {
		    set validate_cmd_ "numeric %c"
		}
		integer {
		    set validate_cmd_ "integer %P"
		}
		hexidecimal {
		    set validate_cmd_ "hexidecimal %P"
		}
		real {
		    set validate_cmd_ "real %P"
		}
		alphabetic {
		    set validate_cmd_ "alphabetic %c"
		}
		alphanumeric {
		    set validate_cmd_ "alphanumeric %c"
		}
	    }
	    if {"$validate_cmd_" != ""} {
		bind  $itk_component(entry) <KeyPress> [code $this validate_ %A %K]
	    }
	}
    }


    # select the contents of the entry whenever it gets the focus
    itk_option define -autoselect autoSelect AutoSelect {0} {
	if {$itk_option(-autoselect)} {
	    #bind autoSelect FocusIn "$itk_component(entry) select range 0 end"
	    #bindtags $itk_component(entry) autoSelect
	} else {
	    #bind autoSelect FocusIn { }
	}
    }

    # for compat with iwidgets entryfield: action for invalid char with -validate
    itk_option define -invalid invalid Command {bell}

    # -- protected vars --

    # this flag is set to 1 to avoid tracing when changing the entry's value
    protected variable notrace_ 0

    # command to validate entry contents
    protected variable validate_cmd_ ""

}
