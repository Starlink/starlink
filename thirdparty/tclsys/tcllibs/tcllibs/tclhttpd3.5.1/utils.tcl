# utils.tcl
#
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::utils 1.0

# Stderr - print to standard error

proc Stderr {string} {
    catch {puts stderr $string}
}

# iscommand - returns true if the command is defined  or lives in auto_index.

proc iscommand {name} {
    expr {([string length [info command $name]] > 0) || [auto_load $name]}
}

# lappendOnce - add to a list if not already there

proc lappendOnce {listName value} {
    upvar $listName list
    if ![info exists list] {
	lappend list $value
    } else {
	set ix [lsearch $list $value]
	if {$ix < 0} {
	    lappend list $value
	}
    }
}

# setmax - set the variable to the maximum of its current value
# or the value of the second argument
# return 1 if the variable's value was changed.

proc setmax {varName value} {
    upvar $varName var
    if {![info exists var] || ($value > $var)} {
	set var $value
	return 1
    } 
    return 0
}

# setmin - set the variable to the minimum of its current value
# or the value of the second argument
# return 1 if the variable's value was changed.

proc setmin {varName value} {
    upvar $varName var
    if {![info exists var] || ($value < $var)} {
	set var $value
	return 1
    } 
    return 0
}

# Incr - version of incr that handles undefined variables.

proc Incr {varName {value 1}} {
    upvar $varName var
    if {![info exists var]} {
	set var $value
    }  else {
	set var [expr $var + $value]
    }
}

# Assign a set of variables from a list of values, a la TclX.
# If there are more values than variables, they are returned.
# If there are fewer values than variables, the variables get the empty string.

proc lassign {valueList args} {
    if {[llength $args] == 0} {
	error "wrong # args: lassign list varname ?varname..?"
    }
    if {[llength $valueList] == 0} {
	foreach x $args {
	    uplevel 1 [list set $x {}]
	}
    } else {
	uplevel 1 [list foreach $args $valueList {break}]
    }
    return [lrange $valueList [llength $args] end]
}
# Assign a set of variables from a list of values.
# If there are more values than variables, they are ignored.
# If there are fewer values than variables, the variables get the empty string.

proc lassign-brent {varList value} {
    if {[string length $value] == 0} {
	foreach var $varList {
	    uplevel [list set $var {}]
	}
    } else {
	uplevel [list foreach $varList $value { break }]
    }
}

# Delete a list item by value.  Returns 1 if the item was present, else 0

proc ldelete {varList value} {
    upvar $varList list
    if ![info exist list] {
	return 0
    }
    set ix [lsearch $list $value]
    if {$ix >= 0} {
	set list [lreplace $list $ix $ix]
	return 1
    } else {
	return 0
    }
}

# Recursive make directory

if {$tcl_version < 7.6} {
proc makedir { pathname } {
    if {[file isdirectory $pathname]} {
	return [glob $pathname]	;# Handle ~
    } elseif {[file exists $pathname]} {
	error "Non-directory $pathname already exists."
    } else {
	# Recurse to create intermediate directories
	set parent [makedir [file dirname $pathname]]
	set pathname [file join $parent [file tail $pathname]]
	exec mkdir $pathname
	return $pathname
    }
}
} else {
proc makedir { pathname } {
    file mkdir $pathname
}
}

# see if an option matches a list of options
# return full option name if it does, otherwise ""
#  option:  the name of the option (or unique prefix)
#  list:    the list of valid options (e.g. -foo -bar ...)

proc matchOption {option list} {
    if {![regexp -- {-[a-zA-Z0-9:_-]+} $option]} {
    	error "Invalid option: \"$option\""
    }
    if {[regsub -all -- "$option\[^ \]*" $list {} {}] == 1} {
    	regexp -- "$option\[^ \]*" $list result
    	return $result
    } else {
    	return ""
    }
}

# Set local variables based on defaults - passed in as an array, and
# a set of name value pairs.  The "params" always override the current setting
# of the local variables; the defaults only get set if no vars exist.
#   array: name of the array (with default values)
#   params:  The "-name value" pairs to set

proc optionSet {array params} {
    upvar $array options
    set list [array names options -*]
    foreach {option value} $params {
	set realoption [matchOption $option $list]
	if {$realoption != ""} {
	    regexp -- {-(.*)} $realoption {} var
	    uplevel [list set $var $value]
	}
    }

    foreach {name value} [array get options -*] {
	regexp -- {-(.*)} $name {} var
	upvar $var set
	if {![info exists set]} {
	    uplevel [list set $var $value]
	}
    }
}

# "configure" for options in an array
#   name:  The name of the array containing the options
#   args:  The name value pairs

proc optionConfigure {name args} {
    upvar $name data

    set len [llength $args]
    set list [array names data -*]
    if {$len > 1 && ($len % 2) == 1} {
    	return -code error "optionConfigure Must have 1 or even number of arguments"
    }
    set result ""

    # return entire configuration list

    if {$len == 0} {
    	foreach option [lsort $list] {
	    lappend result $option $data($option)
	}

    # return a single configuration value

    } elseif {$len == 1} {
    	set option [matchOption $args $list]
    	if {$option == ""} {
	    return -code error "$args is an invalid option, should be one of: [join $list ", "]."
	}
	set result $data($option)

    # Set a bunch of options

    } else {
    	foreach {option value} $args {
	    set realoption [matchOption $option $list]
	    if {$realoption == ""} {
		return -code error "$option is an invalid option, should be one of: [join $list ", "]."
	    }
	    if {[info exists data(validate$realoption)]} {
	    	eval $data(validate$realoption) {data $realoption $value}
	    } else {
		set data($realoption) $value
	    }
	}
    }
    return $result
}


# print an import array 

proc poptions {array args} {
    upvar $array data
    puts "*** $array *** $args"
    foreach name [array names data] {
    	regexp -- {-(.*)} $name {} var
    	upvar $var value
    	if {[info exists value]} {
	    puts "${array}($var) = $value"
    	} else {
	    puts "${array}($var) = <unset>"
    	}
    }
}
# simple random number generator snagged from the net

set RNseed [pid]
proc randomx {} {
    global RNseed
    set RNseed [expr 30903*($RNseed&65535)+($RNseed>>16)]
    return [format %.4x [expr int(32767*($RNseed & 65535)/65535.0)]]
} 

# escape html characters (simple version)

proc protect_text {text} {
    array set Map { < lt   > gt   & amp   \" quot}
    regsub -all {[\\$]} $text {\\&} text
    regsub -all {[><&"]} $text {\&$Map(&);} text
    subst -nocommands $text
}

# File_Reset - hack to close files after a leak has sprung

proc File_Reset {} {
    for {set i 5} {$i <= 1025} {incr i} {
	if {! [catch {close file$i}]} {
	    append result file$i\n
	}
    }
    for {set i 10} {$i <= 1025} {incr i} {
	if {! [catch {close sock$i}]} {
	    append result sock$i\n
	}
    }
    Log_SetFile
    return $result
}

# File_List - report which files are open.

proc File_List {} {
    global OpenFiles
    for {set i 1} {$i <= 1025} {incr i} {
	if {! [catch {fconfigure file$i} conf]} {
	    append result "file$i $conf\n"
	    if {[info exist OpenFiles(file$i)]} {
		append result "file$i: $OpenFiles(file$i)\n"
	    }
	}
	if {! [catch {fconfigure sock$i} conf]} {
	    array set c {-peername {} -sockname {}}
	    array set c $conf
	    append result "sock$i $c(-peername) $c(-sockname)\n"
	}
    }
    return $result
}

# parray - version of parray that returns the result instead
# of printing it out.

proc parray {aname {pat *}} {
    upvar $aname a
    foreach name [array names a $pat] {
	setmax max [string length $name]
    }
    if ![info exists max] {
	return {}
    }
    incr max [string length $aname]
    incr max 2
    set result {}
    foreach name [lsort [array names a $pat]] {
	append result [list set ${aname}($name) $a($name)]
	append result \n
    }
    return $result
}

#
# Example 27-3
# Listbox with optional scrollbars.
#

proc Scrolled_Listbox { f args } {
	frame $f
	listbox $f.list \
		-xscrollcommand [list Scroll_Set $f.xscroll \
			[list grid $f.xscroll -row 1 -column 0 -sticky we]] \
		-yscrollcommand [list Scroll_Set $f.yscroll \
			[list grid $f.yscroll -row 0 -column 1 -sticky ns]]
	eval {$f.list configure} $args
	scrollbar $f.xscroll -orient horizontal \
		-command [list $f.list xview]
	scrollbar $f.yscroll -orient vertical \
		-command [list $f.list yview]
	grid $f.list $f.yscroll -sticky news
	grid $f.xscroll -sticky news
	grid rowconfigure $f 0 -weight 1
	grid columnconfigure $f 0 -weight 1
	return $f.list
}


#
# Example 27-1
# A text widget and two scrollbars.
#

proc Scrolled_Text { f args } {
	frame $f
	eval {text $f.text \
		-xscrollcommand [list $f.xscroll set] \
		-yscrollcommand [list $f.yscroll set]} $args
	scrollbar $f.xscroll -orient horizontal \
		-command [list $f.text xview]
	scrollbar $f.yscroll -orient vertical \
		-command [list $f.text yview]
	grid $f.text $f.yscroll -sticky news
	grid $f.xscroll -sticky news
	grid rowconfigure $f 0 -weight 1
	grid columnconfigure $f 0 -weight 1
	return $f.text
}

#
# Example 27-2
# Scroll_Set manages optional scrollbars.
#

proc Scroll_Set {scrollbar geoCmd offset size} {
	if {$offset != 0.0 || $size != 1.0} {
		eval $geoCmd					;# Make sure it is visible
		$scrollbar set $offset $size
	} else {
		set manager [lindex $geoCmd 0]
		$manager forget $scrollbar								;# hide it
	}
}

#
# Example 31-1
# A large scrolling canvas.
#

proc Scrolled_Canvas { c args } {
	frame $c
	eval {canvas $c.canvas \
		-xscrollcommand [list $c.xscroll set] \
		-yscrollcommand [list $c.yscroll set] \
		-highlightthickness 0 \
		-borderwidth 0} $args
	scrollbar $c.xscroll -orient horizontal \
		-command [list $c.canvas xview]
	scrollbar $c.yscroll -orient vertical \
		-command [list $c.canvas yview]
	grid $c.canvas $c.yscroll -sticky news
	grid $c.xscroll -sticky ew
	grid rowconfigure $c 0 -weight 1
	grid columnconfigure $c 0 -weight 1
	return $c.canvas
}


proc ChopLine {line {limit 72}} {
    regsub -all " *\n" $line " " line
    set new {}
    while {[string length $line] > $limit} {
	set hit 0
	for {set c $limit} {$c >= 0} {incr c -1} {
	    set char [string index $line $c]
	    if [regexp \[\ \t\n>/\] $char] {
		set hit 1
		break
	    }
	}
	if !$hit {
	    set c $limit
	}
	append new [string trimright [string range $line 0 $c]]\n
	incr c
	set line [string range $line $c end]
    }
    append new "$line"
    return $new
}

# boolean --
#
#	Convert boolean values to 0/1
#
# Arguments:
#	value	boolean value: true/false, on/off, yes/no, etc
#
# Results:
#	Returns 0 or 1.

proc boolean value {
    if {!([regsub -nocase {^(1|yes|true|on)$} $value 1 value] || \
	  [regsub -nocase {^(0|no|false|off)$} $value 0 value])} {
	error "boolean value expected"
    }
    return $value
}


# file_latest --
#
#	Return the newest file from the list.
#
# Arguments:
#	files	A list of filenames.
#
# Results:
#	None
#
# Side Effects:
#	The name of the newest file.

proc file_latest {files} {
    set newest {}
    foreach file $files {
	if {[file readable $file]} {
	    set m [file mtime $file]
	    if {![info exist mtime] || ($m > $mtime)} {
		set mtime $m
		set newest $file
	    }
	}
    }
    return $newest
}

if {([package vcompare [package provide Tcl] 7.6] < 0)
    && [string match unix $tcl_platform(platform)]} {

    # The subcommands copy, delete, rename, and mkdir were added to
    # the Tcl command 'file' in Tcl version 7.6.  The following command
    # approximates them on Unix platforms.  It may not agree with
    # the Tcl 7.6+ command 'file' in all of its functionality (notably
    # the way it reports errors).  Further refinements should be made as
    # needed.
    rename file Tcl7.5_file
    proc file {option args} {
        switch -glob -- $option {
            c* {
                if {[string first $option copy] != 0} {
                    return [uplevel [list Tcl7.5_file $option] $args]
                }
                # Translate -force into -f
                if {[string match -force [lindex $args 0]]} {
                    set args [lreplace $args 0 0 -f]
                }
                uplevel exec cp $args
            }
            de* {
                if {[string first $option delete] != 0} {
                    return [uplevel [list Tcl7.5_file $option] $args]
                }
                if {[string match -force [lindex $args 0]]} {
                    set args [lreplace $args 0 0 -f]
                }
                catch {uplevel exec rm $args}
            }
            mk* {
                if {[string first $option mkdir] != 0} {
                    return [uplevel [list Tcl7.5_file $option] $args]
                }
                uplevel exec mkdir $args
            }
            ren* {
                if {[string first $option rename] != 0} {
                    return [uplevel [list Tcl7.5_file $option] $args]
                }
                if {[string match -force [lindex $args 0]]} {
                    set args [lreplace $args 0 0 -f]
                }
                uplevel exec mv $args
            }
            default {
                uplevel [list Tcl7.5_file $option] $args
            }
        }
    }
}

if {[package vcompare [package provide Tcl] 8] < 0} {
    
    # The subcommands nativename and attributes were added to
    # the Tcl command 'file' in Tcl version 8.0.  Here is an approximation
    # for earlier Tcl versions:
    rename file Tcl7.6_file
    ;proc file {option args} {
        switch -glob -- $option {
            att* {
                if {[string first $option attributes] != 0} {
                    uplevel [list Tcl7.6_file $option] $args
                }
                return -code error "Tcl [package provide Tcl] does not support\
			\[file attributes\].\n\tUpgrade to Tcl 8.0 to use it."
            }
            n* {
                if {[string first $option nativename] != 0} {
                    uplevel [list Tcl7.6_file $option] $args
                }
                if {![llength $args]} {
                    return -code error "wrong # args: should be\
                	    \"file nativename name ?arg ...?\""
                }
                set fcomps [file split [lindex $args 0]]
                # Take care of tilde substitution
                set first [lindex $fcomps 0]
                if {[string match ~* $first]} {
                    set first [file join [file dirname $first] [file tail $first]]
                }
                set result [eval file join [list $first] [lrange $fcomps 1 end]]
                global tcl_platform
                if {[string match windows $tcl_platform(platform)]} {
                    regsub -all -- / $result \\ result
                }
                return $result
            }
            default {
                uplevel [list Tcl7.6_file $option] $args
            }
        }
    }
}

if {[package vcompare [package provide Tcl] 8.4] < 0} {
    # The subcommands nativename and attributes were added to
    # the Tcl command 'file' in Tcl version 8.0.  Here is an approximation
    # for earlier Tcl versions:
    rename file Tcl8.0_file
    ;proc file {option args} {
        switch -glob -- $option {
	    norm* {
		set sp [file split [lindex $args 0]]
		if {[file pathtype [lindex $sp 0]] == "relative"} {
		    set sp [file split [eval [list file join [pwd]] $sp]]
		}
		set np {}
		foreach ele $sp {
		    if {$ele != ".."} {
			if {$ele != "."} { lappend np $ele }
		    } elseif {[llength $np]> 1} {
			set np [lrange $np 0 [expr {[llength $np] - 2}]]
		    }
		}
		if {[llength $np] > 0} { return [eval file join $np] }
	    }
	    default {
		uplevel [list Tcl8.0_file $option] $args
	    }
	}
    }
}

# see http://mini.net/tcl/lambda
proc K {a b} {set a}
proc lambda {argl body} {K [info level 0] [proc [info level 0] $argl $body]}

# Tcllib 1.6 has inconsistencies with md5 1.4.3 and 2.0.0,
# and requiring 1.0 cures later conflicts with 2.0
# we run with whatever version is available
# by making an aliased wrapper
if {[package vcompare [package present md5] 2.0] > -1} {
    # we have md5 v2 - it needs to be told to return hex
    interp alias {} md5hex {} ::md5::md5 --hex --
} else {
    # we have md5 v1 - it returns hex anyway
    interp alias {} md5hex {} ::md5::md5
}
