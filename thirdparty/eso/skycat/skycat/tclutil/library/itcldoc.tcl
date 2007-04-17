#! /bin/sh
# -*-tcl-*-
# The next line is executed by /bin/sh, but not Tcl \
exec tclsh $0 ${1+"$@"}

# E.S.O. - VLT project
#
# "@(#) $Id: itcldoc.tcl,v 1.1.1.1 2006/01/12 16:40:37 abrighto Exp $" 
#
# itcldoc.tcl - script to extract man pages from itcl class source files.
#
# Usage: itcldoc ?filenames?
#
# This script assumes a certain style of coding, but does not require
# any special hints otherwise. It assumes comments precede each declaration
# in comment blocks (such as this one). The script splits an itcl file into
# a Tcl list. It first filters the comments to make them list elements.
# It then looks for declaration keywords in the list: class, method, proc,
# itk_option, public, protected,... and sorts and documents them based on
# the comments in the code preceding the declarations.
#
# In addition, each source file should have a comment header, such as
# this one, containing a line in the format:
#
# filename.tcl - short description
#
# It is assumed that each file given defines one itcl class.
#
# This script is only used for itcl files defining an itcl class.  By
# convention itcl classes start with an upper case letter and the file
# names for files that contain classes usually have the same name with
# a ".tcl" extension.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 07 Jan 98   created

package require Itcl
package require Tclx

itcl::class ItclDoc {

    # constructor 
    constructor {args} {
    }

    
    # start processing the command line args

    public method start {args} {
	foreach arg $args {
	    foreach filename [glob -nocomplain $arg] {
		if {[catch {
		    doc_file $filename
		} msg]} {
		    puts "$filename: $msg"
		}
	    }
	}
    }

    # filter the file to turn comments into list items
    # i.e,: turn "# comment" into "{comment}".

    protected method filter_comments {in_fd fd} {
	set in_comment 0
	set comment {}
	while {[gets $in_fd line] != -1} {
	    set line [string trimleft $line]
	    if {"[string index $line 0]" == "#"} {
		set s "[string range $line 2 end]"
		if {$in_comment} {
		    append comment "\n$s"
		} else {
		    incr in_comment
		    set comment $s
		}
	    } else {
		if {$in_comment} {
		    set comment [list "#" $comment]
		}
		puts $fd $comment
		set in_comment 0
		set comment {}
		puts $fd $line
	    }
	}
    }


    # return a list of {namespace name} for a given name by splitting
    # a string x::y into {x y}.

    protected method split_name {name} {
	set s [split $name :]
	if {[llength $s] == 3} {
	    lassign $s namespace {} name
	} else {
	    set namespace ""
	}
	return [list $namespace $name]
    }


    # return a line of text, formatted and indented with the given indent
    # char, filled to the given line length and, if is_sentence is true,
    # make sure the first char is capitalized and the text ends with a
    # period.

    protected method fmt {line  {indent ""} {length 72} {is_sentence 1}} {
	if {$is_sentence} {
	    set line "[string toupper [string index $line 0]][string range $line 1 end]"
	    if {"[string index $line [expr [string length $line]-1]]" != "."} {
		append line "."
	    }
	}

	set line [exec fmt -w $length << $line]

	if {"$indent" != ""} {
	    set tmp $line
	    set line {}
	    foreach i [split $tmp "\n"] {
		append line "$indent$i\n"
	    }
	}

	return $line
    }


    # fix newlines in arg lists and format

    protected method fix_args {name arglist} {
	regsub -all "\[\t\n \]+" $arglist { } result
	set len [expr 70-[string length $name]]
	return [string trim [fmt $result "            " $len 0]]
    }


    # check the body of an itk_component declaration to document

    protected method doc_component {body} {
	# make array of tokens from body
	set n 0
	foreach tok $body {
	    set ar($n) $tok
	    incr n
	}

	set comment {}
	set i 0
	while {$i < $n} {
	    switch -exact -- "$ar($i)" {
		"#" {
		    set comment $ar([incr i])
		}
		"keep" {
		    set opt $ar([incr i])
		    while {$i < $n && ("[string index $opt 0]" == "-" || "$opt" == " ")} {
			if {"$opt" == " "} {
			    set opt $ar([incr i])
			    continue
			}
			if {! [info exists component_options_ar_($opt)]} {
			    set component_options_ar_($opt) 1
			    lappend component_options_ $opt
			}
			catch {set opt $ar([incr i])}
		    }
		    incr i -1
		}
		"rename" {
		    incr i
		    lappend component_options_  $ar([incr i])
		    incr i 2
		}
	    }
	    incr i
	}
    }


    # check the body of a method, constructor, option, for itk components
    # to document

    protected method doc_body {body} {
	if {! [regsub -- "itk_component add" $body {} {}]} {
	    return
	}
	
	# make array of tokens from body
	set n 0
	foreach tok $body {
	    set ar($n) $tok
	    incr n
	}

	set comment {}
	set i 0
	while {$i < $n} {
	    switch -exact -- "$ar($i)" {
		"#" {
		    set comment $ar([incr i])
		}
		"itk_component" {
		    if {"$ar([incr i])" == "add"} {
			set name $ar([incr i])
			if {! [info exists components_ar_($name)]} {
			    lappend components_ [list $name $comment]
			}
		    }
		    incr i
		    set comment {}
		    if {[info exists ar([expr $i+1])]} {
			switch -exact -- "[lindex $ar([expr $i+1]) 0]" {
			    "keep" -
			    "rename" -
			    "usual" -
			    "ignore" {
				doc_component $ar([incr i])
			    }
			}
		    }
		}
		default {
		    # may have to check subblocks
		    if {[regsub -- "itk_component add" $ar($i) {} {}]} {
			doc_body $ar($i)
		    }
		}
	    }
	    incr i
	}
    }


    # generate man page info for the given class, given the following
    # args:
    #
    # desc: A short description string extracted from the file's header,
    #       from a line in the format: "# name.tcl - desc..."
    #
    # fd: the file to write to/
    #
    # comment: the class comment
    #
    # name: the class name
    #
    # body: the body of the itcl class

    protected method doc_class {desc fd comment name body} {
	set components_ {}
	set component_options_ {}
	catch {unset component_options_ar_}

	lassign [split_name $name] namespace name
	set description $comment

	# make array of tokens from class body
	set n 0
	foreach tok $body {
	    set ar($n) $tok
	    incr n
	}
	set ar($n) {}

	set comment {}
	set public_methods {}
	set protected_methods {}
	set procs {}
	set options {}
	set protected_variables {}
	set commons {}
	set parents {}
	set i 0
	while {$i < $n} {
	    switch -exact -- "$ar($i)" {
		"#" {
		    set comment $ar([incr i])
		}
		"method" {
		    lappend public_methods [list $ar([incr i]) $ar([incr i]) $comment]
		    incr i
		    set comment {}
		}
		"proc" {
		    lappend procs [list $ar([incr i]) $ar([incr i]) $comment]
		    incr i
		    set comment {}
		}
		"constructor" {
		    incr i
		    set body $ar([incr i])
		    set j [expr $i+1]
		    if {[info exists ar($j)] && "$ar($j)" != "#"} {
			set body $ar([incr i])
		    }
		    if {[catch {doc_body $body} msg]} {
			puts "warning: $msg"
		    }
		    set comment {}
		}
		"destructor" {
		    incr i
		    set comment {}
		}
		"inherit" {
		    set parents $ar([incr i])
		    # XXX handle more than one parent?
		    set comment {}
		}
		"itk_option" {
		    incr i
		    lappend options [list $ar([incr i]) $comment]
		    incr i 3
		    if {$i+1 < $n} {
			set body $ar([expr $i+1])
			if {[catch {doc_body $body} msg]} {
			    puts "warning: $msg"
			}
		    }
		    set comment {}
		}
		"public" {
		    switch -exact -- $ar([incr i]) {
			"variable" {
			    lappend options [list -$ar([incr i]) $comment]
			    if {[info exists ar([expr $i+1])] && "$ar([expr $i+1])" != "#"} {
				incr i
			    }
			    set comment {}
			}
			"method" {
			    lappend public_methods [list $ar([incr i]) $ar([incr i]) $comment]
			    if {[catch {doc_body $ar([incr i])} msg]} {
				puts "warning: $msg"
			    }
			    set comment {}
			}
			"common" {
			    lappend commons [list $ar([incr i]) $comment]
			    set comment {}
			}
		    }
		}
		"protected" {
		    switch -exact -- $ar([incr i]) {
			"variable" {
			    lappend protected_variables [list $ar([incr i]) $comment]
			    if {[info exists ar([expr $i+1])] && "$ar([expr $i+1])" != "#"} {
				incr i
			    }
			    set comment {}
			}
			"method" {
			    lappend protected_methods [list $ar([incr i]) $ar([incr i]) $comment]
			    if {[catch {doc_body $ar([incr i])} msg]} {
				puts "warning: $msg"
			    }
			    set comment {}
			}
			"common" {
			    lappend commons [list $ar([incr i]) $comment]
			    set comment {}
			}
		    }
		}
		"private" {
		    switch -exact -- $ar([incr i]) {
			"variable" {
			    incr i 2
			    set comment {}
			}
			"method" {
			    incr i 2
			    if {[catch {doc_body $ar([incr i])} msg]} {
				puts "warning: $msg"
			    }
			    set comment {}
			}
			"common" {
			    incr i
			    set comment {}
			}
		    }
		}
		"common" {
		    lappend commons [list $ar([incr i]) $comment]
		    set comment {}
		}
	    }
	    incr i
	}

	# call a method that can be overridden in a subclass to print the man page
	output $fd $name $desc $namespace $description $parents  \
	    $components_ $component_options_ $public_methods \
	    $protected_methods $procs $options $protected_variables \
	    $commons

    }

    
    # This method takes the lists of Itcl class information and outputs the
    # documentation
    #
    # Args:
    #
    #  fd - output file descriptor
    #
    #  name - class name  
    #
    #  desc - short description
    #
    #  namespace - namespace
    #
    #  description - class description
    #
    #  parents - list of parent classes
    #
    #  components - list of itk components {{component desc} ...}
    #
    #  component_options - list of itk component options
    #
    #  public_methods - list of public methods {{method args desc} ...}
    #
    #  protected_methods - list of protected methods {{method args desc} ...}
    #
    #  procs - list of class procs {{proc args desc} ...}
    #
    #  options - list of class options (itk or public) {{option desc} ...}
    #
    #  protected_variables - list of protected vars {{var desc} ...}
    #
    #  commons - list of common class vars {{var desc} ...}

    protected method output {fd name desc namespace description parents 
			     components component_options public_methods 
			     protected_methods procs options 
			     protected_variables commons} {

	if {"$desc" != ""} {
	    puts $fd "NAME\n $name - $desc\n"
	} else {
	    puts $fd "NAME\n $name - \n"
	}

	if {"$namespace" != ""} {
	    puts $fd "NAMESPACE\n $namespace\n"
	}

	set synopsis "SYNOPSIS\n $name <path> ?options?\n"
	set description "DESCRIPTION\n[fmt $description " "]\n"

	if {[llength $parents]} {
	    puts $fd "PARENT CLASS"
	    foreach i $parents {
		puts $fd " $i"
	    }
	    puts $fd ""
	}

	puts $fd $synopsis
	puts $fd $description

	if {[llength $components]} {
	    puts $fd "ITK COMPONENTS"
	    foreach i [lsort $components] {
		lassign $i name desc
		if {"$desc" != ""} {
		    puts $fd " $name"
		    puts $fd [fmt $desc "\t" 67]
		}
	    }
	}
	
	if {[llength $component_options]} {
	    puts $fd "STANDARD OPTIONS"
	    if {"$component_options" != ""} {
		puts $fd [fmt [lsort $component_options] " " 67 0]
	    }
	}
	
	if {[llength $options]} {
	    puts $fd "WIDGET OPTIONS"
	    foreach i [lsort $options] {
		lassign $i name desc
		if {"$desc" != ""} {
		    puts $fd " $name"
		    puts $fd [fmt $desc "\t" 67]
		}
	    }
	}
	
	if {[llength $public_methods]} {
	    puts $fd "PUBLIC METHODS"
	    foreach i [lsort $public_methods] {
		lassign $i name args desc
		if {"$desc" != ""} {
		    set args [fix_args $name $args]
		    puts $fd " $name {$args}"
		    puts $fd [fmt $desc "\t" 67]
		}
	    }
	}
	
	if {[llength $protected_methods]} {
	    puts $fd "PROTECTED METHODS"
	    foreach i [lsort $protected_methods] {
		lassign $i name args desc
		if {"$desc" != ""} {
		    set args [fix_args $name $args]
		    puts $fd " $name {$args}"
		    puts $fd [fmt $desc "\t" 67]
		}
	    }
	}
	
	if {[llength $procs]} {
	    puts $fd "PROCS"
	    foreach i [lsort $procs] {
		lassign $i name args desc
		if {"$desc" != ""} {
		    set args [fix_args $name $args]
		    puts $fd " $name {$args}"
		    puts $fd [fmt $desc "\t" 67]
		}
	    }
	}

	if {[llength $protected_variables]} {
	    puts $fd "PROTECTED VARIABLES"
	    foreach i [lsort $protected_variables] {
		lassign $i name desc
		if {"$desc" != ""} {
		    puts $fd " $name"
		    puts $fd [fmt $desc "\t" 67]
		}
	    }
	}

	if {[llength $commons]} {
	    puts $fd "COMMON CLASS VARIABLES"
	    foreach i [lsort $commons] {
		lassign $i name desc
		if {"$desc" != ""} {
		    puts $fd " $name"
		    puts $fd [fmt $desc "\t" 67]
		}
	    }
	}

	if {[llength $parents]} {
	    puts $fd "SEE ALSO"
	    foreach name $parents {
		lassign [split_name $name] namespace name
		puts $fd " ${name}\(n)"
	    }
	    puts $fd ""
	}
    }


    # generate a man page for the given itcl source file

    protected method doc_file {filename} {
	set filedir [file dirname $filename]
	set mandir "$filedir/../man"
	set basename [file rootname [file tail $filename]]
	set name $basename.mann

	if {! [file isdirectory $mandir]} {
	    set mandir $filedir
	} 
	set outfile "$mandir/$name"
	
	if {[file exists $outfile] && ([file mtime $outfile] >= [file mtime $filename])} {
	    puts "$outfile is up to date"
	    return
	}
	puts "making $outfile..."

	set fd [open $outfile w]
	set in_fd [open $filename]
	
	filter_comments $in_fd $fd
	
	close $fd
	close $in_fd
	set n 0
	# make array of tokens from file
	foreach tok [read_file $outfile] {
	    set ar($n) $tok
	    incr n
	}

	set fd [open $outfile w]

	puts $fd "# This file was automatically generated by itcldoc"
	puts $fd "# from source code comments. Do not edit by hand!\n"

	puts $fd "# This file may be processed by the ESO/VLT docDoManPages"
	puts $fd "#  command to produce a man page in nroff, TeX and MIF formats."
	puts $fd "# See docDoManPages(1) for a description of the input format.\n"

	set comment {}
	set desc {}
	set i 0
	set found 0
	while {$i < $n} {
	    switch -exact -- "$ar($i)" {
		"#" {
		    # look for a short description on a line such as:
		    # file.tcl - description...
		    set comment $ar([incr i])
		    if {"$desc" == ""} {
			foreach line [split $comment "\n"] {
			    if {[regsub "$basename.tcl - (.*)"  $line {\1 \2} s]} {
				set desc $s
				break
			    }
			}
		    }
		}
		"itcl::class" -
		"class" {
		    incr found
		    set name $ar([incr i])
		    set body $ar([incr i])
		    doc_class $desc $fd $comment $name $body
		}
	    }
	    incr i
	}
	if {! $found} {
	    puts "no classes found"
	}

	puts $fd "----------------------------------------------------------------------"
	close $fd
    }

    # -- protected variables --

    # list of itk components
    protected variable components_ {}

    # array of itk component names (to avoid duplicates)
    protected variable components_ar_

    # list of itk component options
    protected variable component_options_ {}

    # array of itk component options (to avoid duplicates)
    protected variable component_options_ar_
}


# -- main --

set itcldoc_usage "Usage: itcldoc ?filenames?"

# helps subclassing...
if {! [info exists mainclass]} {
    set mainclass ItclDoc
}
$mainclass main
eval "main start $argv"

exit
