# E.S.O. - VLT project 
# "@(#) $Id: skyCatInit.tcl,v 1.7 1998/10/28 17:44:26 abrighto Exp $"
#
# skyCatInit.tcl - one time initialization for skycat application
#
# This script creates a single Tcl source file out of all of the
# Tcl libraries for use with ET.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  14 Nov 97  Created

set itcl_library ${itcl::library}
set itk_library [file dirname $itcl_library]/itk${itcl::version}
set tclx_library 

set tclutil_library $env(TCLUTIL_LIBRARY)
set astrotcl_library $env(ASTROTCL_LIBRARY)
set cat_library $env(CAT_LIBRARY)
set rtd_library $env(RTD_LIBRARY)
set skycat_library $env(SKYCAT_LIBRARY)

set srcdirs [list \
		 $itcl_library \
		 $itk_library \
		 $tclutil_library \
		 $astrotcl_library \
		 $rtd_library \
		 $cat_library \
		 $skycat_library]

proc load_tcl_file {file} {
    global depend_list loaded outfile
    if {[info exists depend_list($file)]} {
	foreach i $depend_list($file) {
	    load_tcl_file $i
	}
    }
    if {! [info exists loaded($file)]} {
	set loaded($file) 1
	puts "load $file"
	exec cat $file >> $outfile
    }
}


proc main {} {
    global argv argc srcdirs depend_list outfile \
	tclx_library itcl_library itk_library iwidgets_library tcl_version

    if {$argc != 1} {
	puts "usage: skycatInit.tcl output_file_name"
	exit 1
    }
    set outfile [lindex $argv 0]
    set fd [open $outfile w]

    puts "loading tclIndex files..."
    foreach dir $srcdirs {
	if {[file exists $dir/tclIndex]} {
	    puts "source $dir/tclIndex"
	    source $dir/tclIndex
	}
    }

    # special case: tclX doesn't use tclIndex files!
    if {[info exists tclx_library] && [file exists $tclx_library/tcl.tlib]} {
	puts "loading TclX lib"
	if {$tcl_version >= 8.0} {
	    exec cat $tclx_library/autoload.tcl >> $outfile
	} else {
	    exec cat $tclx_library/loadouster.tcl >> $outfile
	}
	exec cat $tclx_library/tcl.tlib >> $outfile
    } else {
	error "can't find TclX library files"
    }

    # load itcl and itk init scripts
    if {[file exists $itcl_library/itcl.tcl]} {
	puts "loading itcl.tcl"
	exec cat $itcl_library/itcl.tcl >> $outfile
    } else {
	error "can't find itcl.tcl"
    }
    if {[file exists $itk_library/itk.tcl]} {
	puts "loading itk.tcl"
	exec cat $itk_library/itk.tcl >> $outfile
    } else {
	error "can't find itk.tcl"
    }

    # base classes must appear first, since we are not auto_loading
    puts "sorting Itcl classes to put base classes first..."
    foreach i [array names auto_index] {
	set file [lindex $auto_index($i) 1]
	if {! [info exists sourced($file)]} {
	    set sourced($file) 1
	    if {! [catch {set r [exec grep inherit $file]}]} {
		foreach line [split $r "\n"] {
		    if {[llength $line] == 2} {
			set c "::[lindex $line 1]"
			if {[info exists auto_index($c)]} {
			    set file2 [lindex $auto_index($c) 1]
			    if {[info exists depend_list($file)]} {
				lappend depend_list($file) $file2
			    } else {
				set depend_list($file) $file2
			    }
			}
		    }
		}
	    }
	}
    }

    puts "loading Tcl files..."
    foreach file [array names sourced] {
	load_tcl_file $file
    }
    
    # load the Tk postscript prolog at runtime
    load_tcl_file library/psprolog.tcl
}

main
exit

