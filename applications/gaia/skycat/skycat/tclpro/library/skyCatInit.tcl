# E.S.O. - VLT project 
# "@(#) $Id: skyCatInit.tcl,v 1.1 1998/10/30 18:20:52 abrighto Exp $"
#
# skyCatInit.tcl - one time initialization for skycat application
#
# This script creates a single Tcl source file out of all of the
# Tcl libraries for use with ET.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  14 Nov 97  Created

set tclutil_library $env(TCLUTIL_LIBRARY)
set astrotcl_library $env(ASTROTCL_LIBRARY)
set cat_library $env(CAT_LIBRARY)
set rtd_library $env(RTD_LIBRARY)
set skycat_library $env(SKYCAT_LIBRARY)
set blt_library $env(BLT_LIBRARY)

set srcdirs [list \
		 $tclutil_library \
		 $astrotcl_library \
		 $rtd_library \
		 $cat_library \
		 $skycat_library]

proc main {} {
    global argv0 argv argc srcdirs depend_list outfile tclx_library blt_library

    if {$argc != 1} {
	puts "usage: skycatInit.tcl output_file_name"
	exit 1
    }
    set outfile [lindex $argv 0]
    set fd [open $outfile w]

    # use relative pathnames to access wrapped files...
    foreach dir $srcdirs {
	if {[file exists $dir/tclIndex]} {
	    puts $fd "-relativeto [file dirname [file dirname $dir]]"
	    puts $fd $dir/tclIndex
	    puts $fd $dir/*.tcl
	}
    }

    # tclX doesn't use tclIndex files
    if {[info exists tclx_library] && [file exists $tclx_library/tcl.tlib]} {
	puts $fd "-relativeto [file dirname $tclx_library]"
	puts $fd $tclx_library/*.tcl
	puts $fd $tclx_library/tcl.tlib
	puts $fd $tclx_library/tcl.tndx
    } else {
	puts "$argv0: can't find TclX library files"
	exit 1
    }

    # BLT has some special files
    if {[info exists blt_library] && [file exists $blt_library/bltGraph.pro]} {
	puts $fd "-relativeto [file dirname $blt_library]"
	puts $fd $blt_library/tclIndex
	puts $fd $blt_library/*.tcl
	puts $fd $blt_library/*.pro
    } else {
	puts "$argv0: can't find BLT library files"
	exit 1
    }
}

main
exit

