# E.S.O. - VLT project 
# "@(#) $Id: gaiaInit.tcl,v 1.1 1999/03/17 20:49:16 abrighto Exp $"
#
# gaiaInit.tcl - This script generates a scriptics prowrap config 
#                file on the stdout.
#
# who             when       what
# --------------  ---------  ----------------------------------------
# Allan Brighton  16 Mar 99  Created

set tclutil_library $env(TCLUTIL_LIBRARY)
set astrotcl_library $env(ASTROTCL_LIBRARY)
set cat_library $env(CAT_LIBRARY)
set rtd_library $env(RTD_LIBRARY)
set blt_library $env(BLT_LIBRARY)
set skycat_library $env(SKYCAT_LIBRARY)
set gaia_library $env(GAIA_LIBRARY)

set srcdirs [list \
		 $tclutil_library \
		 $astrotcl_library \
		 $rtd_library \
		 $cat_library \
		 $skycat_library \
		 $gaia_library]

proc main {} {
    global argv0 argv argc srcdirs depend_list outfile \
	tclx_library blt_library gaia_library

    if {$argc != 1} {
	puts "usage: gaiaInit.tcl output_file_name"
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
    
    # gaia also needs these
    puts $fd $gaia_library/*.hlp
    puts $fd $gaia_library/demos/tclIndex
    puts $fd $gaia_library/demos/adamtask.tcl

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

