#
# loadouster.tcl --
#
# Procedure to load a Ousterhout index when its encountered.
#------------------------------------------------------------------------------
# Copyright 1992-1997 Karl Lehenbauer and Mark Diekhans.
#
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted, provided
# that the above copyright notice appear in all copies.  Karl Lehenbauer and
# Mark Diekhans make no representations about the suitability of this
# software for any purpose.  It is provided "as is" without express or
# implied warranty.
#------------------------------------------------------------------------------
# $Id: loadouster.tcl,v 8.4 1997/08/23 18:55:23 markd Exp $
#------------------------------------------------------------------------------
#

#
# Body of code taken from init.tcl auto_load proc.  Indentation maintained.
#

proc auto_load_ouster_index dir {
    global auto_index errorInfo errorCode

    # Check if we are a safe interpreter. In that case, we support only
    # newer format tclIndex files.

    set issafe [interp issafe]

	set f ""
	if {$issafe} {
	    catch {source [file join $dir tclIndex]}
	} elseif [catch {set f [open [file join $dir tclIndex]]}] {
	    return
	} else {
	    set error [catch {
		set id [gets $f]
		if {$id == "# Tcl autoload index file, version 2.0"} {
		    eval [read $f]
		} elseif {$id == \
		    "# Tcl autoload index file: each line identifies a Tcl"} {
		    while {[gets $f line] >= 0} {
			if {([string index $line 0] == "#")
				|| ([llength $line] != 2)} {
			    continue
			}
			set name [lindex $line 0]
			set auto_index($name) \
			    "source [file join $dir [lindex $line 1]]"
		    }
		} else {
		    error \
		      "[file join $dir tclIndex] isn't a proper Tcl index file"
		}
	    } msg]
	    if {$f != ""} {
		close $f
	    }
	    if $error {
		error $msg $errorInfo $errorCode
	    }
	}
}
