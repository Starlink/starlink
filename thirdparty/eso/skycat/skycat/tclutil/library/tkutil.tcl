# tkutil.tcl - utility tcl/tk routines
#
# Copyright (C) 1994 Allan Brighton (abrighto@eso.org)
# "@(#) $Id: tkutil.tcl,v 1.1.1.1 2006/01/12 16:40:53 abrighto Exp $"
#



# return the name of the top level window for w, or "" if it is .

proc utilGetTopLevel {w} {
    set t [winfo toplevel $w]
    if {"$t" == "."} {
	return ""
    }
    return $t
}


# get the selection or return an empty string

proc utilGetSelection {} {
    set s ""
    catch {set s [selection get]} err
    return $s
}


# make the given top level window visible by raising it if it is
# hidden or deiconifying it if it is not mapped

proc utilRaiseWindow {w} {
    if {[winfo viewable $w]} {
	raise $w
    } else {
	wm deiconify $w
    }
}


# if the widget exists, make it visible and reconfigure it
# otherwise create it with the given options

proc utilReUseWidget {type w args} {
    if {[winfo exists $w]} {
	uplevel "$w config $args"
	utilRaiseWindow $w
    } else {
	uplevel "$type $w $args"
    }
}


# for debugging: print all errors on stderr 

proc utilPrintErrors {} {
    catch {bgerror}
    rename bgerror bgerror__
    proc bgerror {msg} {
	global ::errorInfo ::argv0 ::env
	if {[info exists env(TCL_DEBUG)]} {
	    catch {puts stderr "[file rootname [file tail $argv0]]: $errorInfo"}
	} else {
	    catch {puts stderr "[file rootname [file tail $argv0]]: $msg"}
	}
	bgerror__ $msg
    }
}
