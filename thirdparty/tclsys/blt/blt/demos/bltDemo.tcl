# ----------------------------------------------------------------------------
#
#	The following code is solely a convenience so that you can test the 
#	BLT distribution without first installing it.
#
# ----------------------------------------------------------------------------

# If we're in the ./demos directory, we can simply specify
# "../library" as the library directory without having to install the
# files.

if { [file exists ../library/bltGraph.tcl] } {
    global blt_library
    set blt_library ../library
}

global auto_path
lappend auto_path $blt_library

# Add a binding for convenience to let you exit with pressing the
# "quit" button.

wm protocol . WM_DELETE_WINDOW { DoExit 0 }
bind all <Control-KeyPress-c> { DoExit 0 } 
bind all <KeyPress-q> { DoExit 0 }
focus .

proc DoExit { code } {
    destroy .
#    exit $code
}

if { [info commands "namespace"] == "namespace" } {
    if { $tcl_version >= 8.0 } {
	namespace import blt::*
    } else {
	catch { import add blt }
    }
    if { $tcl_version >= 8.0 } {
	namespace import -force blt::tile::*
    } else {
	import add blt::tile
    }
} else {
    foreach cmd { button checkbutton radiobutton frame label 
	scrollbar toplevel menubutton listbox } {
	if { [info command tile${cmd}] == "tile${cmd}" } {
	    rename ${cmd} ""
	    rename tile${cmd} ${cmd}
	}
    }
}

