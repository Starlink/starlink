#-----------------------------------------------------------------------------
# tclx.tcl -- Extended Tcl initialization.
#-----------------------------------------------------------------------------
# $Id: tclx.tcl,v 8.3 1999/05/03 17:30:36 markd Exp $
#-----------------------------------------------------------------------------

if {[info exists tclx_library] && ![cequal $tclx_library {}]} {
    set auto_index(buildpackageindex) {source [file join $tclx_library buildidx.tcl]}
    if {![info exists auto_path] || ![lcontain $auto_path $tclx_library]} {
	lappend auto_path $tclx_library
    }
}
# == Put any code you want all Tcl programs to include here. ==

if !$tcl_interactive return

# == Interactive Tcl session initialization ==

# Replace standard Tcl prompt with out prompt if its the TclX shell

if ![info exists tcl_prompt1] {
    set tcl_prompt1 {global argv0; puts -nonewline stdout [file tail $argv0]>}
}
if ![info exists tcl_prompt2] {
    set tcl_prompt2 {puts -nonewline stdout =>}
}


