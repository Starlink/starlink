/*
 * E.S.O. - VLT project 
 * "@(#) $Id: tcl_findLibrary.h,v 1.2 2006/01/21 01:06:54 abrighto Exp $"
 *
 * Replacement for Tcl's tcl_findLibrary script (This version is taken 
 * from tcl-8.4.11/auto.tcl).
 * In earlier Tcl versions, this script did not look in the auto_path
 * for extensions.
 * 
 * who             when       what
 * --------------  --------   ----------------------------------------
 * Allan Brighton  11/01/06   created
 */
static const char* const rcsId="@(#) $Id: tcl_findLibrary.h,v 1.2 2006/01/21 01:06:54 abrighto Exp $";

char* tcl_findLibrary = 
"proc tcl_findLibrary {basename version patch initScript enVarName varName} {\n"
"    upvar #0 $varName the_library\n"
"    global env errorInfo\n"
"    set dirs {}\n"
"    set errors {}\n"
"    set variableSet [info exists the_library]\n"
"    if {$variableSet && $the_library != \"\"} {\n"
"	lappend dirs $the_library\n"
"    } else {\n"
"        if {[info exists env($enVarName)]} {\n"
"            lappend dirs $env($enVarName)\n"
"        }\n"
"	foreach d $::auto_path {\n"
"	    lappend dirs [file join $d $basename$version]\n"
"	    if {$::tcl_platform(platform) == \"unix\"\n"
"		&& $::tcl_platform(os) ==\"Darwin\"} {\n"
"		lappend dirs [file join $d $basename$version Resources Scripts]\n"
"	    }\n"
"	}\n"
"        set parentDir [file dirname [file dirname [info nameofexecutable]]]\n"
"        set grandParentDir [file dirname $parentDir]\n"
"        lappend dirs [file join $parentDir lib $basename$version]\n"
"        lappend dirs [file join $grandParentDir lib $basename$version]\n"
"        lappend dirs [file join $parentDir library]\n"
"	if {1} {\n"
"	    lappend dirs [file join $grandParentDir library]\n"
"	    lappend dirs [file join $grandParentDir $basename$patch library]\n"
"	    lappend dirs [file join [file dirname $grandParentDir] \\n"
"			      $basename$patch library]\n"
"	}\n"
"    }\n"
"    array set seen {}\n"
"    foreach i $dirs {\n"
"	if {1 || [interp issafe]} {\n"
"	    set norm $i\n"
"	} else {\n"
"	    set norm [file normalize $i]\n"
"	}\n"
"	if {[info exists seen($norm)]} { continue }\n"
"	set seen($norm) \"\"\n"
"	lappend uniqdirs $i\n"
"    }\n"
"    set dirs $uniqdirs\n"
"    foreach i $dirs {\n"
"        set the_library $i\n"
"        set file [file join $i $initScript]\n"
"        if {[interp issafe] || [file exists $file]} {\n"
"            if {![catch {uplevel #0 [list source $file]} msg]} {\n"
"                return\n"
"            } else {\n"
"                append errors \"$file: $msg\n$errorInfo\n\"\n"
"            }\n"
"        }\n"
"    }\n"
"    if {!$variableSet} {\n"
"	unset the_library\n"
"    }\n"
"    set msg \"Can't find a usable $initScript in the following directories: \n\"\n"
"    append msg \"    $dirs\n\n\"\n"
"    append msg \"$errors\n\n\"\n"
"    append msg \"This probably means that $basename wasn't installed properly.\n\"\n"
"    error $msg\n"
"}\n";
