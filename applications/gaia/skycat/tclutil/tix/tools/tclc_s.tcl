# tclc.tcl --
#
#	This Tcl script translates Tcl commands to one C string.
#
# Usage:
#	[tclsh] tclc.tcl file1.tcl [file2.tcl ...]
#
#	The output is printed in the standard output.

# ParseFile --
#
#	Read in a file and insert line number information into the code.
#
proc ParseFile {fileName n} {
    set fd [open $fileName {RDONLY}]
    set lineNum 1

    puts "static char script_$n\[\] = \{"

    set N [format \n]
    set T [format \t]
    set NTS [format "\n\t\ "]
    set sep ""
    while {![eof $fd]} {
	set line [gets $fd]

	# Check whether this line is backslash-ended. If so, merge this line
	# with the next one
	#
	regsub -all $N $line " " foo
	append foo \na
	set foo [subst -nocommands -novariables $foo]
	if [regexp $N $foo] {
	    set cmd "$line\n"
	} else {
	    regsub -all \\\\\[$NTS\]*$ $line " " line
	    set cmd "$line"
	}
	set cmd '[join [split $cmd ""] ',']'
	regsub -all \\\\ $cmd \\\\\\\\ cmd
	regsub -all $N $cmd \\n cmd
	regsub -all $T $cmd \\t cmd
	regsub -all ''' $cmd '\\'' cmd
	regsub -all '\"' $cmd '\\\"' cmd

        puts -nonewline $sep$cmd
	set sep ,\n
    }
    puts "$sep'\\0'\};"
    close $fd
}

proc tclc_Main {} {
    global argv argv0

    set files [lrange $argv 0 end]

    set n 0
    foreach fileName $argv {
	ParseFile $fileName $n
	incr n
    }

    puts "static int LoadScripts(interp)"
    puts "    Tcl_Interp * interp;"
    puts "\{"

    if {$n > 0} {
	puts "    char *scripts\[$n\];"
	puts "    int i;"

	for {set k 0} {$k < $n} {incr k} {
	    puts "    scripts\[$k\] = script_$k;"
	}

	puts "    for (i=0; i<$n; i++) \{"
	puts "        if (Tcl_Eval(interp, scripts\[i\]) != TCL_OK) \{"
	puts "            return TCL_ERROR;"
	puts "        \}"
	puts "    \}"
    }

    puts "    return TCL_OK;"
    puts "\}"
}
