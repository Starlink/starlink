#! /bin/sh
# the next line restarts using tclsh7.5 \
exec tclsh7.5 "$0" "$@"

#
# tixverify.tcl
#
#	Parses some files in the Tix distribution (.tcl scripts, Makefile.in,
#	etc) and detect potential errors.
#
#

#----------------------------------------------------------------------
#			 AUX ROUTINES
#----------------------------------------------------------------------

proc Usage {} {
    global info

    puts "Usage: \[test ... --\] files ..."
    puts "available tests:"
    set maxLen 0
    foreach name [lsort [array names info]] {
	if {$maxLen < [string length $name]} {
	    set maxLen [string length $name]
	}
    }
    foreach name [lsort [array names info]] {
	puts "  [format %-$maxLen\s $name] : $info($name)"
    }
}

proc ReadFile {file} {
    set data {}
    set fd [open $file {RDONLY}]
    while {![eof $fd]} {
	append data [gets $fd]\n
    }
    close $fd
    return $data
}

# returns a list of all the procedures in the $file
#
#
proc ProcParser {file} {
    global procs
    set procs {}

    set interp [interp create]

    foreach cmd [$interp eval info commands] {
	if {$cmd == "rename"} {
	    continue
	}
	if {$cmd == "proc"} {
	    continue
	}
	if {$cmd == "unknown"} {
	    continue
	}
	if {$cmd == "source"} {
	    continue
	}
	if {$cmd == "info"} {
	    continue
	}
	if {$cmd == "set"} {
	    continue
	}
	$interp eval [list rename $cmd {}]
    }

    $interp eval rename source __source
    $interp eval rename info   __info
    $interp eval rename rename __rename
    $interp alias unknown unknown_sub
    $interp alias proc proc_sub

    proc unknown_sub {args} {
	#puts "Ignoring toplevel command $args"
    }

    proc proc_sub {name arg body} {
	global procs
	lappend procs [list $name $arg $body]
    }

    $interp eval __source $file
    interp delete $interp 

    return $procs
}

#----------------------------------------------------------------------
#		       THE TESTS
#----------------------------------------------------------------------

#----------------------------------------------------------------------
#
set info(make) "the .o targets in the Makefiles"

proc Verify:make {files} {
    # $files is not used
    #
    set list {}
    set list_s {}
    set appPWD [pwd]
    set dir [file dirname [info script]]
    cd $dir
    cd ..
    set dir [pwd]
    cd $appPWD
    puts "checking the makefiles $dir/*/Makefile.in"
    foreach file [glob $dir/*/Makefile.in] {
	set data [ReadFile $file]
	if [regexp {tixClass[.]o} $data] {
	    lappend list $file

	    foreach line [split $data \n] {
		if [regexp {tix[A-z]*[.]o} $line target] {
		    regsub _s $target "" target
		    set targets($target) 1
		}
	    }
	}
	if [regexp {tixClass_s[.]o} $data] {
	    lappend list_s $file
	}
    }

    foreach file $list {
	set data [ReadFile $file]
	set filename "$file:\n"
	foreach target [lsort [array names targets]] {
	    if {![regexp $target $data]} {
		puts -nonewline $filename
		puts \t$target
		set filename ""
	    }
	}
	if {[lsearch $list_s $file] != -1} {
	    foreach target [lsort [array names targets]] {
		if {$target == "tixAppInit.o"} {
		    continue
		}
		regsub {[.]o} $target _s.o target
		if {![regexp $target $data]} {
		    puts -nonewline $filename
		    puts \t$target
		    set filename ""
		}
	    }
	}
    }
}

#----------------------------------------------------------------------
#
set info(strcmp)  "ungarded string comparisons"

proc Verify:strcmp {files} {
    catch {
	set lines ""
	set lines [eval exec egrep -n [list {if.*[!=]=.*[^x]\$}] $files]
    }

    foreach line [split $lines \n] {
	if [regexp {[x]\$} $line] {
	    continue
	}
	puts $line
    }
}

#----------------------------------------------------------------------
#
set info(strcmp1) "improperly guarded string comparisons"

proc Verify:strcmp1 {files} {
    catch {
	set lines ""
	set lines [eval exec egrep -n [list {if.*[!=]=.*[x]\$}] $files]
    }

    foreach line [split $lines \n] {
       if ![regexp {[\"]x[^\"]*[\"][ ]*[!=]=[ ]*[\"]x[^\"]*[\"]} $line stuff] {
	    puts $line
	    continue
	}
    }
}

#----------------------------------------------------------------------
#
set info(bool) "unverified boolean options (missing tixVerifyBoolean)"

proc Verify:bool {files} {
    set boolOpts {
	-disablecallback
	-dropdown
	-editable
	-fancy
	-history
	-prunehistory
	-disablecallback
	-showhidden
	-takefocus
	-allowzero
	-radio
	-dynamicgeometry
	-ignoreinvoke
    }

    puts "checking the following options: \{$boolOpts\}"

    set rexp ""
    set prefix ""

    foreach opt $boolOpts {
	append rexp "$prefix\(\{$opt\[\ \].*\[^n\]\}\)"
	set prefix "|"
    }

    catch {
	puts [eval exec egrep -n [list $rexp] $files]
    }
}

#----------------------------------------------------------------------
#
set info(classname) "misspelled class name"

proc Verify:classname {files} {
    foreach file $files {
	set data [exec cat $file]
	if [regexp "(tixWidgetClass|tixClass)\[^\{\]*\{" $data def] {
	    regsub (tixWidgetClass|tixClass) $def "" def
	    regsub \{ $def "" def
	    set def [string trim $def]

	    set inFile "in file $file:\n"

	    foreach line [split $data \n] {
		if {[regexp "^proc.*:" $line] && ![regexp $def: $line]} {
		    puts -nonewline $inFile
		    puts "    $line"
		    set inFile ""
		}
	    }
	}
    }
}

#----------------------------------------------------------------------
#
set info(chain)  "improperly chained methods"

proc Verify:chain {files} {
    set baseClass(InitWidgetRec)   tixPrimitive
    set baseClass(ConstructWidget) tixPrimitive
    set baseClass(SetBindings)     tixPrimitive
    set baseClass(Destructor)      tixPrimitive
    set mustChain [array names baseClass]

    foreach file $files {
	foreach proc [ProcParser $file] {
	    set name [lindex $proc 0]
	    set args [lindex $proc 1]
	    set body [lindex $proc 2]

	    set class  [lindex [split $name :] 0]
	    set method [lindex [split $name :] 1]

	    foreach p $mustChain {
		if {$baseClass($p) == $class} {
		    continue
		}
		if [regexp :$p $name] {
		    if {![string match "*tixChainMethod \$w $p*" $body]} {
			puts $name
		    }
		}
	    }
	}
    }
}

#----------------------------------------------------------------------
#	Main program
#----------------------------------------------------------------------
if {$argv == {}} {
    Usage
    exit 0
}

set idx [lsearch $argv "--"]
if {$idx > 0 } {
    set tests [lrange $argv 0 [expr $idx-1]]
} else {
    set tests [lsort [array names info]]
}

if {$idx != -1} {
    set files [lrange $argv [expr $idx+1] end]
} else {
    set files $argv
}


foreach test $tests {
    if {![info exists info($test)]} {
	puts "Error: \"$test\" is not a test"
	Usage
	exit 1
    }
}

foreach test $tests {
    puts "Executing test \"$test\": Checking $info($test)"
    Verify:$test $files
    puts --------OK-----------
}
