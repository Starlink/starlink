#
# Script to run base Tcl tests.  This skips certain tests that we can't run
# and issues notices about expected discrepancies.  While Tcl is now at Sun,
# this is called ucbtests.tcl, well, just because we always have.
# 
# Requires a single argument of the path to the Tcl source directory.
#

if {$argc != 1} {
    puts stderr "Wrong # of args: $argv0 tclsrcdir"
    exit 1
}
set tclTestDir [lindex $argv 0]/tests

if ![file isdirectory $tclTestDir] {
    puts stderr "Directory \"$tclTestDir\" does not exist"
    exit 1
}
cd $tclTestDir

#
# Table of tests that should be skipped.
#
set testsToSkip(env.test) "can't be run with other Tcl applications"
set testsToSkip(namespace-old.test) \
        "test produces errors due to environment assumptions"


foreach i [lsort [glob *.test]] {
    if [info exists testsToSkip($i)] {
        puts "$i skipped: $testsToSkip($i)"
        continue
    }

    puts $i
    if [catch {source $i} msg] {
        puts [replicate "=" 75]
        puts "Uncaught error in $i: $msg"
        puts $errorInfo
        puts [replicate "=" 75]
    }
}

if !$tcl_interactive {
    exit
}


