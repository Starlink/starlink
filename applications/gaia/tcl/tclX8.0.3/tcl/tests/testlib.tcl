#
# testlib.tcl --
#
# Test support routines.  Some of these are based on routines provided with
# standard Tcl.
#------------------------------------------------------------------------------
# Set the global variable or environment variable TEST_ERROR_INFO to display
# errorInfo when a test fails.
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
# $Id: testlib.tcl,v 8.3 1997/08/23 18:55:59 markd Exp $
#------------------------------------------------------------------------------
#

# Save the unknown command in a variable SAVED_UNKNOWN.  To get it back, eval
# that variable.

global SAVED_UNKNOWN TCL_PROGRAM env TEST_ERROR_INFO tcl_platform testXConfig
global TEST_VERBOSE

if [info exists env(TEST_ERROR_INFO)] {
    set TEST_ERROR_INFO 1
}

if [info exists env(TEST_VERBOSE)] {
    set TEST_VERBOSE 1
}

# Check configuration information that will determine which tests
# to run.  To do this, create an array testXConfig.  Each element
# has a 0 or 1 value, and the following elements are defined:
#	unixOnly -	1 means this is a UNIX platform, so it's OK
#			to run tests that only work under UNIX.
#	pcOnly -	1 means this is a PC platform, so it's OK to
#			run tests that only work on PCs.
#	unixOrPc -	1 means this is a UNIX or PC platform.
#	tempNotPc -	The inverse of pcOnly.  This flag is used to
#			temporarily disable a test.

catch {unset testXConfig}
if {$tcl_platform(platform) == "unix"} {
    set testXConfig(unixOnly) 1
    set testXConfig(tempNotPc) 1
} else {
    set testXConfig(unixOnly) 0
} 
if {$tcl_platform(platform) == "windows"} {
    set testXConfig(pcOnly) 1
} else {
    set testXConfig(pcOnly) 0
}
set testXConfig(unixOrPc) [expr $testXConfig(unixOnly) || $testXConfig(pcOnly)]

#
# Save path to Tcl program to exec, use it when running children in the
# tests.  Order of checking:
#   o Environment variable TCL_PROGRAM
#   o If $argv0 can be found, use it.  Generate an absolute path.
#   o Use a name "tcl", hopefully on the path.
# Then normallize path for the platform.
#
if ![info exists TCL_PROGRAM] {
    if [info exists env(TCL_PROGRAM)] {
       set TCL_PROGRAM $env(TCL_PROGRAM)
    } else {
        set TCL_PROGRAM [info nameofexecutable]
        puts "    * WARNING: No environment variable TCL_PROGRAM, Using the"
        puts "    * command \"$TCL_PROGRAM\""
        puts "    * as the program to use for running subprocesses in the tests."
    }
}

set TCL_PROGRAM [eval file join [file split $TCL_PROGRAM]]

if {([info command unknown] == "") && ![info exists SAVED_UNKNOWN]} {
    error "can't find either unknown or SAVED_UNKNOWN"
}
if {[info command unknown] != ""} {
    set SAVED_UNKNOWN "proc unknown "
    append SAVED_UNKNOWN "\{[info args unknown]\} "
    append SAVED_UNKNOWN "\{[info body unknown]\}"
    rename unknown {}
}


#
# Convert a Tcl result code to a string.
#
proc TestResultCode code {
    switch -- $code {
        0 {return TCL_OK}
        1 {return TCL_ERROR}
        2 {return TCL_RETURN}
        3 {return TCL_BREAK}
        4 {return TCL_CONTINUE}
        default {return "***Unknown error code $code***"}
    }
}

#
# Output a test error.
#
proc OutTestError {test_name test_description contents_of_test
                   passing_int_result passing_result int_result result} {
    global TEST_ERROR_INFO errorInfo errorCode

    puts stderr "==== $test_name $test_description"
    puts stderr "==== Contents of test case:"
    puts stderr "$contents_of_test"
    puts stderr "==== Result was: [TestResultCode $int_result]"
    puts stderr "$result"
    puts stderr "---- Result should have been: [TestResultCode $passing_int_result]"
    puts stderr "$passing_result"
    puts stderr "---- $test_name FAILED" 
    if {[info exists TEST_ERROR_INFO] && [info exists errorInfo]} {
        puts stderr $errorCode
        puts stderr $errorInfo
        puts stderr "---------------------------------------------------"
    }
}

#
# Routine to execute tests and compare to expected results.
#
proc Test {test_name test_description contents_of_test passing_int_result
           passing_result {constraints {}}} {
    global testXConfig TEST_VERBOSE

    # Check constraints to see if we should run this test.
    foreach constraint $constraints {
        if {![info exists testXConfig($constraint)] ||
            !$testXConfig($constraint)} {
                return
        }
    }
    if [info exists TEST_VERBOSE] {
        puts "$test_name $test_description"
    }

    set int_result [catch {uplevel $contents_of_test} result]

    if {($int_result != $passing_int_result) ||
        ![cequal $result $passing_result]} {
        OutTestError $test_name $test_description $contents_of_test \
                     $passing_int_result $passing_result $int_result $result
    }
}

#
# Compare result against case-insensitive regular expression.
#

proc TestReg {test_name test_description contents_of_test passing_int_result
              passing_result} {
    set int_result [catch {uplevel $contents_of_test} result]

    if {($int_result != $passing_int_result) ||
        ![regexp -nocase $passing_result $result]} {
        OutTestError $test_name $test_description $contents_of_test \
                     $passing_int_result $passing_result $int_result $result
    }
}

proc dotests {file args} {
    global TESTS
    set savedTests $TESTS
    set TESTS $args
    source $file
    set TESTS $savedTests
}

# Genenerate a unique file record that can be verified.  The record
# grows quite large to test the dynamic buffering in the file I/O.

proc GenRec {id} {
    return [format "Key:%04d {This is a test of file I/O (%d)} KeyX:%04d %s" \
                    $id $id $id [replicate :@@@@@@@@: $id]]
}

# Proc to fork and exec child that loops until it gets a signal.
# Can optionally set its pgroup.  Wait till child has actually execed or
# kill breaks on some systems (i.e. AIX).  Windows is a drag, since the
# command line parsing is really dumb.  Pass it in a file instead.

proc ForkLoopingChild {{setPGroup 0}} {
    global TCL_PROGRAM tcl_platform

    set childProg {file delete CHILD.RUN; catch {while {1} {sleep 1}}; exit 10}

    # Create semaphore (it also contains the program to run for windows).
    set fh [open CHILD.RUN w]
    puts $fh $childProg
    close $fh
    flush stdout
    flush stderr

    if [cequal $tcl_platform(platform) unix] {
        set newPid [fork]
        if {$newPid == 0} {
            if $setPGroup {
                id process group set
            }
            catch {
                execl $TCL_PROGRAM [list -qc $childProg]
            } msg
            puts stderr "execl failed (ForkLoopingChild): $msg"
            exit 1
        }
    }
    if [cequal $tcl_platform(platform) windows] {
        if $setPGroup {
            error "setpgroup not supported on windows"
        }
        set newPid [execl $TCL_PROGRAM [list -q CHILD.RUN]]
    }
        
        
    # Wait till the child is actually running.
    while {[file exists CHILD.RUN]} {
        sleep 1
    }
    return $newPid
}

#
# Create a file.  If the directory doesn't exist, create it.
#
proc TestTouch file {
    file mkdir [file dirname $file]
    close [open $file w]
}

#
# Remove files and directories with out errors.
#
proc TestRemove args {
    foreach f $args {
        catch {file delete -force $f}
    }
}


