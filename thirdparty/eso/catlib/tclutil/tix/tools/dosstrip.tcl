#! /bin/sh
# the next line restarts using tclsh7.6 \
unset TCL_LIBRARY; exec tclsh7.6 "$0" "$@"


if {$argv == {}} {
    puts "usage: dosstrip.tcl \[-n|-f\] file"
    exit 1
}

set files $argv

if {[llength $files] > 1 && [lindex $files 0] == "-n"} {
    set test 1
    set files [lrange $files 1 end]
} else {
    set test 0
}

if {[llength $files] > 1 && [lindex $files 0] == "-f"} {
    set forced 1
    set files [lrange $files 1 end]
} else {
    set forced 0
}

foreach file $files {
    if [file isdir $file] {
	puts stderr "$file is a directory"
	continue
    }
    if [catch {set fd [open $file {RDONLY}]}] {
	puts stderr "Cannot open $file for reading"
	continue
    }
    fconfigure $fd -translation binary
    set data [read $fd [file size $file]]
    close $fd
    set ctrlM [format %s \r]
    if {[regsub -all $ctrlM $data "" data]} {
	if {$test} {
	    puts "$file contains ^M"
	} else {
	    set chmod 0
	    if [catch {set fd [open $file {WRONLY TRUNC}]}] {
		if $forced {
		    catch {exec chmod u+w $file}
		    if [catch {set fd [open $file {WRONLY TRUNC}]}] {
			puts stderr "Cannot open $file for writing"
			continue
		    }
		    set chmod 1
		} else {
		    puts stderr "Cannot open $file for writing"
		    continue
		}
	    }
	    puts $fd $data
	    close $fd
	    if {$chmod} {
		catch {exec chmod u-w $file}
	    }
	}
	puts "+ $file"
    } else {
	if {$test} {
	    puts "$file does not contain ^M"
	} else {
	    puts "- $file"
	}
    }
}
