#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

set dir [lindex $argv 0]

proc CheckDep {file dep} {
    global $file $dep

    if [info exist $file\($dep\)] {
	return 1
    } else {
	foreach dd [array names $file] {
	    if [CheckDep $dd $dep] {
		return 1
	    }
	}
	return 0
    }
}

proc PrintDep {file dep} {
    global $file $dep

    if [info exist $file\($dep\)] {
	return "$file"
    } else {
	foreach dd [array names $file] {
	    set list [PrintDep $dd $dep]
	    if {$list != {}} {
		return "$file -> $list"
	    }
	}
	return {}
    }
}


foreach line [split [exec cat $dir/tclIndex] \n] {
    regsub auto_index $line a line
    regsub "\"source \{\\\$dir/" $line "" line
    regsub "\}\"" $line "" line

    if [regexp :: $line] {
	continue
    }

    eval $line
}

set fake(Compat.tcl,FileBox.tcl)	1
set fake(Compat.tcl,ComboBox.tcl)	1
set fake(Compat.tcl,WinFile.tcl)	1
set fake(Compat.tcl,UnixFile.tcl)	1
set fake(FileCmpt.tcl,Tix.tcl)		1
set fake(DefSchm.tcl,Tix.tcl)		1
set fake(Tix.tcl,Balloon.tcl)		1
set fake(FileCmpt.tcl,Tix.tcl)		1
set fake(Tix.tcl,FileDlg.tcl)		1
set fake(Tix.tcl,Shell.tcl)		1
set fake(FileBox.tcl,FileDlg.tcl)	1
set fake(UnixFile.tcl,WinFile.tcl)	1
set fake(WinFile.tcl,UnixFile.tcl)	1
set fake(UnixFile.tcl,Tix.tcl)		1
set fake(WinFile.tcl,Tix.tcl)		1
set fake(WinFile.tcl,Compat.tcl)	1


set fake(Tix.tcl,Balloon.tcl)		1
set fake(Tix.tcl,Shell.tcl)		1
set fake(Tix.tcl,FileDlg.tcl)		1
set fake(Utils.tcl,FileDlg.tcl)		1
set fake(ComboBox.tcl,FileBox.tcl)	1

cd ../library

if 1 {
    set TH [glob *.tcl]
} else {
    set TH {Compat.tcl FileBox.tcl Tix.tcl FileCmpt.tcl Tree.tcl Verify.tcl}
}

set hasError 0

foreach file [lsort $TH] {
    set files($file) 1
    set data [exec cat $file]
    foreach proc [array names a] {
	set otherFile $a($proc)

	if {$a($proc) == $file} {
	    continue
	}
	if [info exist $file\($otherFile\)] {
	    continue
	}
	if [regexp $proc $data] {
	    if [info exists fake($file,$otherFile)] {
		puts stderr "\t(ignored) FAKE dependence $file -> $otherFile"
		continue
	    } elseif [CheckDep $otherFile $file] {
		puts stderr "\t(error) CIRCULAR dependence $file -> $otherFile"
		puts stderr "\t$file -> [PrintDep $otherFile $file]"
		set hasError 1
	    } else {
 		set $file\($otherFile\) 1
		puts stderr  "$file -> $otherFile"
	    }
	}
    }
}

if {$hasError} {
    puts stderr "Error occurred"
    exit -1
} else {
    puts stderr "All dependencies resolved. Proceeding ..."
}

proc Load {file} {
    global loaded dir

    if [info exists loaded($file)] {
	return
    } else {
	global $file
	if [info exists $file] {
	    foreach n [array names $file] {
		Load $n
	    }
	}
	puts "    ET_INCLUDE( $dir/$file );"
	set loaded($file) 1
    }
}

proc LoadFiles {} {
    global files loaded
    catch {
	unset loaded
    }

    foreach f [array names files] {
	Load $f
    }
}

LoadFiles

puts stderr Done
