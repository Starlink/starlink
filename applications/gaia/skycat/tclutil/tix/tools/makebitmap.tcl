#!/bin/sh
# the next line restarts using tclsh \
exec tclsh "$0" "$@"

# makebitmap.tcl --
#
#	Generates a ET file that Includes all built-in bitmaps and 
#	bitmaps.
#
# Copyright (c) 1996, Expert Interface Technologies
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.


set PWD [pwd]

foreach dir $argv {
    cd $dir

    foreach filename [glob -nocomplain *] {
	case [file ext $filename] {
	    {.xbm} {
		set root [file root $filename]
		if {$root == ""} {
		    continue
		}

		set bitsName {}
		set widthName {}
		set heightName {}
		set data [exec cat $filename]
		regexp {[A-z_0-9]*_bits} $data bitsName
		regexp {[A-z_0-9]*_width} $data widthName
		regexp {[A-z_0-9]*_height} $data heightName
		if ![regexp unsigned $data] {
		    regsub char $data "unsigned char" data
		}

		if {$bitsName == {} ||
		    $widthName == {} ||
		    $heightName == {}} {

		    puts stderr \
		"Warning: bad XBM format in file $dir/$filename, file excluded"
		} else {
		    puts "\{"
		    puts [exec cat $filename]
		    puts -nonewline "Tk_DefineBitmap(Et_Interp, "
		    puts -nonewline "Tk_GetUid(\"$root\"), "
		    puts -nonewline "$bitsName, "
		    puts -nonewline "$widthName, "
		    puts -nonewline "$heightName);"
		    puts ""
		    puts "\}"
		}

	    }
	    {.xpm} {
		set name [file root $filename]
		set data [exec cat $filename]
		if [regexp char\[^\\\[\]* $data root] {
		    regsub char   $root "" root
		    regsub \[\*\] $root "" root
		    set root [string trim $root]
		} else {
		    set root [file root $filename]\_xpm
		}
		puts "\{"
		puts [exec cat $filename]
		puts -nonewline "Tix_DefinePixmap(Et_Interp, "
		puts -nonewline "Tk_GetUid(\"$name\"), "
		puts -nonewline "$root);"
		puts ""
		puts "\}"
	    }
	}
    }
    cd $PWD
}

