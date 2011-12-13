#+
#
#  Name:
#    StarKeywordFile
#
#  Type of module:
#    [incr Tcl] class
#
#  Purpose:
#    Support reading and querying a file of keywords.
#
#  Description:
#    Reads in and parses a file containing a list of keywords, and
#    allows queries on the result.
#
#  Invocation:
#    StarKeywordFile object_name
#
#  Methods:
#    query {keyword} : returns the value corresponding to the given
#      keyword.  If there was no such keyword, then returns {}.
#    status : non-zero if the parsing of the file was completed without
#      error.  The return value is positive if there were no warnings,
#      and negative if there were.
#    error_msg : Any error or warning message.
#    comments : return any comments as a single string.
#
#  Inheritance:
#    None
#
#  Copyright:
#     Copyright 1999, Central Laboratory of the Research Councils
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA
#
#  Author:
#    NG: Norman Gray (Starlink, Glasgow)
#
#  History:
#    14-DEC-1999 (NG):
#      Initial version
#
#  RCS Id:
#    $Id$
#-

itk::usual StarKeywordFile {}

itcl::class gaia::StarKeywordFile {

    # --- Inheritances
    # None

    # --- Constructor
    constructor {filename args} {
	set read_ok_ 0
	set nerrs 0
	if {$filename != {} && [file readable $filename]} {
	    set f [::open $filename r]
	    while {[::gets $f line] >= 0} {
		if {[regexp {^ *# *(.*)$} $line wholeline comm] != 0} {
		    set comments_ "$comments_$comm\n"
		} elseif {[regexp {^ *([a-zA-Z0-9]+) *\(([0-9, ]+)\) *= *(.*)} $line wholeline k dims v] != 0} {
		    if {[read_array_ $f $k $dims $v] != 0} {
			incr nerrs
		    }
		} elseif {[regexp {^ *([a-zA-Z0-9]+) *= *(.*)} $line wholeline k v] != 0} {
		    set hash_($k) $v
		    #puts "hash_($k)=$v"
		} else {
		    set errs_ "${errs_}warning: unparsable line <$line>\n"
		}
	    }
	    ::close $f

	    if {$nerrs > 0} {
		set read_ok_ 0
	    } else {
		set read_ok_ 1
	    }
	}

	# Evaluate any options
	eval configure $args
    }

    # --- Public methods and variables

    public method query {keyword {i1 -1} {i2 -1}}
    public method status {}
    public method error_msg {}
    public method comments {}

    # --- Private methods and variables

    private method read_array_ {stream keyword dims leadingval}

    private variable hash_
    private variable comments_ {}
    private variable errs_ {}
    private variable read_ok_ 0
}

body gaia::StarKeywordFile::query {keyword {i1 -1} {i2 -1}} {
    if {$i1 < 0} {
	set idx $keyword
    } elseif {$i2 < 0} {
	set idx "$keyword,1,$i1"
    } else {
	set idx "$keyword,$i1,$i2"
    }
    if {[info exists hash_($idx)]} {
	return $hash_($idx)
    } else {
	return {}
    }
}

body gaia::StarKeywordFile::status {} {
    if {$read_ok_} {
	if {$errs_ == {}} {
	    return 1
	} else {
	    return -1
	}
    } else {
	return 0
    }
}

body gaia::StarKeywordFile::error_msg {} {
    return $errs_
}

body gaia::StarKeywordFile::comments {} {
    return $comments_
}

# Read an array of dimensions $dims , starting with the string in
# $leadingval, and reading lines from $stream until the array is
# full.  Null dimensions are ignored.
#
# Return 0 on success.
body gaia::StarKeywordFile::read_array_ {stream keyword dims leadingval} {
    set dims1 [string trim $dims ", "]
    regsub -all { } $dims1 {} dims2
    regsub -all {,,+} $dims2 , tdims

    set dimsl [split $tdims ,]
    set ndims [llength $dimsl]
    if {$ndims == 1} {
	set nrows 1
	set ncols [lindex $dimsl 0]
    } elseif {$ndims == 2} {
	set nrows [lindex $dimsl 0]
	set ncols [lindex $dimsl 1]
    } else {
	set errs_ "${errs_}Error: too many dimensions in ($dims)"
	return 1
    }

    set totalvals [expr $nrows * $ncols]
    set line $leadingval
    set r 1
    set c 1
    set n 0
    set eof 0
    while {$n < $totalvals && !$eof} {
	set l1 [string trim $line]
	regsub -all {  +} $l1 " " l2
	set ll [split $l2]
	for {set nv 0} {$nv < [llength $ll]} {incr nv} {
	    set hash_($keyword,$r,$c) [lindex $ll $nv]
	    #puts "$n/$totalvals: hash_($keyword,$r,$c)=$hash_($keyword,$r,$c)"
	    incr n
	    incr c
	    if {$c > $ncols} {
		incr r
		set c 1
	    }
	}
	if {$n < $totalvals} {
	    if {[::gets $stream line] < 0} {
		set errs_ "${errs_}Error: unexpected EOF"
		return 1
	    }
	}
    }
    return 0
}
