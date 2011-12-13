#+
#
#  Name:
#    StarSTLFile
#
#  Type of module:
#    [incr Tcl] class
#
#  Purpose:
#    Support reading and querying an STL file, as defined in SUN/190
#    and written by the CAT library (SUN/181).
#
#  Description:
#    Reads in and parses a file containing a list of column
#    definition, parameters, and a table, and
#    allows queries on the result.
#
#  Invocation:
#    StarSTLFile object_name
#
#  Methods:
#    parameter {keyword} : returns the value corresponding to the given
#      parameter.  If there was no such parameter, then returns {}.
#    table {row col} : return the value in the specified table
#      position.  The column may be specified either as a number or a
#      column name.  If there is no such entry, or the corresponding
#      entry in the table was marked invalid, then it returns {}.
#      This can be detected by a subsequent call to isvalid{}.
#      The number of rows and columns in the table are available as
#      the parameters _ncolumns and _nrows.
#    isvalid {row col} : return true if the corresponding table element
#      was null or valid.
#    isvalid : return true if previous calls to table returned invalid data.
#    reset_isvalid : reset the `memory' of the isvalid{} function.
#    columns : return the column names in a list.
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
#     Copyright 2000, Central Laboratory of the Research Councils
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

itk::usual StarSTLFile {}

itcl::class gaia::StarSTLFile {

    # --- Inheritances
    # None

    # --- Constructor
    constructor {filename args} {
	set read_ok_ 0
	set nerrs 0
	set ncolumns_ 0
	set nrows_ 0
	if {$filename != {} && [file readable $filename]} {
	    set f [::open $filename r]
	    while {[::gets $f line] >= 0} {
		if {[regexp {^ *! *(.*)$} $line wholeline comm] != 0} {
		    set comments_ "$comments_$comm\n"
		} elseif {[regexp {^ *[Pp][^ ]* +([^ ]+) +([A-Za-z]+) +([0-9.eE+-]+)} $line wholeline name units value] != 0} {
		    set lname [string tolower $name]
		    set hash_(pv$lname) $value
		    set hash_(pu$lname) $units
		} elseif {[regexp {^ *[Cc][^ ]* +([^ ]+) +([A-Za-z]+) +([0-9]+)} $line wholeline name units colnum] != 0} {
		    set lname [string tolower $name]
		    set hash_(cn$lname) $colnum
		    set hash_(ca$colnum) $lname
		    set hash_(cu$colnum) $units
		    incr ncolumns_
		} elseif {[regexp -nocase {^ *begintable} $line] != 0} {
		    if {[read_array_ $f] != 0} {
			incr nerrs
		    }
		} elseif {[regexp {^ *$} $line] != 0 || [regexp {^ *:} $line] != 0} {
		    # blank or continuation line -- do nothing
		} else {
		    set errs_ "${errs_}Warning: Unparseable line <$line>"
		}
	    }
	    ::close $f

	    set hash_(pv_ncolumns) $ncolumns_
	    set hash_(pv_nrows) $nrows_

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

    public method parameter {keyword}
    public method table {row col}
    public method reset_isvalid {}
    public method isvalid {{row -1} {col -1}}
    public method columns {}
    public method status {}
    public method error_msg {}
    public method comments {}

    # --- Private methods and variables

    private method read_array_ {stream}

    private variable hash_
    private variable comments_ {}
    private variable errs_ {}
    private variable read_ok_ 0
    private variable ncolumns_ 0
    private variable nrows_ 0
    private variable table_returned_isvalid_ 1
}

body gaia::StarSTLFile::parameter {keyword} {
    if {[info exists hash_(pv$keyword)]} {
	return $hash_(pv$keyword)
    } else {
	return {}
    }
}

body gaia::StarSTLFile::table {row col} {
    if {[regexp {^[0-9]+$} $col] != 0} {
	set colindex $col
    } else {
	set lname [string tolower $col]
	set colindex $hash_(cn$lname)
    }
    if {$colindex != "" && [info exists hash_(t-$row-$colindex)]} {
	return $hash_(t-$row-$colindex)
    } else {
	set table_returned_isvalid_ 0
	return {}
    }
}

body gaia::StarSTLFile::reset_isvalid {} {
    set table_returned_isvalid_ 1
}

body gaia::StarSTLFile::isvalid {{row -1} {col -1}} {
    if {$row < 0} {
	# no arguments -- return accumulated invalid flag
	return $table_returned_isvalid_
    } else {
	if {[regexp {^[0-9]+$} $col] != 0} {
	    set colindex $col
	} else {
	    set lname [string tolower $col]
	    set colindex $hash_(cn$lname)
	}
	return [info exists hash_(t-$row-$colindex)]
    }
}

body gaia::StarSTLFile::columns {} {
    set collist {}
    set eolist 0
    for {set c 1} {!$eolist} {incr c} {
	if {[info exists hash_(ca$c)]} {
	    lappend collist $hash_(ca$c)
	} else {
	    set eolist 1
	}
    }
    return $collist
}

body gaia::StarSTLFile::status {} {
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

body gaia::StarSTLFile::error_msg {} {
    return $errs_
}

body gaia::StarSTLFile::comments {} {
    return $comments_
}

# Read a table from $stream, until EOF.  We know that there are
# ncolumns_ columns, but we don't know how many rows there are.
#
# Return 0 on success.
body gaia::StarSTLFile::read_array_ {stream} {

    set r 1
    while {[::gets $stream line] >= 0} {

	set l1 [string trim $line]
	regsub -all {  +} $l1 " " l2
	set ll [split $l2]
	if {$ncolumns_ != [llength $ll]} {
	    set errs_ "${errs_}Warning: Row $r has the wrong number of elements ($ncolumns_ != [llength $ll]: discarded)"
	} else {
	    for {set c 1} {$c <= $ncolumns_} {incr c} {
		set t [lindex $ll [expr $c-1]]
		if {$t != "<null>"} {
		    set hash_(t-$r-$c) $t
		}
	    }
	    incr r
	}
    }
    set nrows_ [expr $r-1]
    return 0
}
