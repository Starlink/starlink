# dirlist.tcl --
#
# Create a HTML-formatted directory listing
#
# Steve Ball (c) 1997 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::dirlist 1.1
 
# DirList_IndexFile --
#
#	Define the index file for a directory
#
# Arguments:
#	pat	A glob pattern for index files in a directory.
#
# Results:
#	None
#
# Side Effects:
#	Sets the index file glob pattern.

proc DirList_IndexFile {pat} {
    global dirlist
    set dirlist(indexpat) $pat
}

# DirList_Directory --
#
#	Handle a directory.  Look for the index file, falling back to
#	the Directory Listing module, if necessary.  If the Directory
#	Listing is hidden, then give the not-found page.
#
# Arguments:
#	prefix	The URL domain prefix.
#	path	The file system pathname of the directory.
#	suffix	The URL suffix.
#	sock	The socket connection.
#
# Results:
#	None
#
# Side Effects:
#	Dispatches to the appropriate page handler.

proc DirList_Directory {prefix path suffix sock} {
    upvar #0 Httpd$sock data
    global dirlist tcl_platform

    # Special case because glob doesn't work in wrapped files
    # Just set indexpat to "index.tml" or "index.html"

    set npath [file join $path $dirlist(indexpat)]
    if {[info exist tcl_platform(isWrapped)] && $tcl_platform(isWrapped)} {
	set newest $npath
    } else {
	set newest [file_latest [glob -nocomplain $npath]]
    }
    if {[string length $newest]} {

	# Template hack.  Ask for the corresponding .html file in
	# case that file should be cached when running the template.
	# If we ask for the .tml directly then its result is never cached.
	global Template
	if {[string compare $Template(tmlExt) [file extension $newest]] == 0} {
	    set newest [file root $newest]$Template(htmlExt)
	}
	return [Doc_Handle $prefix $newest $suffix $sock]
    }
    if {[Dir_ListingIsHidden]} {
        # Directory listings are hidden, so give the not-found page.
        return [Doc_NotFound $sock]
    }
    # Listings are not hidden, so show it.
    Httpd_ReturnData $sock text/html [DirList $sock $path $data(url)]
}

# Dir_HideListings --
#
#	If a directory is viewed, hide the directory listing.
#
# Arguments:
#	None
#
# Results:
#	None
#
# Side Effects:
#	From now on, directory listings are hidden.

proc Dir_HideListings {} {
    global Doc
    set Doc(HideDirListing) 1
    return
}

# Dir_ShowListings --
#
#	If a directory is viewed, show the directory listing.
#
# Arguments:
#	None
#
# Results:
#	None
#
# Side Effects:
#	From now on, directory listings can be shown.

proc Dir_ShowListings {} {
    global Doc
    set Doc(HideDirListing) 0
    return
}

# Dir_ListingIsHidden --
#
#	Tell whether directory listings are currently hidden.
#
# Arguments:
#	None
#
# Results:
#	Returns 1 if listings are hidden, otherwise 0.
#
# Side Effects:
#	None

proc Dir_ListingIsHidden {} {
    global Doc
    return $Doc(HideDirListing)
}

# By default, directory listings are shown.
Dir_ShowListings

proc DirListForm {dir urlpath {sort name} {pattern *}} {
    set what [DirListTerm]
    set namecheck ""
    set sizecheck ""
    set numcheck ""
    switch -- $sort {
        number {
	    set numcheck checked
	}
	size {
	    set sizecheck checked
	}
	default {
	    set namecheck checked
	}
    }
    set listing "
<H1>Listing of $what $urlpath</H1>

<form action=$urlpath>
Pattern <input type=text name=pattern value=$pattern><br>
Sort by Modify Date <input type=radio name=sort value=number $numcheck>
or Name <input type=radio name=sort value=name $namecheck>
or Size <input type=radio name=sort value=size $sizecheck><br>
<input type=submit name=submit value='Again'><p>
"
    append listing [DirListInner $dir $urlpath $sort $pattern]
    append listing "</form>\n"
    return $listing
}

proc DirListInner {dir urlpath sort pattern} {
    set listing "<PRE>\n"
    set path [file split $dir]

    # Filter pattern to avoid leaking path information
    regsub -all {\.+/} $pattern {} pattern
    set pattern [string trimleft $pattern /]

    set list [glob -nocomplain -- [file join $dir $pattern]]
    if {[llength $path] > 1} {
	append listing \
	    "<A HREF=\"..\">Up to parent [string tolower [DirListTerm]]</A>\n"
    }

    set timeformat "%b %e, %Y %X"
    if {[llength $list] > 0} {
	set max 0
	foreach entry $list {
	    setmax max [string length [file tail $entry]]
	}
	incr max [string length </a>]

	# Resort the list into list2

	switch -- $sort {
	    number {
		set mlist {}
		foreach entry $list {
		    lappend mlist [list $entry [file mtime $entry]]
		}
		if {[catch {lsort -decreasing -integer -index 1 $mlist} list2]} {
		    set list2 [lsort -command DateCompare $mlist]
		}
		set extra 1
	    }
	    size {
		set slist {}
		foreach entry $list {
		    lappend slist [list $entry [file size $entry]]
		}
		if {[catch {lsort -decreasing -integer -index 1 $slist} list2]} {
		    set list2 [lsort -command SizeCompare $slist]
		}
		set extra 1
	    }
	    default {
		if {[catch {lsort -dict $list} list2]} {
		    set list2 [lsort -command DirlistCompare $list]
		}
		set extra 0
	    }
	}

	# Loop through list2, which may have an extra sorting field we ignore

	foreach entry $list2 {
	    if {$extra} {
		set entry [lindex $entry 0]
	    }
	    file lstat $entry lst
	    switch $lst(type) {
		file {
		    # Should determine dingbat from file type
		    append listing "<A HREF=\"[DirHref $entry]\">[format %-*s $max [file tail $entry]</a>] [format %8d $lst(size)] [format %-5s bytes]  [clock format $lst(mtime) -format $timeformat]\n"
		}
		directory {
		    append listing "<A HREF=\"[DirHref $entry]/\">[format %-*s $max [file tail $entry]/</a>] [format %8s {}] [format %-5s dir]  [clock format $lst(mtime) -format $timeformat]\n"
		}
		link {
		    append listing "<A HREF=\"[DirHref $entry]\">[format %-*s $max [file tail $entry]</a>] [format %8s {}] [format %-5s link]  [clock format $lst(mtime) -format $timeformat] -> [file readlink $entry]\n"
		}
		characterSpecial -
		blockSpecial -
		fifo -
		socket {
		    append listing "<A HREF=\"${urlpath}[file tail $entry]\">[format %-20s [file tail $entry]</a>] $lst(type)\n"
		}
	    }
	}
    } else {
	append listing "[DirListTerm] is empty\n"
    }
 
    append listing "
</PRE>
"
    return $listing
}

proc DirHref {entry} {
    set entry [Url_Encode [file tail $entry]]
    # Decode ".",
    regsub -all -nocase {%2e} $entry . entry
    regsub -all -nocase {%5f} $entry _ entry
    return $entry
}

proc DirList {sock dir urlpath} {
    upvar #0 Httpd$sock data

    set sort name
    set pattern *
    if [info exists data(query)] {
	foreach {name value} [Url_DecodeQuery $data(query)] {
	    switch $name {
		sort {set sort $value}
		pattern {set pattern $value}
	    }
	}
    }

    return "
<HTML>
<HEAD>
    <TITLE>Listing of [DirListTerm] $urlpath</TITLE>
</HEAD>
<BODY>
    [DirListForm $dir $urlpath $sort $pattern]
</BODY>
</HTML>"
}

# DirlistCompare --
#
# Utility procedure for case-insensitive filename comparison.
# Suitable for use with lsort
 
proc DirlistCompare {a b} {
    string compare [string tolower $a] [string tolower $b]
}
 

proc DateCompare {a b} {
    set a [lindex $a 1]
    set b [lindex $b 1]
    if {$a > $b} {
	return -1
    } elseif {$a < $b} {
	return 1
    } else {
	return 0
    }
}
 
proc SizeCompare {a b} {
    set aa [lindex $a 1]
    set bb [lindex $b 1]
    set res [string compare $aa $bb]
    if { $res != 0 } {
	return $res
    } else {
	return [string compare $a $b]
    }
}

# DirListTerm --
#
# Return "Folder" or "Directory" as appropriate

proc DirListTerm {} {
    global tcl_platform
 
    if [string compare macintosh $tcl_platform(platform)] {
	set what Directory
    } else {
	set what Folder
    }
    return $what
}
