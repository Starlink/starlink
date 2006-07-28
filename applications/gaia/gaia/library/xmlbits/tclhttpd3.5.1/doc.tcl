# doc.tcl
#
# File system based URL support.
# This calls out to the Auth module to check for access files.
# Once a file is found, it checks for content-type handlers defined
# by Tcl procs of the form Doc_$contentType.  If those are present
# then they are responsible for processing the file and returning it.
# Otherwise the file is returned by Doc_Handle.
#
# If a file is not found then a limited form of content negotiation is
# done based on the browser's Accept header.  For example, this makes
# it easy to transition between foo.shtml and foo.html.  Just rename
# the file and content negotiation will find it from old links.
#
# Stephen Uhler / Brent Welch (c) 1997-1998 Sun Microsystems
# Brent Welch (c) 1998-2000 Ajuba Solutions
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# RCS: @(#) $Id$

package provide httpd::doc 1.1

package require uri
package require httpd::fallback
package require httpd::subst
package require httpd::template
package require httpd::dirlist
package require httpd::doc_error
package require httpd::cookie

# Doc_Root --
#
# Query or set the physical pathname of the document root
#
# Arguments:
#	real 	Optional.  The name of the file system directory
#		containing the root of the URL tree.  If this is empty,
#		then the current document root is returned instead.
#	args	"real" followed by "args" for Url_PrefixInstall.
#		"real" is the name of the file system directory
#		containing the root of the URL tree.  If no args are given,
#		then the current document root is returned instead.
#
# Results:
#	If querying, returns the name of the directory of the document root.
#	Otherwise returns nothing.
#
# Side Effects:
#	Sets the document root.

proc Doc_Root {args} {
    global Doc
    if {[llength $args] > 0} {
        set real [lindex $args 0]
	set Doc(root) $real
	eval [list Doc_AddRoot / $real] [lrange $args 1 end]
        return
    }
    return $Doc(root)
}

# Doc_AddRoot
#	Add a file system to the virtual document hierarchy
#
# Arguments:
#	virtual		The URL prefix of the document tree to add.
#	directory	The file system directory containing the doc tree.
#	args		Same as args for Url_PrefixInstall
#
# Results:
#	None
#
# Side Effects:
#	Sets up a document URL domain and the document-based access hook.

proc Doc_AddRoot {virtual directory args} {
    Doc_RegisterRoot $virtual $directory
    eval [list Url_PrefixInstall $virtual [list DocDomain $virtual $directory]] $args
    Url_AccessInstall DocAccessHook
    return
}

# Doc_RegisterRoot
#	Add a file system managed by any Domain Handler (e.g. CGI)
#	This is necessary for Doc_AccessControl to search directories right.
#
# Arguments:
#	virtual		The prefix of the URL
#	directory	The directory that corresponds to $virtual
#
# Results:
#	None
#
# Side Effects:
#	Registers the URL to directory mapping

proc Doc_RegisterRoot {virtual directory} {
    global Doc
    if {[info exist Doc(root,$virtual)] &&
	    [string compare $Doc(root,$virtual) $directory] != 0} {
	return -code error \
		"Doc_RegisterRoot will not change an existing url to directory mapping"
    }
    set Doc(root,$virtual) $directory
}

# Doc_Include
#
#	Read the contents of the file and substitute them at the stack level
#	at which this procedure was called.  Relative paths will be joined
#	with the directory at the top of the include stack in global variable
#	page(includeStack).
#
# Arguments:
#	filename	The relative path of the file to read.
#
# Results:
#	Returns the substituted contents of the file.
#
# Side Effects:
#	None.

proc Doc_Include {filename} {
    global page

    # Use the path at the top of the include stack.
    set oldIncludeStack $page(includeStack)
    set path [file join [lindex $oldIncludeStack 0] $filename]

    # Read the data from the file.
    ::set f [open $path]
    ::set data [read -nonewline $f]
    close $f

    # Call subst on the data in the stack frame above this one.

    # To support nested includes, we use a uniquely named
    # variable to store incremental results.
    ::set resultVar "__result_[expr rand()]"

    # Create the script to eval in the stack frame above this one.
    ::set script "append $resultVar \[subst \{$data\}\]"

    # Create a temporary variable in the stack frame above this one,
    # and use it to store the incremental resutls of the multiple loop
    # iterations.  Remove the temporary variable when we're done so there's
    # no trace of this loop left in that stack frame.
    upvar $resultVar tmp
    ::set tmp ""

    # Including of doc templates can be nested.
    # Push the directory of the file we are subst'ing onto the include stack.
    # Perform the substitution, and the pop that dir from the stack.
    set page(includeStack) [linsert $oldIncludeStack 0 [file dirname $path]]
    uplevel $script
    set page(includeStack) $oldIncludeStack

    ::set result $tmp
    unset tmp
    return $result
}

# Doc_PublicHtml --
#
# Enable URLS of the form ~user/a/b/c and map those to
# a subdirectory of that users account.
#
# Arguments:
#	homedir The directory under a user's home that is their
#		personal URL root.  Defaults to public_html.
#		If this is empty, then user home directories
#		are disabled.
#
# Results:
#	None
#
# Side Effects:
#	Sets the per-user public_html directory name.

proc Doc_PublicHtml {{homedir public_html}} {
    global Doc
    if {[string length $homedir] == 0} {
	catch {unset Doc(homedir)}
    } else {
	set Doc(homedir) [string trim $homedir /]
    }
}

# Doc_Virtual - return a real pathname corresponding to a 
# "virtual" path in an include
#
# Arguments:
#	sock	The client connection.
#	curfile	The pathname of the file that contains the
#		"virtual" URL spec.  This is used to resolve
#		relative URLs.
#	virtual	The URL we need the file name of.
#
# Results:
#	The file name corresponding to the URL.
#	If "" is returned, then the URL is invalid.
#
# Side Effects:
#	None

proc Doc_Virtual {sock curfile virtual} {
    global Doc
    if {[regexp ^~ $virtual]} {
	# This is UNIX-specific, so we don't need file joins
	if {![info exists Doc(homedir)]} {
	    return {}	;# Not allowed
	}
	set list [split $virtual /]
	set user [lindex $list 0]
	if {[catch {glob $user} homedir]} {
	    return {}	;# No such user
	}
	return $homedir/$Doc(homedir)/[join [lrange $list 1 end] /]
    }

    # Try to hook up the pathname under the appropriate document root

    if {[regexp ^/ $virtual]} {
	Url_PrefixMatch $virtual prefix suffix
	if {[info exist Doc(root,$prefix)]} {
	    return [file join $Doc(root,$prefix) [string trimleft $suffix /]]
	} else {
	    # Not a document domain, so there cannot be a file behind this url.

	    return {}
	}
    }

    # Non-absolute URL

    return [file join [file dirname $curfile] $virtual]
}

# DocAccessHook
#
#	Access handle for Doc domains.
#	This looks for special files in the file system that
#	determine access control.  This is registered via
#	Url_AccessInstall
#
# Arguments:
#	sock	Client connection
#	url	The full URL. We realy need the prefix/suffix, which
#		is stored for us in the connection state
#
# Results:
#	"denied", in which case an authorization challenge or
#	not found error has been returned.  Otherwise "skip"
#	which means other access checkers could be run, but
# 	most likely access will be granted.

proc DocAccessHook {sock url} {
    global Doc
    upvar #0 Httpd$sock data

    # Make sure the path doesn't sneak out via ..
    # This turns the URL suffix into a list of pathname components

    if {[catch {Url_PathCheck $data(suffix)} data(pathlist)]} {
	Doc_NotFound $sock
	return denied
    }

    # Figure out the directory corresponding to the domain, taking
    # into account other document roots.

    if {[info exist Doc(root,$data(prefix))]} {
	set directory $Doc(root,$data(prefix))
    } else {
	set directory [file join $Doc(root,/) [string trimleft $data(prefix) /]]
    }

    # Look for .htaccess and .tclaccess files along the path
    # If you wanted to have a time-limited cache of these
    # cookies you could save the cost of probing the file system
    # for these files on each URL.

    set cookie [Auth_Check $sock $directory $data(pathlist)]

    # Finally, check access

    if {![Auth_Verify $sock $cookie]} {
	return denied
    } else {
	return skip
    }
}

# DocDomain --
#
# Main handler for Doc domains (i.e. file systems)
# This looks around for a file and, if found, uses Doc_Handle
# to return the contents.
#
# Arguments:
#	prefix		The URL prefix of the domain.
#	directory	The directory containing teh domain.
#	sock		The socket connection.
#	suffix		The URL after the prefix.
#
# Results:
#	None
#
# Side Effects:
#	Dispatch to the document handler that is in charge
#	of generating an HTTP response.

proc DocDomain {prefix directory sock suffix} {
    global Doc
    upvar #0 Httpd$sock data

    # The pathlist has been checked and URL decoded by
    # DocAccess, so we ignore the suffix and recompute it.

    set pathlist $data(pathlist)
    set suffix [join $pathlist /]

    # Check for personal home pages

    if {[regexp ^~ $pathlist] && [info exists Doc(homedir)]} {
	set user [lindex $pathlist 0]
	if {[catch {glob $user} homedir]} {
	    Doc_NotFound $sock
	    return	;# No such user
	}
	set directory [file join $homedir $Doc(homedir)]
	if {![file isdirectory $directory]} {
	    Doc_NotFound $sock
	    return	;# No User's Public Directory
	}
	if {![file readable $directory]} {
	    Doc_NotFound $sock
	    return	;# No access to User's Public Directory
	}
	set pathlist [lrange $pathlist 1 end]
	set suffix [join $pathlist /]
    }

    # Handle existing files

    # The file join here is subject to attacks that create absolute
    # pathnames outside the URL tree.  We trim left the / and ~
    # to prevent those attacks.

    set path [file join $directory [string trimleft $suffix /~]]
    set path [DocPathNormalize $path]
    set data(path) $path	;# record this path for not found handling

    if {[file exists $path]} {
	CountName $data(url) hit
	Doc_Handle $prefix $path $suffix $sock
	return
    }

    # Try to find an alternate.

    if {![Fallback_Try $prefix $path $suffix $sock]} {
	# Couldn't find anything.
	# check for cgi script in the middle of the path
	Cgi_Domain $prefix $directory $sock $suffix
    }
}

# Doc_Handle --
#
# Handle a document URL.  Dispatch to the mime type handler, if defined.
#
# Arguments:
#	prefix	The URL prefix of the domain.
#	path	The file system pathname of the file.
#	suffix	The URL suffix.
#	sock	The socket connection.
#
# Results:
#	None
#
# Side Effects:
#	Dispatch to the correct document handler.

proc Doc_Handle {prefix path suffix sock} {
    upvar #0 Httpd$sock data
    if {[file isdirectory $path]} {
	if {[string length $data(url)] && ![regexp /$ $data(url)]} {

	    # Insist on the trailing slash

	    Httpd_RedirectDir $sock
	    return
	}
	DirList_Directory $prefix $path $suffix $sock
    } elseif {[file readable $path]} {
	
	# Look for Tcl procedures whos name match the MIME Content-Type

	set cmd Doc_[Mtype $path]
	if {![iscommand $cmd]} {
	    Httpd_ReturnFile $sock [Mtype $path] $path
	} else {
	    $cmd $path $suffix $sock
	}
    } else {
	# Either not found, or we can find an alternate (e.g. a template).

	if {![Fallback_Try $prefix $path $suffix $sock]} {
	    Doc_NotFound $sock
	}
    }
}

# Doc_GetPath --
#	
#	Return a list of unique directories from domain root to a given path
#	Adjusts for Document roots and user directories
#
# Arguments:
#	sock		The client connection
#	file		The file endpoint of the path
# Results:
#	A list of directories from root to directory of $data(path)
#
# Side Effects:
#	None.

proc Doc_GetPath {sock {file ""}} {
    global Doc
    upvar #0 Httpd$sock data

    if {$file == ""} {
	set file $data(path)
    }

    # Start at the Doc_AddRoot point
    if {[info exist Doc(root,$data(prefix))]} {
	set root $Doc(root,$data(prefix))

	# always start in the rootdir
	set dirs $Doc(root)
    } else {
	set root $Doc(root,/)
	set dirs {}
    }

    set dirsplit [file split [file dirname $file]]
    if {[string match ${root}* $file]} {

	# Normal case of pathname under domain prefix

	set path $root
	set extra [lrange $dirsplit [llength [file split $root]] end]

    } elseif {[set hindex [lsearch -exact $dirsplit $Doc(homedir)]] >= 0} {
	
	# "public_html" is in the path, so we have been warped
	# to a user's URL tree.

	set path [eval file join [lrange $dirsplit 0 $hindex]]
	incr hindex
	set extra [lrange $dirsplit $hindex end]
    } else {
	# Don't know where we are - just use the current directory

	set path [file dirname $file] 
	set extra {}
    }

    foreach dir [concat [list {}] $extra] {
	set path [file join $path $dir]
	# Don't add duplicates to the list.
	if {[lsearch $dirs $path] == -1} {
	    lappend dirs $path
	}
    }

    return $dirs
}

# Helper routine for cleaning up file names
# Part of the context here is that "file exists" on Windows
# ignores trailing . in a pathname, leading to cute attacks

if {[catch {file normalize /a/b/../c/foo.tml.}]} {
  # No file normalize command
  if {$tcl_platform(platform) == "windows"} {
    proc DocPathNormalize {path} {
      return [string trimright $path .]
    }
  } else {
    proc DocPathNormalize {path} {
      return $path
    }
  }
} else {
  proc DocPathNormalize {path} {
    return [file normalize $path]
  }
}

# Compat routines with 3.4 routines

catch {interp alias {} Doc_Dynamic {} Template_Dynamic}
catch {interp alias {} Doc_Redirect {} Redirect_To}
catch {interp alias {} Doc_RedirectSelf {} Redirect_Self}
