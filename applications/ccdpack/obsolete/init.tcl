# init.tcl --
#
# Default system startup file for Tcl-based applications.  Defines
# "unknown" procedure and auto-load facilities.
#
# SCCS: @(#) init.tcl 1.66 96/10/06 14:29:28
#
# Copyright (c) 1991-1993 The Regents of the University of California.
# Copyright (c) 1994-1996 Sun Microsystems, Inc.
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
# ======================================================================
# >>>>>>>>>>>>>>>> INCLUDES MODIFICATIONS FOR [incr Tcl] <<<<<<<<<<<<<<<<<
#
#   AUTHOR:  Michael J. McLennan
#            Bell Labs Innovations for Lucent Technologies
#            mmclennan@lucent.com
#            http://www.tcltk.com/itcl
#
#      RCS:  $Id$
# ======================================================================
#               Copyright (c) 1993-1996  Lucent Technologies
# ----------------------------------------------------------------------
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#

if {[info commands package] == ""} {
    error "version mismatch: library\nscripts expect Tcl version 7.5b1 or later but the loaded version is\nonly [info patchlevel]"
}
package require -exact Tcl 7.6
if [catch {set auto_path $env(TCLLIBPATH)}] {
    set auto_path ""
}
if {[lsearch -exact $auto_path [info library]] < 0} {
    lappend auto_path [info library]
}
catch {
    foreach dir $tcl_pkgPath {
	if {[lsearch -exact $auto_path $dir] < 0} {
	    lappend auto_path $dir
	}
    }
    unset dir
}
package unknown tclPkgUnknown
if {[info commands exec] == ""} {

    # Some machines, such as the Macintosh, do not have exec. Also, on all
    # platforms, safe interpreters do not have exec.

    set auto_noexec 1
}
set errorCode ""
set errorInfo ""

set unknown_handler_order {}

# ------------------------------------------------------------------------
#  USAGE: unknown ?command arg arg...?
#
#  Invoked automatically whenever an unknown command is encountered.
#  Works through a list of "unknown handlers" that have been registered
#  to deal with unknown commands.  Extensions can integrate their own
#  handlers into the "unknown" facility via "unknown_handle".
#
#  If a handler exists that recognizes the command, then it will
#  take care of the command action and return a valid result or a
#  Tcl error.  Otherwise, it should return "-code continue" (=2)
#  and responsibility for the command is passed to the next handler.
# ------------------------------------------------------------------------
proc unknown {args} {
    global unknown_handler_order unknown_handlers errorInfo errorCode

    #
    # Be careful to save error info now, and restore it later
    # for each handler.  Some handlers generate their own errors
    # and disrupt handling.
    #
    set savedErrorCode $errorCode
    set savedErrorInfo $errorInfo

    foreach handler $unknown_handler_order {
        set status [catch {uplevel $unknown_handlers($handler) $args} result]

        if {$status == 1} {
            #
            # Strip the last five lines off the error stack (they're
            # from the "uplevel" command).
            #
            set new [split $errorInfo \n]
            set new [join [lrange $new 0 [expr [llength $new] - 6]] \n]
            return -code $status -errorcode $errorCode \
                -errorinfo $new $result

        } elseif {$status != 4} {
            return -code $status $result
        }

        set errorCode $savedErrorCode
        set errorInfo $savedErrorInfo
    }

    set name [lindex $args 0]
    return -code error "invalid command name \"$name\""
}

# ------------------------------------------------------------------------
#  USAGE: unknown_handler name command ?arg arg...?
#
#  Registers another handler to be used in conjunction with the
#  unknown command.  Each time an unknown command is encountered, the
#  list of handlers is consulted to determine how to handle the command.
#  If a handler recognizes the command, then it should load, define,
#  or otherwise handle the command and return.  Otherwise, it should
#  return "-code continue", which tells the "unknown" facility to
#  continue with another handler.
#
#  Handlers registered first are consulted last.  This way, extensions
#  that are loaded later have higher priority for handling special cases.
#
#  Usually "command" is the name of a handling procedure.  But extra
#  arguments can be specified when the handler is registered, and
#  these will be kept as arguments when the handler is invoked.  The
#  actual unknown command and its arguments are appended after these.
# ------------------------------------------------------------------------
proc unknown_handler {name args} {
    global unknown_handler_order unknown_handlers

    set unknown_handlers($name) $args
    set unknown_handler_order [linsert $unknown_handler_order 0 $name]
}

# tcl_unknown:
# Invoked when a Tcl command is invoked that doesn't exist in the
# interpreter:
#
#	1. See if the autoload facility can locate the command in a
#	   Tcl script file.  If so, load it and execute it.
#	2. If the command was invoked interactively at top-level:
#	    (a) see if the command exists as an executable UNIX program.
#		If so, "exec" the command.
#	    (b) see if the command requests csh-like history substitution
#		in one of the common forms !!, !<number>, or ^old^new.  If
#		so, emulate csh's history substitution.
#	    (c) see if the command is a unique abbreviation for another
#		command.  If so, invoke the command.
#
# Arguments:
# args -	A list whose elements are the words of the original
#		command, including the command name.

unknown_handler "tcl" tcl_unknown

proc tcl_unknown args {
    global auto_noexec auto_noload env unknown_pending tcl_interactive
    global errorCode errorInfo

    # Save the values of errorCode and errorInfo variables, since they
    # may get modified if caught errors occur below.  The variables will
    # be restored just before re-executing the missing command.

    set savedErrorCode $errorCode
    set savedErrorInfo $errorInfo
    set name [lindex $args 0]
    if ![info exists auto_noload] {
	#
	# Make sure we're not trying to load the same proc twice.
	#
	if [info exists unknown_pending($name)] {
	    unset unknown_pending($name)
	    if {[array size unknown_pending] == 0} {
		unset unknown_pending
	    }
	    return -code error "self-referential recursion in \"unknown\" for command \"$name\"";
	}
	set unknown_pending($name) pending;
	set ret [catch {auto_load $name} msg]
	unset unknown_pending($name);
	if {$ret != 0} {
	    return -code $ret -errorcode $errorCode \
		"error while autoloading \"$name\": $msg"
	}
	if ![array size unknown_pending] {
	    unset unknown_pending
	}
	if $msg {
	    set errorCode $savedErrorCode
	    set errorInfo $savedErrorInfo
	    set code [catch {uplevel $args} msg]
	    if {$code ==  1} {
		#
		# Strip the last five lines off the error stack (they're
		# from the "uplevel" command).
		#

		set new [split $errorInfo \n]
		set new [join [lrange $new 0 [expr [llength $new] - 6]] \n]
		return -code error -errorcode $errorCode \
			-errorinfo $new $msg
	    } else {
		return -code $code $msg
	    }
	}
    }
    if {([info level] == 1) && ([info script] == "") \
	    && [info exists tcl_interactive] && $tcl_interactive} {
	if ![info exists auto_noexec] {
	    set new [auto_execok $name]
	    if {$new != ""} {
		set errorCode $savedErrorCode
		set errorInfo $savedErrorInfo
		return [uplevel exec >&@stdout <@stdin $new [lrange $args 1 end]]
	    }
	}
	set errorCode $savedErrorCode
	set errorInfo $savedErrorInfo
	if {$name == "!!"} {
	    return [uplevel {history redo}]
	}
	if [regexp {^!(.+)$} $name dummy event] {
	    return [uplevel [list history redo $event]]
	}
	if [regexp {^\^([^^]*)\^([^^]*)\^?$} $name dummy old new] {
	    return [uplevel [list history substitute $old $new]]
	}
	set cmds [info commands $name*]
	if {[llength $cmds] == 1} {
	    return [uplevel [lreplace $args 0 0 $cmds]]
	}
	if {[llength $cmds] != 0} {
	    if {$name == ""} {
		return -code error "empty command name \"\""
	    } else {
		return -code error \
			"ambiguous command name \"$name\": [lsort $cmds]"
	    }
	}
    }
    return -code continue
}

# auto_load --
# Checks a collection of library directories to see if a procedure
# is defined in one of them.  If so, it sources the appropriate
# library file to create the procedure.  Returns 1 if it successfully
# loaded the procedure, 0 otherwise.
#
# Arguments: 
# cmd -			Name of the command to find and load.

proc auto_load cmd {
    global auto_index auto_oldpath auto_path env errorInfo errorCode

    if {[info exists auto_index($cmd)]} {
        set loadcmd $auto_index($cmd)
    } else {
        foreach nsref [uplevel import all] {
            set ns [lindex $nsref 0]
			if {$ns == "::"} {
            	set fullname "::$cmd"
			} else {
            	set fullname "$ns::$cmd"
			}
            if {[info exists auto_index($fullname)]} {
                set loadcmd $auto_index($fullname)
                break
            }
        }
    }
    if {[info exists loadcmd]} {
	uplevel #0 $loadcmd
	return [expr {[info which $cmd] != ""}]
    }
    if ![info exists auto_path] {
	return 0
    }
    if [info exists auto_oldpath] {
	if {$auto_oldpath == $auto_path} {
	    return 0
	}
    }
    set auto_oldpath $auto_path
    for {set i [expr [llength $auto_path] - 1]} {$i >= 0} {incr i -1} {
	set dir [lindex $auto_path $i]
	set f ""
	if [catch {set f [open [file join $dir tclIndex]]}] {
	    continue
	}
	set error [catch {
	    set id [gets $f]
	    if {$id == "# Tcl autoload index file, version 2.0" ||
		$id == "# Tcl autoload index file, version 2.0 for \[incr Tcl\]"} {
		eval [read $f]
	    } elseif {$id == "# Tcl autoload index file: each line identifies a Tcl"} {
		while {[gets $f line] >= 0} {
		    if {([string index $line 0] == "#")
			    || ([llength $line] != 2)} {
			continue
		    }
		    set name [lindex $line 0]
		    set auto_index($name) \
			"source [file join $dir [lindex $line 1]]"
		}
	    } else {
		error "[file join $dir tclIndex] isn't a proper Tcl index file"
	    }
	} msg]
	if {$f != ""} {
	    close $f
	}
	if $error {
	    error $msg $errorInfo $errorCode
	}
    }

    if {[info exists auto_index($cmd)]} {
        set loadcmd $auto_index($cmd)
    } else {
        foreach nsref [uplevel import all] {
            set ns [lindex $nsref 0]
			if {$ns == "::"} {
            	set fullname "::$cmd"
			} else {
            	set fullname "$ns::$cmd"
			}
            if {[info exists auto_index($fullname)]} {
                set loadcmd $auto_index($fullname)
                break
            }
        }
    }
    if {[info exists loadcmd]} {
	uplevel #0 $loadcmd
	return [expr {[info which $cmd] != ""}]
    }
    return 0
}

if {[string compare $tcl_platform(platform) windows] == 0} {

# auto_execok --
#
# Returns string that indicates name of program to execute if 
# name corresponds to a shell builtin or an executable in the
# Windows search path, or "" otherwise.  Builds an associative 
# array auto_execs that caches information about previous checks, 
# for speed.
#
# Arguments: 
# name -			Name of a command.

# Windows version.
#
# Note that info executable doesn't work under Windows, so we have to
# look for files with .exe, .com, or .bat extensions.  Also, the path
# may be in the Path or PATH environment variables, and path
# components are separated with semicolons, not colons as under Unix.
#
proc auto_execok name {
    global auto_execs env tcl_platform

    if [info exists auto_execs($name)] {
	return $auto_execs($name)
    }
    set auto_execs($name) ""

    if {[lsearch -exact {cls copy date del erase dir echo mkdir md rename 
	    ren rmdir rd time type ver vol} $name] != -1} {
	if {[info exists env(COMSPEC)]} {
	    set comspec $env(COMSPEC) 
	} elseif {[info exists env(ComSpec)]} {
	    set comspec $env(ComSpec)
	} elseif {$tcl_platform(os) == "Windows NT"} {
	    set comspec "cmd.exe"
	} else {
	    set comspec "command.com"
	}
	return [set auto_execs($name) [list $comspec /c $name]]
    }

    if {[llength [file split $name]] != 1} {
	foreach ext {{} .com .exe .bat} {
	    set file ${name}${ext}
	    if {[file exists $file] && ![file isdirectory $file]} {
		return [set auto_execs($name) $file]
	    }
	}
	return ""
    }

    set path "[file dirname [info nameof]];.;"
    if {[info exists env(WINDIR)]} {
	set windir $env(WINDIR) 
    } elseif {[info exists env(windir)]} {
	set windir $env(windir)
    }
    if {[info exists windir]} {
	if {$tcl_platform(os) == "Windows NT"} {
	    append path "$windir/system32;"
	}
	append path "$windir/system;$windir;"
    }

    if {! [info exists env(PATH)]} {
	if [info exists env(Path)] {
	    append path $env(Path)
	} else {
	    return ""
	}
    } else {
	append path $env(PATH)
    }

    foreach dir [split $path {;}] {
	if {$dir == ""} {
	    set dir .
	}
	foreach ext {{} .com .exe .bat} {
	    set file [file join $dir ${name}${ext}]
	    if {[file exists $file] && ![file isdirectory $file]} {
		return [set auto_execs($name) $file]
	    }
	}
    }
    return ""
}

} else {

# auto_execok --
#
# Returns string that indicates name of program to execute if 
# name corresponds to an executable in the path. Builds an associative 
# array auto_execs that caches information about previous checks, 
# for speed.
#
# Arguments: 
# name -			Name of a command.

# Unix version.
#
proc auto_execok name {
    global auto_execs env

    if [info exists auto_execs($name)] {
	return $auto_execs($name)
    }
    set auto_execs($name) ""
    if {[llength [file split $name]] != 1} {
	if {[file executable $name] && ![file isdirectory $name]} {
	    set auto_execs($name) $name
	}
	return $auto_execs($name)
    }
    foreach dir [split $env(PATH) :] {
	if {$dir == ""} {
	    set dir .
	}
	set file [file join $dir $name]
	if {[file executable $file] && ![file isdirectory $file]} {
	    set auto_execs($name) $file
	    return $file
	}
    }
    return ""
}

}
# auto_reset --
# Destroy all cached information for auto-loading and auto-execution,
# so that the information gets recomputed the next time it's needed.
# Also delete any procedures that are listed in the auto-load index
# except those defined in this file.
#
# Arguments: 
# None.

proc auto_reset {} {
    global auto_execs auto_index auto_oldpath
    foreach p [info procs] {
	if {[info exists auto_index($p)] && ![string match auto_* $p]
		&& ([lsearch -exact {unknown pkg_mkIndex tclPkgSetup
			tclPkgUnknown} $p] < 0)} {
	    rename $p {}
	}
    }
    catch {unset auto_execs}
    catch {unset auto_index}
    catch {unset auto_oldpath}
}

# auto_mkindex --
# Regenerate a tclIndex file from Tcl source files.  Takes as argument
# the name of the directory in which the tclIndex file is to be placed,
# followed by any number of glob patterns to use in that directory to
# locate all of the relevant files.
#
# Arguments: 
# dir -			Name of the directory in which to create an index.
# args -		Any number of additional arguments giving the
#			names of files within dir.  If no additional
#			are given auto_mkindex will look for *.tcl.

proc auto_mkindex {dir args} {
    global errorCode errorInfo
    set oldDir [pwd]
    cd $dir
    set dir [pwd]

    global ::tcl::mkindex-parser::index
    set index ""

    append index "# Tcl autoload index file, version 2.0 for \[incr Tcl\]\n"
    append index "# This file is generated by the \"auto_mkindex\" command\n"
    append index "# and sourced to set up indexing information for one or\n"
    append index "# more commands.  Typically each line is a command that\n"
    append index "# sets an element in the auto_index array, where the\n"
    append index "# element name is the name of a command and the value is\n"
    append index "# a script that loads the command.\n\n"
    if {$args == ""} {
	set args *.tcl
    }
    foreach file [eval glob $args] {
        if {[catch {tcl::mkindex-parser::mkindex $file} msg] != 0} {
            set code $errorCode
            set info $errorInfo
            cd $oldDir
            error $msg $info $code
        }
    }

    set fid [open tclIndex w]
    puts $fid $index nonewline
    close $fid
    cd $oldDir
}

#
# Create a namespace that can be used to parse Tcl source files to
# generate a tclIndex file for autoloading.  This namespace contains
# commands for things that need index entries.  Each time a command
# is executed, it writes an entry out to the index file.
#
# An isolated namespace is set up to filter incoming commands.
# Those that aren't registered in the "mkindex-parser" namespace
# are simply ignored.
#
namespace ::tcl::mkindex-parser {
    ::public variable index ""          ;# maintains index as it is built
    ::protected variable scriptFile ""  ;# name of file being processed
    ::protected variable scopeStack ""  ;# stack of namespace scopes

    # --------------------------------------------------------------------
    # USAGE:  mkindex <fileName>
    #
    # Scans Tcl code from the specified <fileName> and adds entries
    # into the global "index" variable for elements that are recognized
    # by the autoloader.
    # --------------------------------------------------------------------
    ::public ::proc mkindex {file} {
        global scriptFile
        set scriptFile $file

        set fid [open $file]
        set contents [read $fid]
        close $fid

        #
        # There is one problem with sourcing files into the isolated
        # namespace:  references like "$x" will fail since code is not
        # really being executed and variables do not really exist.
        # Be careful to escape all naked "$" before evaluating.
        #
        regsub -all {([^\$])\$([^\$])} $contents {\1\\$\2} contents
        ::namespace ::tcl::mkindex-parser::isolated $contents
    }

    #
    # Set up an isolated namespace to filter incoming commands.
    # Put the "enforce_cmd" proc in the parent namespace, so commands
    # within it work with some sanity.
    #
    ::proc enforce_cmd {name} {
        global commands
        if {[info exists commands($name)]} {
            return $commands($name)
        }
        return "mkindex_ignore"
    }
    ::namespace isolated -local -enforced yes

    #
    # Set up a command registry.  All commands that require an
    # tclIndex entry, and all possible namespace paths for these
    # commands, should be registered in this array.
    #
    protected variable commands

    #
    # Most commands get mapped to "mkindex_ignore" and are ignored.
    #
    ::proc mkindex_ignore {args} {}

    #
    # HANDLE:  namespace name ... {commands...}
    #
    ::proc namespace {name args} {
        global scopeStack
        set scopeStack [linsert $scopeStack 0 $name]

        set cmds [lindex $args end]
        ::namespace isolated $cmds

        set scopeStack [lrange $scopeStack 1 end]
    }
    set commands(namespace) namespace
    set commands(::namespace) namespace

    #
    # USAGE:  mkindex_path <name>
    #
    # Returns the complete namespace path for the specified <name>.
    # If <name> starts with "::", then it is returned directly.
    # Otherwise, elements from the current namespace stack are added
    # until the complete path starts with "::", or until all elements
    # have been added.
    #
    ::protected ::proc mkindex_path {name} {
        global scopeStack
        if {[string match ::* $name]} {
            return $name
        }
        foreach ns $scopeStack {
            set name "$ns::$name"
            if {[string match ::* $name]} {
                return $name
            }
        }
        return "::$name"
    }

    #
    # HANDLE:  ensemble name body
    #
    ::proc ensemble {name body} {
        global index scriptFile
        set name [mkindex_path $name]
        append index "set [list auto_index($name)]"
        append index " \[list source \[file join \$dir [list $scriptFile]\]\]\n"
    }
    set commands(ensemble) ensemble
    set commands(::ensemble) ensemble

    #
    # HANDLE:  public ...
    #          protected ...
    #          private ...
    #
    ::proc plevel {args} {
        if {[llength $args] == 1} {
            ::namespace isolated [lindex $args 0]
        } else {
            ::namespace isolated $args
        }
    }
    set commands(public) plevel
    set commands(::public) plevel
    set commands(protected) plevel
    set commands(::protected) plevel
    set commands(private) plevel
    set commands(::private) plevel

    #
    # HANDLE:  proc name ?arglist? ?body?
    #   NOTE:  Within a class, the arglist and body are optional
    #          for proc declarations.  This same proc command
    #          handles both cases.
    #
    ::proc proc {name {arglist ""} {body ""}} {
        global index scriptFile
        set name [mkindex_path $name]
        append index "set [list auto_index($name)]"
        append index " \[list source \[file join \$dir [list $scriptFile]\]\]\n"
    }
    set commands(proc) proc
    set commands(::proc) proc
}

# pkg_mkIndex --
# This procedure creates a package index in a given directory.  The
# package index consists of a "pkgIndex.tcl" file whose contents are
# a Tcl script that sets up package information with "package require"
# commands.  The commands describe all of the packages defined by the
# files given as arguments.
#
# Arguments:
# dir -			Name of the directory in which to create the index.
# args -		Any number of additional arguments, each giving
#			a glob pattern that matches the names of one or
#			more shared libraries or Tcl script files in
#			dir.

proc pkg_mkIndex {dir args} {
    global errorCode errorInfo
    append index "# Tcl package index file, version 1.0\n"
    append index "# This file is generated by the \"pkg_mkIndex\" command\n"
    append index "# and sourced either when an application starts up or\n"
    append index "# by a \"package unknown\" script.  It invokes the\n"
    append index "# \"package ifneeded\" command to set up package-related\n"
    append index "# information so that packages will be loaded automatically\n"
    append index "# in response to \"package require\" commands.  When this\n"
    append index "# script is sourced, the variable \$dir must contain the\n"
    append index "# full path name of this file's directory.\n"
    set oldDir [pwd]
    cd $dir
    foreach file [eval glob $args] {
	# For each file, figure out what commands and packages it provides.
	# To do this, create a child interpreter, load the file into the
	# interpreter, and get a list of the new commands and packages
	# that are defined.  Define an empty "package unknown" script so
	# that there are no recursive package inclusions.

	set c [interp create]

	# If Tk is loaded in the parent interpreter, load it into the
	# child also, in case the extension depends on it.

	foreach pkg [info loaded] {
	    if {[lindex $pkg 1] == "Tk"} {
		$c eval {set argv {-geometry +0+0}}
		load [lindex $pkg 0] Tk $c
		break
	    }
	}
	$c eval [list set file $file]
	if [catch {
	    $c eval {
		proc dummy args {}
		package unknown dummy
		set origCmds [info commands]
		set dir ""		;# in case file is pkgIndex.tcl
		set pkgs ""

		# Try to load the file if it has the shared library extension,
		# otherwise source it.  It's important not to try to load
		# files that aren't shared libraries, because on some systems
		# (like SunOS) the loader will abort the whole application
		# when it gets an error.

		if {[string compare [file extension $file] \
			[info sharedlibextension]] == 0} {

		    # The "file join ." command below is necessary.  Without
		    # it, if the file name has no \'s and we're on UNIX, the
		    # load command will invoke the LD_LIBRARY_PATH search
		    # mechanism, which could cause the wrong file to be used.

		    load [file join . $file]
		    set type load
		} else {
		    source $file
		    set type source
		}
		foreach i [info commands] {
		    set cmds($i) 1
		}
		foreach i $origCmds {
		    catch {unset cmds($i)}
		}
		foreach i [package names] {
		    if {([string compare [package provide $i] ""] != 0)
			    && ([string compare $i Tcl] != 0)
			    && ([string compare $i Tk] != 0)} {
			lappend pkgs [list $i [package provide $i]]
		    }
		}
	    }
	} msg] {
	    puts "error while loading or sourcing $file: $msg"
	}
	foreach pkg [$c eval set pkgs] {
	    lappend files($pkg) [list $file [$c eval set type] \
		    [lsort [$c eval array names cmds]]]
	}
	interp delete $c
    }
    foreach pkg [lsort [array names files]] {
	append index "\npackage ifneeded $pkg\
		\[list tclPkgSetup \$dir [lrange $pkg 0 0] [lrange $pkg 1 1]\
		[list $files($pkg)]\]"
    }
    set f [open pkgIndex.tcl w]
    puts $f $index
    close $f
    cd $oldDir
}

# tclPkgSetup --
# This is a utility procedure use by pkgIndex.tcl files.  It is invoked
# as part of a "package ifneeded" script.  It calls "package provide"
# to indicate that a package is available, then sets entries in the
# auto_index array so that the package's files will be auto-loaded when
# the commands are used.
#
# Arguments:
# dir -			Directory containing all the files for this package.
# pkg -			Name of the package (no version number).
# version -		Version number for the package, such as 2.1.3.
# files -		List of files that constitute the package.  Each
#			element is a sub-list with three elements.  The first
#			is the name of a file relative to $dir, the second is
#			"load" or "source", indicating whether the file is a
#			loadable binary or a script to source, and the third
#			is a list of commands defined by this file.

proc tclPkgSetup {dir pkg version files} {
    global auto_index

    package provide $pkg $version
    foreach fileInfo $files {
	set f [lindex $fileInfo 0]
	set type [lindex $fileInfo 1]
	foreach cmd [lindex $fileInfo 2] {
	    if {$type == "load"} {
		set auto_index($cmd) [list load [file join $dir $f] $pkg]
	    } else {
		set auto_index($cmd) [list source [file join $dir $f]]
	    } 
	}
    }
}

# tclMacPkgSearch --
# The procedure is used on the Macintosh to search a given directory for files
# with a TEXT resource named "pkgIndex".  If it exists it is sourced in to the
# interpreter to setup the package database.
#
# Addition for Itcl: JCI -
# I added a STR resource whose name is Itcl Marker, and I will only look at
# libraries that bear this marker.  This is to avoid conflicts with the Sun
# distribution.

proc tclMacPkgSearch {dir} {
    foreach x [glob -nocomplain [file join $dir *.shlb]] {
	if [file isfile $x] {
	    set res [resource open $x]
	    if { [catch {resource getSTR {Itcl Marker}}] } {
	    	return
	    }
	    foreach y [resource list TEXT $res] {
		if {$y == "pkgIndex"} {source -rsrc pkgIndex}
	    }
	    resource close $res
	}
    }
}

# tclPkgUnknown --
# This procedure provides the default for the "package unknown" function.
# It is invoked when a package that's needed can't be found.  It scans
# the auto_path directories and their immediate children looking for
# pkgIndex.tcl files and sources any such files that are found to setup
# the package database.  (On the Macintosh we also search for pkgIndex
# TEXT resources in all files.)
#
# Arguments:
# name -		Name of desired package.  Not used.
# version -		Version of desired package.  Not used.
# exact -		Either "-exact" or omitted.  Not used.

proc tclPkgUnknown {name version {exact {}}} {
    global auto_path tcl_platform env

    if ![info exists auto_path] {
	return
    }
    for {set i [expr [llength $auto_path] - 1]} {$i >= 0} {incr i -1} {
	set dir [lindex $auto_path $i]
	set file [file join $dir pkgIndex.tcl]
	if [file readable $file] {
	    source $file
	}
	foreach file [glob -nocomplain [file join $dir * pkgIndex.tcl]] {
	    if [file readable $file] {
		set dir [file dirname $file]
		source $file
	    }
	}
	# On the Macintosh we also look in the resource fork 
	# of shared libraries
	if {$tcl_platform(platform) == "macintosh"} {
	    set dir [lindex $auto_path $i]
	    tclMacPkgSearch $dir
	    foreach x [glob -nocomplain [file join $dir *]] {
		if [file isdirectory $x] {
		    set dir $x
		    tclMacPkgSearch $dir
		}
	    }
	}
    }

    # JCI --
    # There is one more case we have to cover:
    #    The pkgIndex file might be in the resource fork of the application.
    #    For instance, I include the Iwidgets in the resource fork of
    #    Itkwish, and so I have to put the pkgIndex there as well, or the
    #    "package require Iwidgets" will not work.  
    #    where all the libraries are bound in.
    #
    #    You still need a package ifneeded of the form
    #    "load {} Libname", or package require will not work.
    #    In this case, you want to make sure NOT to open the pkgIndex resources
    #    from the shlb's, so I require the ones in the application be named
    #    itk:pkgIndex or whatever...
    #
    if {$tcl_platform(platform) == "macintosh"} {
    
        foreach x [resource list TEXT] {
            if {[string first ":pkgIndex" $x] > -1 } {
                source -rsrc $x
            }
        }
        
    }
}
