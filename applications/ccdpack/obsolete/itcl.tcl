#
# itcl.tcl
# ----------------------------------------------------------------------
# Invoked automatically upon startup to customize the interpreter
# for [incr Tcl].
# ----------------------------------------------------------------------
#   AUTHOR:  Michael J. McLennan
#            Bell Labs Innovations for Lucent Technologies
#            mmclennan@lucent.com
#            http://www.tcltk.com/itcl
#
#      RCS:  $Id$
# ----------------------------------------------------------------------
#               Copyright (c) 1993-1996  Lucent Technologies
# ======================================================================
# Permission to use, copy, modify, and distribute this software and its
# documentation for any purpose and without fee is hereby granted,
# provided that the above copyright notice appear in all copies and that
# both that the copyright notice and warranty disclaimer appear in
# supporting documentation, and that the names of Lucent Technologies
# any of their entities not be used in advertising or publicity
# pertaining to distribution of the software without specific, written
# prior permission.
#
# Lucent Technologies disclaims all warranties with regard to this
# software, including all implied warranties of merchantability and
# fitness.  In no event shall Lucent be liable for any special, indirect
# or consequential damages or any damages whatsoever resulting from loss
# of use, data or profits, whether in an action of contract, negligence
# or other tortuous action, arising out of or in connection with the use
# or performance of this software.
# ======================================================================

#
# Provide transparent access to all [incr Tcl] commands
#
import add ::itcl

#
# Add commands to the "mkindex-parser" namespace to support the
# generation of "tclIndex" files for the autoloading facility.
#
namespace ::tcl::mkindex-parser {
    #
    # HANDLE:  class name {definition...}
    #
    ::proc class {name defn} {
        global index scriptFile
        set name [mkindex_path $name]
        append index "set [list auto_index($name)]"
	append index " \[list source \[file join \$dir [list $scriptFile]\]\]\n"
        namespace $name $defn
    }
    set commands(class) class
    set commands(itcl::class) class
    set commands(::itcl::class) class
    set commands(itcl_class) class
    set commands(::itcl_class) class

    #
    # HANDLE:  body name arglist body
    #
    ::proc body {name arglist body} {
        global index scriptFile
        set name [mkindex_path $name]
        append index "set [list auto_index($name)]"
	append index " \[list source \[file join \$dir [list $scriptFile]\]\]\n"
    }
    set commands(body) body
    set commands(itcl::body) body
    set commands(::itcl::body) body

    #
    # HANDLE:  configbody name body
    #
    ::proc configbody {name body} {
        global index scriptFile
        set name [mkindex_path $name]
        append index "set [list auto_index($name)]"
	append index " \[list source \[file join \$dir [list $scriptFile]\]\]\n"
    }
    set commands(configbody) configbody
    set commands(itcl::configbody) configbody
    set commands(::itcl::configbody) configbody
}


# ----------------------------------------------------------------------
#  USAGE:  local <className> <objName> ?<arg> <arg>...?
#
#  Creates a new object called <objName> in class <className>, passing
#  the remaining <arg>'s to the constructor.  Unlike the usual
#  [incr Tcl] objects, however, an object created by this procedure
#  will be automatically deleted when the local call frame is destroyed.
#  This command is useful for creating objects that should only remain
#  alive until a procedure exits.
# ----------------------------------------------------------------------
public proc ::itcl::local {class name args} {
    set ptr [uplevel eval $class $name $args]
    uplevel [list set itcl-local-$ptr $ptr]
    uplevel [list trace variable itcl-local-$ptr u "delete object $ptr"]
    return $ptr
}
