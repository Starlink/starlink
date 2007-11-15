#  Package initialisation script for Tcl-only commands.

#  Pick up GAIA scripts before other packages that we depend on,
#  or not, as required.
package require Tclx
if {![lcontain $auto_path $gaia_library]} {
    lappend auto_path $gaia_library
}
