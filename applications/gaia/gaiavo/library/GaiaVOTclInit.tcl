#  Package initialisation script. Tcl only version. No Tk dependencies.

#  Pick up GAIAVO scripts before other packages that we depend on,
#  or not, as required.
if { [lsearch -exact auto_path $gaiavo_library] == -1 } {
    lappend auto_path $gaiavo_library
}

#  Packages that we depend on. Name these upfront
package require tdom
package require http
package require log
package require uri

