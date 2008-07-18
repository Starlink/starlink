#  Package initialisation script.

#  Pick up GAIAVO scripts before other packages that we depend on,
#  or not, as required.
if {![lcontain $auto_path $gaiavo_library]} {
    lappend auto_path $gaiavo_library
}

#  Packages that we depend on.
package require Gaia

namespace eval gaiavo {namespace export *}
namespace import -force gaiavo::*
