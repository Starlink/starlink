#  Package initialisation script.

#  Pick up GAIA scripts before other packages that we depend on.
package require Tclx
if {![lcontain $auto_path $gaia_library]} {
    lappend auto_path $gaia_library
}

#  Packages that we depend on.
package require Skycat
package require Iwidgets

namespace eval gaia {namespace export *}
namespace import -force gaia::*
