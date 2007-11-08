#  Package initialisation script.

#  Pick up GAIA scripts before other packages that we depend on,
#  or not, as required.
package require Tclx
if {![lcontain $auto_path $gaia3d_library]} {
    lappend auto_path $gaia3d_library
}

#  Packages that we depend on.
package require Gaia
package require vtk

namespace eval gaia3d {namespace export *}
namespace import -force gaia3d::*
