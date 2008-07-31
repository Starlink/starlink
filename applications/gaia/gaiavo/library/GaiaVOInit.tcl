#  Package initialisation script.

#  Pick up GAIAVO scripts before other packages that we depend on,
#  or not, as required.
if {![lcontain $auto_path $gaiavo_library]} {
    lappend auto_path $gaiavo_library
}

#  Packages that we depend on. Name these upfront.
package require Gaia
package require dict
package require tdom
package require http
package require log
package require uri

namespace eval gaiavo {namespace export *}
namespace import -force gaiavo::*
