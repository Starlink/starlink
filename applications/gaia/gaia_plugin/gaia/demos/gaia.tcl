# E.S.O. - VLT project
# $Id: gaia.tcl,v 1.2 1998/04/20 20:23:21 abrighto Exp $ 
#
# gaia.tcl - image display application with catalog support
#            and additional features taken from Starlink's
#            GAIA application. 
#
# Usage: gaia ?options? (using link to gaia.sh script)
#    or: gaia_wish gaia.tcl ?options?
#
# where ?options? are the itk_options listed toward the end of this file 
# and take the form: -option value ...`
#
# See man page gaia(1) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 20 May 95   created


# The startup script gaia.sh(.in) sets these environment variables to
# point to the tcl/tk script dirs. We need to know this mainly when
# loading packages dynamically from shared libraries, since the
# auto_path tcl variable determines the search path for the pkgIndex.tcl
# files used to load the shared libraries at run time.
foreach pkg {BLT RTD CAT SKYCAT GAIA} {
    if {[info exists env(${pkg}_LIBRARY)]} {
	lappend auto_path $env(${pkg}_LIBRARY)
    }
}

# add blt namespace to the import list in the current namespace context
import add ::blt

# load the required packages, if it is not already loaded
foreach pkg {Rtd Cat Skycat Gaia} {
    if {[catch {package require $pkg} msg]} {
	puts "error loading $pkg package: $msg"
	exit 1
    }
}

# print errors also on stderr
utilPrintErrors

# add the GAIA plugin. Normally this would be done dynamically
# by loading it from a shared library, however for testing and
# debugging, it is easier to have things linked in the normal way.
if {[info exists env(SKYCAT_PLUGIN)]} {
    set env(SKYCAT_PLUGIN) "${gaia_library}:$env(SKYCAT_PLUGIN)"
} else {
    set env(SKYCAT_PLUGIN) "$gaia_library"
}

# start the application
SkyCat::startSkyCat

