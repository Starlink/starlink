# E.S.O. - VLT project
# $Id: gaia.tcl,v 1.8 1999/02/03 20:14:36 abrighto Exp $ 
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
foreach pkg {BLT SKYCAT GAIA} {
    if {[info exists env(${pkg}_LIBRARY)]} {
	lappend auto_path $env(${pkg}_LIBRARY)
    }
}

# load the required packages, if it is not already loaded
foreach pkg {Skycat Gaia} {
    if {[catch {package require $pkg} msg]} {
	puts "error loading $pkg package: $msg"
	exit 1
    }
}

# print errors also on stderr
utilPrintErrors

# start the application
gaia::Gaia::startGaia
