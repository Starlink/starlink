# E.S.O. - VLT project
# $Id: skycat.tcl,v 1.16 1998/10/28 17:44:33 abrighto Exp $ 
#
# skycat.tcl - image display application with catalog support 
#              (AstroCatalog classes)
#
# Usage: skycat ?options? (using link to skycat.sh script)
#    or: skycat_wish skycat.tcl ?options?
#
# where ?options? are the itk_options listed toward the end of this file 
# and take the form: -option value ...`
#
# See man page skycat(1) for a complete description.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 20 May 95   created


# Tk intercepts the -help option
if {"[lindex $argv 0]" == "-help"} {
    puts " --help:    prints a complete list of skycat options"
    return
}

# The startup script skycat.sh(.in) sets these environment variables to
# point to the tcl/tk script dirs. We need to know this mainly when
# loading packages dynamically from shared libraries, since the
# auto_path tcl variable determines the search path for the pkgIndex.tcl
# files used to load the shared libraries at run time.
foreach pkg {BLT RTD CAT SKYCAT} {
    if {[info exists env(${pkg}_LIBRARY)]} {
	lappend auto_path $env(${pkg}_LIBRARY)
    }
}

# load the required packages, if it is not already loaded
foreach pkg {Rtd Cat Skycat} {
    if {[catch {package require $pkg} msg]} {
	puts "error loading $pkg package: $msg"
	exit 1
    }
}

# print errors also on stderr
utilPrintErrors

# create a dummy rtdimage to make sure the colormap is initialized before 
# the logo is created to avoid color problems...
# catch {image delete [image create rtdimage]}

# start the application
skycat::SkyCat::startSkyCat

