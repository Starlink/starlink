# E.S.O. - VLT project
# $Id 
#
# astrocat.tcl - standalone application for the AstroCat widget
#
# Usage: astrocat ?options?
#    or: cat_wish astrocat.tcl ?options?
#
# where ?options? are the same as the widget options in the main itcl
# class "AstroCat" ane take the form: -option value ...`
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 20 May 95   created


set astrocat_usage "
Usage: astrocat ?localCatalog? ?-option value ...?

Options:
	-catalog name   - set default catalog (must be in config file)
        -tmpfile name   - set temp file to use for preview image
"

# where to look for catalog config file: 
# use ~/.skycat/skycat.cfg if it exists, since it may contain user's 
# preferences, otherwise use $SKYCAT_CONFIG if set, or $CATLIB_CONFIG.
set config_file $env(HOME)/.skycat/skycat.cfg
if {[file exists $config_file]} {
    set env(CATLIB_CONFIG) "file:$config_file"
} elseif {[info exists env(SKYCAT_CONFIG)]} {
    set env(CATLIB_CONFIG) $env(SKYCAT_CONFIG)
} 

# The startup script cat.sh(.in) sets these environment variables to
# point to the tcl/tk script dirs. We need to know this mainly when
# loading packages dynamically from shared libraries, since the
# auto_path tcl variable determines the search path for the pkgIndex.tcl
# files used to load the shared libraries at run time.
foreach pkg {BLT CAT} {
    if {[info exists env(${pkg}_LIBRARY)]} {
	lappend auto_path $env(${pkg}_LIBRARY)
    }
}

tk appname AstroCat
set tk_strictMotif 0
    
# load the required packages, if it is not already loaded
foreach pkg {Cat} {
    if {[catch {package require $pkg} msg]} {
	puts "error loading $pkg package: $msg"
	exit 1
    }
}

# print errors also on stderr
utilPrintErrors

# don't center the main window
lappend argv -center 0
incr argc 2

# start the application
util::TopLevelWidget::start AstroCat "-catalog" "$astrocat_usage"

