# E.S.O. - VLT project
# $Id 
#
# astroimage.tcl - standalone application for the AstroImage widget
#
# Usage: astroimage ?options?
#    or: cat_wish astroimage.tcl ?options?
#
# where ?options? are the itk_options listed toward the end of this file 
# and talke the form: -option value ...`
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 20 May 95   created


set astroimage_usage "
Usage: astroimage ?localCatalog? ?-option value ...?

Options:
	-imagesvr name   - set default catalog (must be in config file)
        -tmpfile name    - set temp file to use for image
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

# the environment variable $CAT_LIBRARY should point to the directory
# containing the Tcl library files
if {[info exists env(CAT_LIBRARY)]} {
    set cat_library $env(CAT_LIBRARY)
} else {
    set cat_library ../library
}

# RTD_LIBRARY has a similar meaning for the rtd module 
if {[info exists env(RTD_LIBRARY)]} {
    set rtd_library $env(RTD_LIBRARY)
} else {
    set rtd_library ../../rtd/library
    if {! [file isdirectory $rtd_library]} {
	set rtd_library ../../../rtd/rtdimg/library
	if {! [file isdirectory $rtd_library]} {
	    puts "can't find RTD Tcl library files, try setting RTD_LIBRARY env variable"
	    exit 1
	}
    }
}

tk appname AstroImage
set tk_strictMotif 0
    
# make sure the tcl library files are found
set auto_path "$cat_library $rtd_library $auto_path"
utilPrintErrors

# add blt library to auto_path
if {[info exists env(BLT_LIBRARY)]} {
    lappend auto_path $env(BLT_LIBRARY)
}

# load the Rtd image extension from a shared library if not already linked in
if {[llength [info commands rtd_set_cmap]] == 0} {
    if {[catch {load $env(RTD_SHLIB)} msg]} {
	puts "error loading RTD extension: $msg"
	exit 1
    }
}

# load the Catlib extension from a shared library if not already linked in
if {[llength [info commands astroimage]] == 0} {
    if {[catch {load $env(CAT_SHLIB)} msg]} {
	puts "error loading Catlib extension: $msg"
	exit 1
    }
}

lappend argv -center 0
incr argc 2

# start the application
util::TopLevelWidget::start AstroImage "-imagesvr" "$astroimage_usage"

