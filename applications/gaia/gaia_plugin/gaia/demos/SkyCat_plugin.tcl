#-*-tcl-*-
# E.S.O. - VLT project/ESO Archive
# $Id: SkyCat_plugin.tcl,v 1.14 1999/02/04 12:03:57 abrighto Exp $
#
# Skycat_plugin.tcl - plugin script to add GAIA features to Skycat
#
# To use this file for skycat, set the environment variable SKYCAT_PLUGIN 
# to the path name of this file, this file's directory or parent directory.
# 
# This file must define at least one tcl proc: SkyCat_plugin, which
# takes one argument: the name of the top level skycat widget.  
#
# This plugin adds the grid feature from GAIA (Starlink) to skycat.
#
# See the documentation, man pages and Itcl widget source code for details.
#
# who         when       what
# --------   ---------   ----------------------------------------------
# A.Brighton 13 Mar 98   created
#

# avoid loading more than once
if {[info exists gaia_initialized_]} {
    return
}
set gaia_initialized_ 1

puts "Loading the Gaia plugin..."

# get dir containing this script
set dir [file dirname [info script]]

# on HP, we need to add this dir to the SHLIB_PATH so that libskycat.sl
# will be found.
if {[info exists env(SHLIB_PATH)]} {
    set env(SHLIB_PATH) "$env(SHLIB_PATH):$dir"
} else {
    set env(SHLIB_PATH) $dir
}

# load the GAIA package dynamically, if it is not compiled in
if {[catch {package require Gaia} msg]} {
    puts "error loading GAIA package: $msg"
    return
} else {
    # replace the main application class with a derived class
    set mainclass Gaia
}

# when used as a plugin, set the gaia_library variable to point to
# the plugin dir
if {[file exists $dir/Gaia.tcl]} {
    set gaia_library $dir

    # we need this for the local iwidgets files
    set env(IWIDGETS_LIBRARY) [lindex [glob $dir/iwidgets*] 0]
    lappend auto_path $env(IWIDGETS_LIBRARY)/scripts
    package require Iwidgets
} 

# set up the STARLINK environment
gaia::Gaia::setup_starlink_env $dir


# This proc is called once for each skycat instance. The parameter is 
# the name ("$this") of the SkyCat Itcl class object.
# (Not used here).

proc SkyCat_plugin {this} {
    # get the toplevel widget name
    #set w [info namespace tail $this]
}

