#-*-tcl-*-
# E.S.O. - VLT project/ESO Archive
# $Id: SkyCat_plugin.tcl,v 1.8 1998/05/06 08:32:26 abrighto Exp $
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

# add blt namespace to the import list in the current namespace context
if {[catch {import add ::blt} msg]} {
    puts $msg
    return
}

# load the GAIA package dynamically, if it is not compiled in
if {[catch {package require Gaia} msg]} {
    puts "error loading GAIA package: $msg"
} else {
    # replace the main application class with a derived class
    set mainclass Gaia
}

# when used as a plugin, set the gaia_library variable to point to
# the plugin dir
set dir [file dirname [info script]]
if {[file exists $dir/Gaia.tcl]} {
    set gaia_library $dir

    # we need this for the local iwidgets files
    set env(IWIDGETS_LIBRARY) [lindex [glob $dir/iwidgets*] 0]
    package require Iwidgets
    catch {
	namespace ::iwidgets
	import add ::iwidgets
    }

    # we need this for the local atclsh binary for running external tcl commands
    set env(TCL_LIBRARY) $dir/tcl7.6
    
    # Check if using local Starlink binaries
    if {[file exists $dir/autophotom]} {
	set env(PHOTOM_DIR) $dir
    }
    if {[file exists $dir/ardmask]} {
	set env(KAPPA_DIR) $dir
    }
} 

if {[info exists env(STARLINK)]} {
    # try to use Starlink installation, if available
    set dir $env(STARLINK)
    if {![info exists env(PHOTOM_DIR)] && [file exists $dir/bin/photom/autophotom]} {
	set env(PHOTOM_DIR) $dir/bin/photom
    }
    if {![info exists env(KAPPA_DIR)] && [file exists $dir/bin/kappa/ardmask]} {
	set env(KAPPA_DIR) $dir/bin/kappa
    }
    # alternate dirs
    if {[file exists $dir/bin/gaiapack/ardmask]} {
	set env(KAPPA_DIR) $dir/bin/gaiapack
    }
    if {[file exists $dir/bin/gaiapack/autophotom]} {
	set env(PHOTOM_DIR) $dir/bin/gaiapack
    }
    if {![info exists env(CONVERT_DIR)] && [file exists $dir/bin/convert/ndf2fits]} {
	set env(CONVERT_DIR) $dir/bin/convert
    }
} 

# add the plugin bin dir to the path
set env(PATH) "${dir}/bin:$env(PATH)"

# See if the user already sourced convert.csh
if {! [info exists env(NDF_FORMATS_IN)] && [info exists env(CONVERT_DIR)]} {
    # Define environment variables (normally done in convert.csh)
    setup_convert
}
# Set up CONVERT to work for .fits file as well as .fit.
if {[info exists env(NDF_FORMATS_IN)]} {
    append env(NDF_FORMATS_IN) ",FITS(.fits),FITS(.fit)"
    append env(NDF_FORMATS_OUT) ",FITS(.fits),FITS(.fit)"
}

#  Extract the known file types and set these up as defaults. These
#  are entered as if command-line arguments so that they propagate
#  to clone windows.
set file_types {{any *} {NDF(.sdf) *.sdf} {FIT(.fit) *.fit} {FITS(fits) *.fits}}
if { [info exists env(NDF_FORMATS_IN)] } {
   set new_types [split $env(NDF_FORMATS_IN) ","]
   foreach pair $new_types {
      regexp {([^\(]*).([^\)]*)} $pair dummy name type
      if { $name != "NDF" && $type != ".fits" && $type != ".fit" } {
         lappend file_types [list $name\($type\) *${type}]
      }
   }
}
lappend argv "-file_types"
lappend argv $file_types
incr argc 2

# need this for StarApp.tcl
if {! [info exists env(TCLADAM_DIR)]} {
    set env(TCLADAM_DIR) $gaia_library/demos
}
# XXX should use ~/adam/gaia-[pid] here, but not sure about cleanup...
if {! [info exists env(ADAM_USER)]} {
    set env(ADAM_USER) $env(HOME)/adam/gaia
}
if {[file isdirectory $env(ADAM_USER)]} {
    exec rm -rf $env(ADAM_USER)
}

# This proc is called once for each skycat instance. The parameter is 
# the name ("$this") of the SkyCat Itcl class object.
# (Not used here).

proc SkyCat_plugin {this} {
    # get the toplevel widget name
    #set w [info namespace tail $this]
}

