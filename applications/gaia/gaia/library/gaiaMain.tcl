#+
#  Name:
#     gaiaMain

#  Purpose:
#     Main routine for GAIA image display and analysis tool.

#  Type of Module:
#     Tcl/Tk script.

#  Description:
#     This is the top-level routine for the GAIA display tool.  It
#     creates the initial window and performs global initialisations
#     (auto path etc.).

#  Invocation:
#     source gaiaMain.tcl

#  Notes:
#     This will only run with the skycat_wish installed as part
#     of the GAIA package.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     21-NOV-1996 (PDRAPER):
#        Original version
#     12-MAR-1999 (PDRAPER):
#        Updated to work with GAIA plugin.
#     {enter_changes_here}

#-
global ::env ::tcl_version ::gaia_library ::gaia_dir

#.

#  Withdraw the . window as this cannot be controlled as a metawidget.
wm withdraw .

# XXX workaround for bug in KDM window manager. Get 2 second freeze of
# some top-level windows without this.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Save a reference to the original raise command (which is builtin).
rename raise raise_orig

#  Set a binding to record the visibility state of all windows.
#  The <Map> binding assumes that when a window is mapped it comes up
#  fully visible.  This seems reasonable, but perhaps there are window
#  managers which do not guarantee this?
bind all <Visibility> {set ::visibilityState(%W) %s}
bind all <Map> { if {!%o} {set ::visibilityState(%W) VisibilityUnobscured} }
bind all <Destroy> { catch {unset ::visibilityState(%W)} }

#  Redefine the raise command
proc raise { window { above "" } } {
   if { $above == "" } {
      if { ! [ info exists ::visibilityState($window) ] || \
              $::visibilityState($window) != "VisibilityUnobscured" } {
         raise_orig $window
      }
   } else {
      raise_orig $window $above
   }
}

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Set the place for locating all external files.
if { [info exists env(GAIA_DIR)] } { 
   set gaia_dir $env(GAIA_DIR)
   set gaia_help $gaia_dir/../../help/gaia.htx
}

#  Check any command-line arguments that we've been passed. The first
#  one is the name of the image and any others are configuration
#  options. What we need to do is parse the name into a full one
#  (missing .sdfs).
if { $argc >= 1 } {
   set image [lindex $argv 0]
   
   #  If first argument doesn't start with a '-' then its an image,
   #  otherwise no image has been given and we just get on with it.
   if { ! [string match {-*} $image] } {
      gaia::GaiaImageName .namer -imagename $image

      #  Set the HDU, this will be removed from the name, if needed.
      set hdu [.namer fitshdunum]
      lappend argv "-hdu" 
      lappend argv "$hdu"
      incr argc 2

      #  Replace the given name by one that GAIA can display.
      set argv [lreplace $argv 0 0 [.namer fullname 0]]
      delete object .namer
   }
}

#  Extract the known file types and set these up as defaults. These
#  are entered as if command-line arguments so that they propagate
#  to clone windows. Exclude GIF and TIFF as these always disappoint.
set file_types {{any *} {NDF(.sdf) *.sdf} {FIT(.fit) *.fit} {FITS(fits) *.fits}}
if { [info exists env(NDF_FORMATS_IN)] } {
   set new_types [split $env(NDF_FORMATS_IN) ","]
   foreach pair $new_types {
      regexp {([^\(]*).([^\)]*)} $pair dummy name type
      if { $name != "NDF" && $type != ".fits" && 
           $type != ".fit" && $name != "GIF" &&
           $name != "TIFF" } {
         lappend file_types [list $name\($type\) *${type}]
      }
   }
}
lappend argv "-file_types"
lappend argv $file_types
incr argc 2

#  This is a native GAIA installation when started by this route
#  (i.e. not the plugin).
set env(NATIVE_GAIA) 1

#  Start up the main window.
gaia::Gaia::startGaia
exit
