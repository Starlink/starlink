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

#  Set the place for locating all external files.
if { [info exists env(GAIA_DIR)] } { 
   set gaia_dir $env(GAIA_DIR)
}

#  Check any command-line arguments that we've been passed. The first
#  one is the name of the image and any others are configuration
#  options. What we need to do is add the file type (.sdf) if missing.
if { $argc >= 1 } {
   set image [lindex $argv 0]
   
   #  If first argument doesn't start with a '-' then its an image,
   #  otherwise no image has been given and we just get on with it.
   if { ! [string match {-*} $image] } {
      if { ! [file readable $image] } {
         
         #  First we must allow for any trailing slice specification.
         #  The format for this is a group of numbers in () at the end
         #  of the string.
         lassign [fileName $image] image slice
         if { ! [file readable $image] } {
            gaia::setXdefaults
            error_dialog "Cannot read image \"${image}${slice}\""
            exit 1
         } else {
            append image $slice
            set argv [lreplace $argv 0 0 $image]
         }
      }
   }
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

#  This is a native GAIA installation when started by this route
#  (i.e. not the plugin).
set env(NATIVE_GAIA) 1

#  Start up the main window.
gaia::Gaia::startGaia
exit
