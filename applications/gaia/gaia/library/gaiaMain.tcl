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
#     {enter_changes_here}

#-
global env

#.
puts "$env(GAIA_DIR), $env(GAIA_LIBRARY)"

#  Withdraw the . window as this cannot be controlled as a metawidget.
wm withdraw .

#  Set the auto_load path for all the GAIA source TCL files.
set gaia_library $env(GAIA_DIR)
set rtd_library $env(RTD_LIBRARY)
set skycat_library $env(SKYCAT_LIBRARY)
lappend auto_path $gaia_library $skycat_library $rtd_library

#  Add blt library to auto_path.
if {[info exists env(BLT_LIBRARY)]} {
    lappend auto_path $env(BLT_LIBRARY)
}

#  Add any required namespaces to the import list.
import add ::blt
import add ::rtd
namespace ::gaia {}
import add ::gaia
namespace ::iwidgets {}
import add ::iwidgets

#  Make GAIA modified core Tcl code available in preference to 
#  the normal versions.
#source $gaia_library/TopLevelWidget.tcl
source $gaia_library/FrameWidget.tcl
source $gaia_library/CanvasPrint.tcl 
source $gaia_library/FileSelect.tcl 
source $gaia_library/LabelEntry.tcl
source $gaia_library/LabelEntryMenu.tcl 
source $gaia_library/LabelEntryScale.tcl 
source $gaia_library/RtdImage.tcl 
source $gaia_library/RtdImageCtrl.tcl 
source $gaia_library/RtdImagePick.tcl
source $gaia_library/RtdImagePixTable.tcl 
source $gaia_library/RtdImageTrans.tcl 
source $gaia_library/RtdImageZoomView.tcl
source $gaia_library/udialog.tcl
source $gaia_library/Batch.tcl

#  Set interface for strict Motif compliance.
set tk_strictMotif 1

#  Decent precision is nice.
set tcl_precision 17

#  Define a useful procedure for handling image names with slices.
proc fileName {image} {

   # return the proper filename of an image and any slice information.
   set i1 [string last {(} $image]
   set i2  [string last {)} $image]
   if { $i1 > -1 && $i2 > -1 } {
      set slice [string range $image $i1 $i2]
      incr i1 -1
      set image [string range $image 0 $i1]
   } else {
      set slice ""
   }
   set image2 "$image"
   if { [file extension $image] == "" } {
      set image "${image}.sdf"
   }
   return [list $image $slice]
}

#  Check any command-line arguments that we've been passed. The first
#  one is the name of the image and any others are configuration
#  options. What we need to do is reconstruct an argv string with the
#  correct format (i.e. -file blah other options).
if { $argc >= 1 } {
   set image [lindex $argv 0]

   #  If first argument doesn't start with a '-' then its an image,
   #  otherwise no image has been given and we just get on with it.
   if { ! [string match {-*} $image] } {

      #  Just as a convenience check the image exists and if not the
      #  name for a file type.  If it has none then add ".sdf".
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
      set argv "-file $argv"
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

#  Where to look for catalog config file: use ~/.skycat/skycat.cfg if
# it exists, since it may contain user's preferences, otherwise use
# $CATLIB_CONFIG (do not use SKYCAT_CONFIG, this also set by
# CURSA).
set config_file $env(HOME)/.skycat/skycat.cfg
if {[file exists $config_file]} {
   set env(CATLIB_CONFIG) "file:$config_file"
} 

#  Start up the main window.
eval Gaia .rtd0 $argv

#----------------------------------------------
#  Debugging code.
# set debug 1
#  if { $debug } {
#     proc profile_on {args} {
#        eval profile $args on
#        .l configure -text "profiling on ($args)"
#     }
#     proc profile_off {args} {
#        global tmp
#        if { [.l cget -text] != "profiling off" } { 
#           profile off tmp
#        }
#        if { [info exists tmp] } { 
#           eval profrep tmp $args
#        }
#        .l configure -text "profiling off"
#     }
  
#     button .a -command {blt::bltdebug 100} -text {Debug on}
#     button .b -command {profile_on} -text {profile on}
#     button .c -command {profile_on -commands} -text {profile on (commands)}
#     button .d -command {profile_on -eval} -text {profile on (eval)}
#     button .e -command {profile_on -commands -eval} -text {profile on (eval & commands)}
#     button .f -command {profile_off cpu} -text {profile off (cpu)}
#     button .g -command {profile_off real} -text {profile off (real)}
#     button .h -command {profile_off calls} -text {profile off (calls)}
  
#     label .l -text "profiling off" -relief raised
#     pack .a .b .c .d .e .f .g .h .l -fill x
#     after idle [wm deiconify .]
#  }   
