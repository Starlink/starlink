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

#  Copyright:
#     Copyright (C) 1996-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of the
#     License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#     GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PWD: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     21-NOV-1996 (PWD):
#        Original version
#     12-MAR-1999 (PWD):
#        Updated to work with GAIA plugin.
#     15-JUL-2003 (PWD):
#        Modified to also start the tabbed version of interface.
#     {enter_changes_here}

#-

global ::env ::gaia_dir

#.

#  Access the GAIA commands.
package require Gaia

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

#  DEBUG: track all puts.
#rename ::puts ::puts_orig
#proc ::puts {args} {
#   for { set i [info level] } { $i > -1 } { incr i -1 } {
#      ::puts_orig "$i: [info level $i]"
#   }
#   eval ::puts_orig $args
#}
#puts "puts redefined"


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#  Set the place for locating all external files.
if { [info exists env(GAIA_DIR)] } {
   set gaia_dir $env(GAIA_DIR)
   set gaia_help $gaia_dir/../../help/gaia
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
      if { $hdu != 0 } {
         lappend argv "-hdu"
         lappend argv "$hdu"
         incr argc 2
      }

      #  Replace the given name by one that GAIA can display.
      set argv [lreplace $argv 0 0 "-file" [.namer fullname 0]]
      delete object .namer
   }
}

#  See if this is the tabbed interface.
set usetabbed 0
set tindex [lsearch -exact $argv "-tabbedgaia"]
if { $tindex != -1 } {
   incr tindex
   set usetabbed [lindex $argv $tindex]
}

#  Restore any properties of the Gaia object that have been set last
#  time around (and explicity saved), these go before other options so
#  that these may be overridden on the command-line.
set props [gaia::GaiaProperties::instance]
foreach fullname [$props get_named_keys Gaia] {
   set value [$props get_property $fullname]
   set option [$props get_unnamed_key Gaia $fullname]
   if { [gaia::GaiaStartup::check_option $option] && $value != {} } {
      set argv [linsert $argv 0 "-$option" "$value"]
   } else {
      puts stderr "Warning: rejected incompatible session persistent option: -$option $value"
      $props unset_property $fullname
   }
}

#  Extract the known file types and set these up as defaults. These
#  are entered as if command-line arguments so that they propagate
#  to clone windows. Exclude GIF and TIFF as these always disappoint.
set file_types {{any *} {NDF(.sdf) *.sdf} {FIT(.fit) *.fit} \
                {FITS(.fits) *.fits} {FITS(.fts) *.fts}}
if { [info exists env(NDF_FORMATS_IN)] } {
   set new_types [split $env(NDF_FORMATS_IN) ","]
   foreach pair $new_types {
      regexp {([^\(]*).([^\)]*)} $pair dummy name type
      if { $name != "NDF" && $type != ".fits" && $type != ".fit" &&
           $type != ".fts" && $name != "GIF" && $name != "TIFF" } {
         lappend file_types [list $name\($type\) *${type}]
      }
   }
}
lappend argv "-file_types"
lappend argv $file_types
incr argc 2

if { $usetabbed } {

   #  We don't want the HDU chooser.
   lappend argv "-show_hdu_chooser"
   lappend argv "0"
   incr argc 2

   #  Start up the tabbed main window.
   gaia::TabbedGaia .\#auto

} else {

   #  Start up the plain main window.
   gaia::Gaia::startGaia
}
