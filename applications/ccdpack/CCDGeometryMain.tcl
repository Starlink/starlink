#+
#  Name:
#     CCDGeometryMain

#  Purpose:
#     Main routine for CCD geometry X interface.

#  Language:
#     TCL

#  Type of Module:
#     Tcl/Tk script.

#  Invocation:
#     wish CCDGeometryMain

#  Description:
#     This is the top-level routine for the X reduction interface for
#     determining CCD geometries. It creates the initial window and
#     performs global initialisations of bindings, colours, script
#     auto path etc.

#  Notes:
#     This interface requires that the extensions [incr Tcl], BLT
#     and TclADAM are available (built into the wish executable that invokes
#     this file) as well as Tcl and Tk.

#  Copyright:
#     Copyright (C) 1994 Science & Engineering Research Council.
#     Copyright (C) 2001, 2003 Central Laboratory of the Research
#     Councils. Copyright (C) 2006 Particle Physics & Astronomy
#     Research Council. All Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     MBT: Mark Taylor (STARLINK)
#     {enter_new_authors_here}

#  History:
#     9-OCT-1994 (PDRAPER):
#        Original version
#     4-JUL-2001 (MBT):
#        Fixed to use globals keyed by Set Index if required.
#     22-JUL-2003 (MBT):
#        Added option to use Mozilla.
#     1-JAN-2006 (PDRAPER):
#        Fixed problems with default file filters being shown as blank.
#        Changed to use new meta-widget names (s/Ccd_/Ccd::/g).
#     {enter_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-

#  Global variables (strictly all variables at this level are global).
   global env
   global CCDstarhtml
   global CCDimagefilters
   global CCDbrowser
   global CCDseetasks
   global CCDallndfs
   global CCDglobalpars
   global CCDgloprefix
   global KAPdir
   global MAIN
#.

#  Name this application (for xresources etc.)
   tk appname geometry
   set MAIN(name) GEOMETRY

#  Withdraw the . window as we don't need it.
   wm withdraw .

#-----------------------------------------------------------------------------
#  Interface initialisation section:
#-----------------------------------------------------------------------------
#  Set the global variable which controls where to pickup source etc.
   if { [ info exists env(CCDPACK_DIR) ] } {
      set CCDdir $env(CCDPACK_DIR)
   } else {
      set CCDdir /star/bin/ccdpack
   }

#  Set the HTML docs installation points.
   if { [ info exists env(CCDPACK_HTML) ] } {
      set CCDstarhtml $env(CCDPACK_HTML)
      set CCDstarhtml "$CCDdir/../../help:$CCDdir/../../docs:$CCDdir"
   }

#  Set the help browser. This is either set by the HTX_BROWSER variable,
#  or by checking that a known one exists on the PATH. This will
#  be overidden by a ~/.ccdpack assignment.
   if { [info exists env(HTX_BROWSER)] } {
      set CCDbrowser $env(HTX_BROWSER)
   } else {
      set CCDbrowser {}
      foreach browser {netscape Netscape mozilla Mozilla firefox Firefox} {
         foreach directory [split $env(PATH) ":" ] {
            if { [ file executable ${directory}/${browser} ] } {
               set CCDbrowser $browser
               break
            }
         }
         if { $CCDbrowser != {} } { break }
      }
   }

#  Set the interface look and feel. Define the autoload path etc.
   lappend auto_path $CCDdir

#  Global bindings.
   source $CCDdir/CCDBindings.tcl

#  Global options. Also re-reads ~/.ccdpack.
   source $CCDdir/CCDOptions.tcl

#  Find and store all the NDF foreign formats being used. These are
#  used as part of the file filtering mechanisms (note this isn't
#  therefore set by any values in .ccdpack, it's important that
#  the conversion filters are actually setup).
   set CCDimagefilters "*.sdf"
   if { [info exists env(NDF_FORMATS_IN)] } {
      set CCDimagefilters {{NDF(.sdf) *.sdf}}
      set new_types [split $env(NDF_FORMATS_IN) ","]
      foreach pair $new_types {
         regexp {([^\(]*).([^\)]*)} $pair dummy name type
         if { $name != "NDF" } {
            lappend CCDimagefilters [list $name\(${type}\) *${type}]
         }
      }
   }

#  Set the display device.
   global XDEVICE
   set XDEVICE "xw;CCDgeometry"
   global GWMDEVICE
   set GWMDEVICE "CCDgeometry"

#  No set prefixes.
   set CCDgloprefix {}

#------------------------------------------------------------------------------

#  Initialise the applications that we need.
   if { [info exists env(KAPPA_DIR)] } {
      set KAPdir $env(KAPPA_DIR)
   } else {
      set KAPdir /star/bin/kappa
   }

#  Initialise the application register.
   if { ! [CCDTaskRegistry] } {

#  If failed no CCDPACK monolith!
      $Top kill $Top
      destroy .
   }

#  Start the geometry application.
   set MAIN(window) .geometry
   CCDGeometry .geometry 0


#  Wait for interaction to end... and then write out results.
#  CCD extent.
   set results ""
   if { [info exists CCDglobalpars(${CCDgloprefix}EXTENT)] } {
      append results \
      "    Useful CCD area = $CCDglobalpars(${CCDgloprefix}EXTENT) \n"
   }

#  Readout direction.
   if {[info exists CCDglobalpars(${CCDgloprefix}DIRECTION)] } {
      append results \
      "    Readout direction = $CCDglobalpars(${CCDgloprefix}DIRECTION) \n"
   }

#  Bias strips.
   if {[info exists CCDglobalpars(${CCDgloprefix}BOUNDS)] } {
      append results \
      "    Bias strip bounds = $CCDglobalpars(${CCDgloprefix}BOUNDS) \n"
   }

#  Write out the result if any.
   if { "$results" != "" } {
      puts ""
      puts "      Geometry"
      puts "      ========"
      puts ""
      puts "  Results of CCD geometry determination:"
      puts ""
      puts "$results"
      puts ""
   } else {
      puts stderr "!! No CCD geometries determined"
   }

#  Finally destroy the program
   destroy .

#  $Id$"
