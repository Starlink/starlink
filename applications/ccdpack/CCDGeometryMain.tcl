#+
#  Name:
#     CCDGeometryMain

#  Purpose:
#     Main routine for CCD geometry X interface.

#  Type of Module:
#     Tcl/Tk script.

#  Description:
#     This is the top-level routine for the X reduction interface for
#     determining CCD geometries. It creates the initial window and
#     performs global initialisations of bindings, colours, script
#     auto path etc.

#  Invocation:
#     wish CCDGeometryMain

#  Notes:
#     This interface requires that the extensions [incr Tcl] version
#     1.5, BLT 1.8 and TclADAM (pre-release) are available (built into
#     the wish executable that invokes this file) as well as Tcl7.4p2
#     and Tk4.0p2. It is not known to work with any other combinations
#     and will not work with earlier versions of Tcl and Tk.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  History:
#     9-OCT-1994 (PDRAPER):
#        Original version
#     {enter_changes_here}

#-

#  Global variables (strictly all variables at this level are global).
   global env
   global CCDstarhtml
   global CCDimagefilters
   global CCDbrowser
   global CCDseetasks
   global CCDallndfs
   global CCDglobalpars
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
   } else {
      if { [info exists env(STARLINK)] } {
         set CCDstarhtml "$env(STARLINK)/docs:$env(STARLINK)/help"
      } else {

#  Use a relative offset to CCDdir.
         set CCDstarhtml "$CCDdir/../../docs:$CCDdir/../../help"
      }
   }

#  Set the help browser. This is either set by the HTX_BROWSER variable,
#  or by checking that a known one exists on the PATH. This will
#  be overidden by a ~/.ccdpack assignment.
   if { [info exists env(HTX_BROWSER)] } { 
      set CCDbrowser $env(HTX_BROWSER)
   } else {
      set CCDbrowser {}
      foreach browser {netscape Netscape Mosaic mosaic} {
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
   set CCDimagefilters {{NDF "*.sdf"}}
   if { [info exists env(NDF_FORMATS_IN)] } { 
      set new_types [split $env(NDF_FORMATS_IN) ","]
      foreach pair $new_types { 
         regexp {([^\(]*).([^\)]*)} $pair dummy name type
         if { $name != "FITS" && $name != "NDF" && $name != "FIT" } { 
            lappend CCDimagefilters [list $name *${type}]
         }
      }
   }

#  Set the display device.
   global XDEVICE
   set XDEVICE "xw;CCDgeometry"
   global GWMDEVICE
   set GWMDEVICE "CCDgeometry"


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
   if { [info exists CCDglobalpars(EXTENT)] } {
      append results "    Useful CCD area = $CCDglobalpars(EXTENT) \n"
   }

#  Readout direction.
   if {[info exists CCDglobalpars(DIRECTION)] } {
      append results "    Readout direction = $CCDglobalpars(DIRECTION) \n"
   }

#  Bias strips.
   if {[info exists CCDglobalpars(BOUNDS)] } {
      append results "    Bias strip bounds = $CCDglobalpars(BOUNDS) \n"
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
