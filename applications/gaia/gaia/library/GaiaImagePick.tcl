#+
#  Name:
#     GaiaImagePick.tcl

#  Purpose:
#     Widget to select an object (star), see stats about it and
#     save/append the details to a file. 

#  Type of Module:
#     [incr Tk] class

#  Description:
#     This class extends RtdImagePick to add an ability to save the 
#     current values to a file. The name of the file is fixed as
#     "GaiaPick.dat". If this exists then the new results will be
#     appended.

#  Invocation:
#     GaiaImagePick name [configuration options]

#  Notes:
#     This will only run with the gaia_wish installed as part of the
#     GAIA package.

#  Authors:
#     PDRAPER: Peter Draper (STARLINK)
#     {enter_new_authors_here}

#  Inherits:
#     Methods and configuration options of SkyCatCtrl.

#  Copyright:
#     Copyright (C) 1998-1999 Central Laboratory of the Research Councils

#  History:
#     19-MAR-1999 (PDRAPER):
#        Original version, inspired by Andy Adamson.
#     {enter_changes_here}

#-

itk::usual GaiaImagePick {}

itcl::class gaia::GaiaImagePick {
   inherit rtd::RtdImagePick
   
   #  Create a new GaiaImagePick widget
   constructor {args} {
      eval rtd::RtdImagePick::constructor $args
   } {
      eval itk_initialize $args
   }
   
   #  Destructor
   destructor {
   }
    
   #  Called after the options have been evaluated
   method init {} {
      rtd::RtdImagePick::init
      
      #  Add a save/append button.
      itk_component add save {
         button $w_.save  -text "Save/Append" -command [code $this save]
      }
      pack $itk_component(save) \
         -side left -expand 1 -padx 2m -pady 2m -in $itk_component(buttons)
      add_short_help $itk_component(save) \
         {Append stats to file GaiaPick.dat, created if doesn't exist}
   }
   
   #  save the results to a file "GaiaPick.dat", these are appended
   #  if the file exists already.
   method save {} {
      set init 0
      if { ! [file exists "GaiaPick.dat"] } { 
         set init 1
      }
      if {[catch {set fd [::open "GaiaPick.dat" a+]} msg]} {
         error_dialog $msg
         return
      }
      
      #  If new file then add headers.
      if { $init } { 
         puts $fd "name \t x \t y \t ra \t dec \t equinox \t angle \t peak \t background \t fwhm (X:Y)"
      }
      
      #  Get the image name.
      set name [$target_image_ cget -file]
      
      #  Get all the values.
      set x [$itk_component(x) cget -value]
      set y [$itk_component(y) cget -value]
      set ra [$itk_component(ra) cget -value]
      set dec [$itk_component(dec) cget -value]
      if { $ra == "" || $dec == "" } { 
         set ra  "<null>"
         set dec "<null>"
      }
      set equinox [$itk_component(equinox) cget -value]
      
      set angle [$itk_component(angle) cget -value]
      set object [$itk_component(object) cget -value]
      set background [$itk_component(background) cget -value]
      set fwhm [$itk_component(fwhm) cget -value]
      
      #  Make sure we have something to write...
      if { $x == "" } { 
         error_dialog "cannot save values, no object picked"
         return
      }
      
      #  Write them out.
      puts $fd "$name \t $x \t $y \t $ra \t $dec \t $equinox \t $angle \t $object \t $background \t $fwhm"
      $itk_option(-shorthelpwin) short_help {saved values...}
      
      ::close $fd
   }
}
