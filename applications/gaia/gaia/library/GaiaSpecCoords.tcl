#+
#  Name:
#     GaiaSpecCoords

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Control the spectral coordinates of an AST FrameSet.

#  Description:
#     This class currently creates a menu populated with preset selections for
#     the various coordinates systems that the spectral axis of a AST FrameSet
#     might take (assuming the FrameSet contains an axis with has a
#     SpecFrame). The FrameSet is accessed using an GaiaNDAccess instance.


#  Invocations:
#
#        GaiaSpecCoords object_name [configuration options]
#
#     This creates an instance of a GaiaSpecCoords object. The return is
#     the name of the object.
#
#        object_name configure -configuration_options value
#
#     Applies any of the configuration options (after the instance has
#     been created).
#
#        object_name method arguments
#
#     Performs the given method on this object.

#  Configuration options:
#     See itk_option definitions below.

#  Methods:
#     See individual method declarations below.

#  Inheritance:
#     Nothing.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     12-MAY-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaSpecCoords {}

itcl::class gaia::GaiaSpecCoords {

   #  Inheritances:
   #  -------------
   #  Nothing for this class.

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options, should include the menu we're to populate, the
      #  GaiaNDAccess instance and the spectral axis index.
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor {
      #  Nothing to do.
   }

   #  Methods:
   #  --------

   #  Add items to a menu so that a known coordinate system can be selected.
   protected method configure_menu_ {} {
      if { $menu != {} } {
         foreach {descr unit system} $systems_ {
            $menu add command -label "$descr" \
               -command [code $this set_selected_system_ "$unit" "$system"]
         }
      }
   }

   #  Apply the selected system to the AST FrameSet. 
   #  Also eval the change_cmd is we have one.
   protected method set_selected_system_ {unit system} {
      if { $accessor != {} } {
         $accessor astset "System($axis)=$system,Unit($axis)=$unit"
         if { $change_cmd != {} } {
            eval $change_cmd
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Define a menu to populate with items for selecting a coordinate system.
   public variable menu {} {
      configure_menu_
   }

   #  The GaiaNDAccess instance.
   public variable accessor {}

   #  The spectral axis (AST index).
   public variable axis 3

   #  A command to execute when the spectral coordinates are changed.
   public variable change_cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------

   #  List of the possible spectral coordinates. These are presented to the
   #  user as units only. The system is assumed.
   common systems_ {
      "Angstroms" "Angstrom" "WAVE"
      "Nanometres" "nm" "WAVE"
      "Millimetres" "mm" "WAVE"
      "Micrometres" "um" "WAVE"
      "Gigahertz" "GHz" "FREQ"
      "Megahertz" "MHz" "FREQ"
      "Terahertz" "THz" "FREQ"
      "Kilohertz" "kHz" "FREQ"
      "Joules" "J" "ENER"
      "Ergs" "erg" "ENER"
      "Electron-volts" "eV" "ENER"
      "Kilo-electron-volts" "keV" "ENER"
      "Metres-per-sec (radio)" "m/s" "VRAD"
      "Kilometres-per-sec (radio)" "km/s" "VRAD"
      "Per-metre" "1/m" "WAVN"
   }
}
