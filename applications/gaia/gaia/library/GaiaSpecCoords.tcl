#+
#  Name:
#     GaiaSpecCoords

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Control the spectral coordinates of an AST FrameSet.

#  Description:

#     This class populates menus with preset selections for the various
#     coordinates systems that the spectral axis of a AST FrameSet might take
#     (assuming the FrameSet contains an axis with has a SpecFrame). The
#     FrameSet is accessed using an GaiaNDAccess instance.
#
#     The disposition of the menu items can be changed to match those of 
#     a new GaiaNDAccess instance (these will be greyed by setting to
#     disabled when the axis of the FrameSet is not a SpecFrame and 
#     the velocity items with be greyed if a rest frequency has not been
#     defined).

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

      #  Evaluate any options, normally the initial GaiaNDAccess instance and
      #  the spectral axis index. 
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor {
      #  Nothing to do.
   }

   #  Methods:
   #  --------

   #  Add a menu to manage and configure it.
   public method add_menu {menu} {
      lappend menus_ $menu
      populate_menu_ $menu
      reconfigure_menus_
   }

   #  Add items to a menu so that a known coordinate system can be selected.
   protected method populate_menu_ {menu} {
      if { $menu != {} } {
         foreach {descr unit system} $simple_systems_ {
            $menu add command -label "$descr" \
               -command [code $this set_selected_system_ "$unit" "$system"]
         }
         foreach {descr unit system} $velocity_systems_ {
            $menu add command -label "$descr" \
               -command [code $this set_selected_system_ "$unit" "$system"]
         }
      }
   }

   #  Reconfigure all the managed menus to reflect the coordinates available
   #  in the attached AST FrameSet. This disables items that cannot be applied
   #  (a SpecFrame is required and a rest frequency for transformation to
   #  velocities) and updates the default system.
   protected method reconfigure_menus_ {} {
      if { $menus_ != {} && $accessor != {} } {

         #  Check if WCS a SpecFrame and has a rest frequency
         set isaspecframe [$accessor isaxisframetype $axis "specframe"]
         if { $isaspecframe } {
            set haverestfreq [$accessor asttest "RestFreq($axis)"]
         } else {
            set haverestfreq 0
         }

         foreach menu $menus_ {
            if { $isaspecframe } {
               foreach {descr unit system} $simple_systems_ {
                  $menu entryconfigure "$descr" -state normal
               }
            } else {
               foreach {descr unit system} $simple_systems_ {
                  $menu entryconfigure "$descr" -state disabled
               }
            }

            if { $isaspecframe && $haverestfreq } {
               foreach {descr unit system} $velocity_systems_ {
                  $menu entryconfigure "$descr" -state normal
               }
            } else {
               foreach {descr unit system} $velocity_systems_ {
                  $menu entryconfigure "$descr" -state disabled
               }
            }
         }
      }
   }

   #  Apply the selected system to the AST FrameSet. 
   #  Also eval the change_cmd is we have one.
   protected method set_selected_system_ {unit system} {
      if { $accessor != {} } {
         if { $system == "default" && [info exists default_system_($axis)] } {
            $accessor astset $default_system_($axis)
         } else {
            $accessor astset "System($axis)=$system,Unit($axis)=$unit"
         }
         if { $change_cmd != {} } {
            eval $change_cmd
         }
      }
   }

   #  Record the default system for a particular axis of an accessor.
   protected method set_default_system_ {} {
      if { $accessor != {} && ! [info exists default_system_($axis)] } {
         if { [$accessor isaxisframetype $axis "specframe"] } {
            set system [$accessor astget "System($axis)"]
            set units [$accessor astget "Unit($axis)"]
            set default_system_($axis) \
               "System($axis)=$system,Unit($axis)=$units"
         } else {
            catch {unset default_system_(axis)}
         }
      }
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  The GaiaNDAccess instance. Will cause the menus to be reconfigured,
   #  and also updates the default system iff the accessor is changed.
   public variable accessor {} {
      if { $last_accessor_ != $accessor } {
         catch {unset default_system_}
         set_default_system_
         set last_accessor_ $accessor
      }
      reconfigure_menus_
   }

   #  The spectral axis (AST index). Updates the default system if needed.
   public variable axis 3 {
      set_default_system_
   }

   #  The last value for accessor, stops changes when is same.
   protected variable last_accessor_ {}

   #  A command to execute when a spectral coordinate system is selected.
   public variable change_cmd {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  The menus we're managing. A simple list of names.
   protected variable menus_ {}

   #  The default (that's initial) values for System and Unit for each axis.
   #  Note initially unset array indexed by $axis.
   protected variable default_system_

   #  Common variables: (shared by all instances)
   #  -----------------

   #  List of the possible spectral coordinates, excluding velocities. 
   #  These are presented to the user as units only. The system is assumed.
   common simple_systems_ {
      "Default" "default" "default"
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
      "Per-metre" "1/m" "WAVN"
   }

   #  List of the possible spectral coordinates involving velocities. 
   #  These are presented to the user as units only. The system is assumed.
   common velocity_systems_ {
      "Metres-per-sec (radio)" "m/s" "VRAD"
      "Kilometres-per-sec (radio)" "km/s" "VRAD"
   }
}
