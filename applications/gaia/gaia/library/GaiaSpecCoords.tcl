#+
#  Name:
#     GaiaSpecCoords

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Control the spectral or timescale coordinates of an AST FrameSet.

#  Description:

#     This class populates menus with preset selections for the various
#     coordinates systems and standard of rest that the spectral or time
#     axis of a AST FrameSet might take (assuming the FrameSet contains
#     an axis with has a SpecFrame).  The FrameSet is accessed using a
#     GaiaNDAccess instance.
#
#     The disposition of the menu items can be changed to match those of
#     a new GaiaNDAccess instance, these will be greyed by setting to
#     disabled when the axis of the FrameSet is not a SpecFrame or
#     TimeFrame. When a SpecFrame is available that doesn't have a
#     rest frequency then the velocity items with be greyed.

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
#     Copyright (C) 2008, 2012 Science and Technology Facilities Council.
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     MJC: Malcolm J. Currie (JAC, Hawaii)
#     {enter_new_authors_here}

#  History:
#     12-MAY-2006 (PWD):
#        Original version.
#     2012 April 20 (MJC):
#        Created support to modify the standard of rest.
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
               -command [code $this set_system "$system" "$unit" 0]
         }
         foreach {descr unit system} $velocity_systems_ {
            $menu add command -label "$descr" \
               -command [code $this set_system "$system" "$unit" 0]
         }
         foreach {descr timescale} $timescales_ {
            $menu add command -label "$descr" \
               -command [code $this set_timescale "$timescale" 0]
         }
         foreach {descr sor} $standards_of_rest_ {
            $menu add command -label "$descr" \
               -command [code $this set_sor "$sor" 0]
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
         set isatimeframe [$accessor isaxisframetype $axis "timeframe"]
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
               foreach {descr sor} $standards_of_rest_ {
                  $menu entryconfigure "$descr" -state normal
               }
            } else {
               foreach {descr unit system} $simple_systems_ {
                  $menu entryconfigure "$descr" -state disabled
               }
                foreach {descr sor} $standards_of_rest_ {
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

            if { $isatimeframe } {
               $menu entryconfigure "Default" -state normal
               foreach {descr timescale} $timescales_ {
                  $menu entryconfigure "$descr" -state normal
               }
            } else {
               foreach {descr timescale} $timescales_ {
                  $menu entryconfigure "$descr" -state disabled
               }
            }
         }
      }
   }

   #  Set the system and unit of the AST FrameSet. Also eval the change_cmd if
   #  we have one and it is requested (quiet false).
   public method set_system {system unit {quiet 0} } {
      if { $accessor != {} } {
         if { $system == "default" } {

            #  If there is no local SpecFrame (or TimeFrame), which is true
            #  when default_system_($axis) is not set, then clearly we are
            #  using the default system, i.e. the one of the underlying
            #  data and there's nothing to do.
            if { [info exists default_system_($axis)] } {

               #  If time_ is set then we're using offset time. Undo that.
               undo_time_offset_

               $accessor astset $default_system_($axis)
            }
         } else {
            if { $unit != "" } {
               $accessor astset "System($axis)=$system,Unit($axis)=$unit"
            } else {
               #  Unit-less system.
               $accessor astset "System($axis)=$system"
            }
         }

         if { ! $quiet && $change_cmd != {} } {
            eval $change_cmd
         }
      }
   }

   #  Set the StdOfRest of the AST FrameSet. Also eval the change_cmd if
   #  we have one and it is requested (quiet false).
   public method set_sor {sor {quiet 0} } {
      if { $accessor != {} } {
         if { $sor == "default" } {

            #  If there is no local standard of rest, which is true when
            #  default_sor_ is not set, then clearly we are using the
            #  default standard of rest, i.e. the one of the underlying
            #  data and there is nothing to do.
            if { [info exists default_sor_] } {

               $accessor astset $default_sor_
            }

         } else {
            $accessor astset "StdOfRest=$sor"
         }

         if { ! $quiet && $change_cmd != {} } {
            eval $change_cmd
         }
      }
   }

   #  Set the timescale of the AST FrameSet. Also eval the change_cmd if
   #  we have one and it is requested (quiet false).
   public method set_timescale {timescale {quiet 0} } {
      if { $accessor != {} } {

         #  Displaying offset time requires a non iso date format. This will
         #  then show time offset from the TimeOrigin.
         if { $timescale == "OT" } {

            #  Record defaults for things we change in offset time.
            save_time_offset_

            #  Need first coordinate of spectrum as new origin, so get pixel
            #  coordinates of the first value and evaluate that position.
            lassign [$accessor getbounds 0] l1 u1 l2 u2 l3 u3
            set section [list [expr $l1-0.5] [expr $l2-0.5] [expr $l3-0.5]]
            set value [$accessor getcoord $axis $section 0]
            if { $value != {} } {

               #  Adjust TimeOrigin to start at time of first coordinate
               #  and to display offsets (requires non-iso format) in seconds.
               set value [expr $time_(origin) + $value]
               $accessor astset "TimeOrigin($axis)=$value"
               $accessor astset "Unit($axis)=s"
               $accessor astset "Format($axis)=%%.12g"
            }
         } else {

            #  Restore defaults for formatting etc. if switched to offset time.
            undo_time_offset_

            $accessor astset "TimeScale($axis)=$timescale"
         }
         if { ! $quiet && $change_cmd != {} } {
            eval $change_cmd
         }
      }
   }

   #  Save defaults of attributes changed when switching to offset time. If
   #  not already in effect.
   protected method save_time_offset_ {} {
      if { ! [info exists time_] } {
         set time_(format) [$accessor astget Format($axis)]
         set time_(origin) [$accessor astget TimeOrigin($axis)]
         set time_(unit) [$accessor astget Unit($axis)]
      }
   }

   #  Undo the changes made to the WCS when switching to offset time.
   #  Note these are order sensitive.
   protected method undo_time_offset_ {} {
      if {  [info exists time_] } {
         $accessor astset "Unit($axis)=$time_(unit)"
         $accessor astset "Format($axis)=$time_(format)"
         $accessor astset "TimeOrigin($axis)=$time_(origin)"
         unset time_
      }
   }

   #  Return a list containing the current system and units. If this isn't
   #  a SpecFrame then no system and units are returned. If the system is the
   #  default one then the string "default" is returned.
   public method get_system {} {
      if { $accessor != {} } {
         if { [$accessor isaxisframetype $axis "specframe"] } {
            set system [$accessor astget "System($axis)"]
            set units [$accessor astget "Unit($axis)"]
            set astatt "System($axis)=$system,Unit($axis)=$units"
            if { [info exists default_system_] &&
                 $astatt != $default_system_($axis) } {
               return "$system $units"
            }
            return [list "default" "default"]
         }
      }
      return [list "" ""]
   }

   #  Return a list containing the current standard of rest.  If this
   #  is not a SpecFrame then no standard of rest is returned.  If the
   #  standard of rest is the default one then the string "default" is
   #  returned.
   public method get_sor {} {
      if { $accessor != {} } {
         if { [$accessor isaxisframetype $axis "specframe"] } {
            set sor [$accessor astget "StdOfRest"]
            set astatt "StdOfRest=$sor"
            if { [info exists default_sor_] &&
                 $astatt != $default_sor_ } {
               return "$sor"
            }
            return [list "default"]
         }
      }
      return [list "" ""]
   }

   #  Record the default system for a particular axis of an accessor.
   protected method record_default_system_ {} {
      if { $accessor != {} && ! [info exists default_system_($axis)] } {
         if { [$accessor isaxisframetype $axis "specframe"] } {
            set system [$accessor astget "System($axis)"]
            set units [$accessor astget "Unit($axis)"]
            set default_system_($axis) \
               "System($axis)=$system,Unit($axis)=$units"
         } elseif { [$accessor isaxisframetype $axis "timeframe"] } {
            set timescale [$accessor astget "TimeScale($axis)"]
            set default_system_($axis) "TimeScale($axis)=$timescale"
         } else {
            catch {unset default_system_(axis)}
         }
      }
   }

   #  Record the default sor for a particular axis of an accessor.
   protected method record_default_sor_ {} {
      if { $accessor != {} && ! [info exists default_sor_] } {
         if { [$accessor isaxisframetype $axis "specframe"] } {
            set sor [$accessor astget "StdOfRest"]
            set default_sor_ "StdOfRest=$sor"
         } else {
            catch {unset default_sor_(axis)}
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
         record_default_system_
         catch {unset default_sor_}
         record_default_sor_
         set last_accessor_ $accessor
      }
      reconfigure_menus_
   }

   #  The spectral axis (AST index). Updates the default system if needed.
   public variable axis 3 {
      record_default_system_
      record_default_sor_
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

   #  The default (that's initial) values for StdOfRest for each axis.
   #  Note initially unset array indexed by $axis.
   protected variable default_sor_

   #  Default attributes for the time axis.
   protected variable time_

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
      "Redshift" "" "ZOPT"
   }

   #  List of possible time scales. Note "Offset Time" isn't known to AST.
   common timescales_ {
      "Offset Time" "OT"
      "International Atomic Time (TAI)" "TAI"
      "Coordinated Universal Time (UTC)" "UTC"
      "Universal Time (UT1)" "UT1"
      "Greenwich Mean Sidereal Time (GMST)" "GMST"
      "Local Apparent Sidereal Time (LAST)" "LAST"
      "Local Mean Sidereal Time (LMST)" "LMST"
      "Terrestrial Time (TT)" "TT"
      "Barycentric Dynamical Time (TDB)" "TDB"
      "Barycentric Coordinate Time (TCB)" "TCB"
      "Geocentric Coordinate Time (TCG)" "TCG"
      "Local Time" "LT"
   }

   common standards_of_rest_ {
      "Topocentric" "TOPO"
      "Default (StdOfRest)" "default"
      "Geocentric" "GEO"
      "Barycentric" "BARY"
      "Heliocentric" "HELIO"
      "Kinematic Local" "LSRK"
      "Dynamical Local" "LSRD"
      "Galactic" "GAL"
      "Local_group" "LG"
      "Source" "SRC"
   }

}
