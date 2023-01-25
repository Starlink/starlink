#+
#  Name:
#     GaiaAstSystem

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Creates a toolbox for changing the celestial coordinate system
#     of an imaqe.

#  Description:
#     This class creates a window that contains controls for setting
#     the celestial coodinate system of an image. This may be done
#     temporarily or permanently and the image saved with the new
#     coordinate system. Current systems are FK5, FK4 (with and
#     without e-terms), Galactic, SuperGalactic, Ecliptic and AzEl.
#     These may be qualified with equinoxes and epochs as required.

#  Invocations:
#
#        GaiaAstSystem object_name [configuration options]
#
#     This creates an instance of a GaiaAstSystem object. The return is
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
#    See itk_option define lines below.

#  Methods:
#     See method definitions below.

#  Inheritance:
#     TopLevelWidget.

#  Copyright:
#     Copyright (C) 1998-2005 Central Laboratory of the Research Councils.
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
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     22-JAN-1998 (PWD):
#        Original version.
#     23-MAY-2000 (PWD):
#         Renamed GaiaAstSystem.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaAstSystem {}

itcl::class gaia::GaiaAstSystem {

   #  Inheritances:
   #  -------------

   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options
      eval itk_initialize $args

      set lwidth 12
      set vwidth 20

      #  Set the top-level window title.
      wm title $w_ "GAIA: Image celestial coordinate system ($itk_option(-number))"

      #  Create the short help window.
      make_short_help
      $itk_component(short_help) configure -width 40

      #  Add File menu for usual stuff.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0

      #  Add option to create a new window.
      $File add command -label {New window} \
         -command [code $this clone_me_] \
         -accelerator {Control-n}
      bind $w_ <Control-n> [code $this clone_me_]
      $short_help_win_ add_menu_short_help $File \
         {New window} {Create a new toolbox}

      #  Options to close window.
      $File add command -label {Cancel change and close window   } \
         -command [code $this cancel] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this cancel]
      $File add command -label {Accept change and close window   } \
         -command [code $this accept] \
         -accelerator {Control-a}
      bind $w_ <Control-a> [code $this accept]

      #  Add window help.
      add_help_button system "On Window..."

      #  There are only three elements that can be controlled: epoch,
      #  equinox and system. Each of these defaults to the string
      #  "default" which is taken to mean do not set this option.
      #  The default label (which is always first in the list)
      #  is appended with the string showing the actual value, when a
      #  system is selected.
      itk_component add rule {
         LabelRule $w_.rule -text {Image celestial coordinate system:}
      }

      #  System.
      itk_component add System {
         util::LabelMenu $w_.system \
	       -text "System:" \
	       -relief raised \
	       -labelwidth $lwidth \
               -valuewidth $vwidth
      }
      add_short_help $itk_component(System) \
	    {New celestial coordinate system}
      foreach {system needepoch needequinox} $systemattrib_ {
         $itk_component(System) add \
	       -command [code $this set_system_ system $system $needepoch $needequinox] \
	       -label $system \
	       -value $system
      }
      set system_(system) default

      #  Epoch (date of observation usually).
      itk_component add Epoch {
         LabelEntryMenu $w_.epoch \
	       -text "Epoch:" \
	       -labelwidth $lwidth
      }
      add_short_help $itk_component(Epoch) \
	    {Epoch of coordinates ((B/J)decimal years)}
      foreach epoch $epochmap_ {
         $itk_component(Epoch) add \
	       -label $epoch \
	       -value $epoch
      }
      set system_(epoch) default

      #  Equinox, J2000 or B1950 usually.
      itk_component add Equinox {
         LabelEntryMenu $w_.equinox \
	       -text "Equinox:" \
	       -labelwidth $lwidth
      }
      add_short_help $itk_component(Equinox) \
	    {Equinox of coordinates ((B/J)decimal years)}
      foreach equinox $equinoxmap_ {
         $itk_component(Equinox) add \
	       -label $equinox \
	       -value $equinox
      }
      set system_(equinox) default

      #  Sky reference type for offset coordinate systems.
      itk_component add SkyRefIs {
         util::LabelMenu $w_.skyrefis \
	       -text "Ref point:" \
	       -relief raised \
	       -labelwidth $lwidth \
               -valuewidth $vwidth
      }
      add_short_help $itk_component(SkyRefIs) \
         {Select the type of offset coordinate system}
      foreach sys $skyrefismap_ {
         $itk_component(SkyRefIs) add \
	       -command [code $this set_skyrefis_ $sys] \
	       -label $sys \
	       -value $sys
      }
      set skyrefis_ [lindex $skyrefismap_ 0]

      #  Observers latitude and longitude. Use sla::obs to get known
      #  observatories.
      itk_component add ObsLon {
         LabelEntryMenu $w_.obslon \
            -text "Obs longitude:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -textvariable [scope obslon_]
      }
      add_short_help $itk_component(ObsLon) \
         {The geodetic longitude of the observer, degrees, measured positive eastwards}

      #  Extract the names of all known telescopes. Break the menu into parts
      #  so that we can see the whole thing.
      set more 1
      set n 0
      set ccount 0
      while { $more } {
         incr n
         if { $ccount > 35 } {
            set cbreak 1
            set ccount 0
         } else {
            set cbreak 0
            incr ccount
         }
         lassign [sla::obs $n] name description long lat height
         if { "$description" != "?" } {
            $itk_component(ObsLon) add \
               -label "$description" \
               -value "$long" \
               -columnbreak $cbreak \
               -command [code $this set_lonlat_ $long $lat]
         } else {
            set more 0
         }
      }

      itk_component add ObsLat {
         util::LabelEntry $w_.obslat \
            -text "Obs latitude:" \
            -labelwidth $lwidth \
            -valuewidth $vwidth \
            -textvariable [scope obslat_]
      }
      add_short_help $itk_component(ObsLat) \
         {The geodetic latitude of the observer, degrees}

      #  Should the changed options be applied to the existing frame,
      #  or to a new skyframe. This changes the meaning of "default"
      #  from what the WCS currently has to the builtin AST ones.
      itk_component add NewSystem {
         StarLabelCheck $w_.newsystem \
            -text "New system:" \
            -onvalue 1 \
            -offvalue 0 \
            -labelwidth 15 \
            -variable [scope newsystem_]
      }
      set newsystem_ 0
      add_short_help $itk_component(NewSystem) \
         {Use new coordinate system, otherwise modify current (beware)}

      #  Set the defaults for all the known systems (these are used to
      #  set the labels for the default identifiers).
      foreach {system epoch equinox} $systemmap_ {
         set system_defaults_($system,epoch)   $epoch
         set system_defaults_($system,equinox) $equinox
      }

      #  Set the first system (default all round).
      set system_(equinox) default
      set_system_ system default 1 1

      #  And set the values that system "default" equates on the labels.
      set_system_defaults_

      #  Add buttons to assign the new system to the image, or back out etc.
      #  Create the button bar
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window and accept the new system.
      itk_component add accept {
         button $itk_component(actionframe).accept -text Accept \
	       -command [code $this accept]
      }
      add_short_help $itk_component(accept) \
	    {Accept new coordinate system and close window}

      #  Add a button to test the WCS.
      itk_component add test {
         button $itk_component(actionframe).test -text Test \
	       -command [code $this test]
      }
      add_short_help $itk_component(test) \
	    {Assign new coordinate system to image}

      #  Add a button to close window and not accept the new WCS.
      itk_component add cancel {
         button $itk_component(actionframe).cancel -text Cancel \
	       -command [code $this cancel]
      }
      add_short_help $itk_component(cancel) \
	    {Close window and restore original system}

      #  Add a button to reset the entries and return to the original
      #  image WCS.
      itk_component add reset {
         button $itk_component(actionframe).reset -text Reset \
	       -command [code $this reset_]
      }
      add_short_help $itk_component(reset) \
	    {Reset image and window to defaults}

      #  Pack window.
      pack $itk_component(rule) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(System) -side top -ipadx 1m -ipady 1m -anchor w
      pack $itk_component(Epoch) -side top -ipadx 1m -ipady 1m -anchor w
      pack $itk_component(Equinox) -side top -ipadx 1m -ipady 1m -anchor w
      pack $itk_component(SkyRefIs) -side top -ipadx 1m -ipady 1m -anchor w
      pack $itk_component(ObsLon) -side top -ipadx 1m -ipady 1m -anchor w
      pack $itk_component(ObsLat) -side top -ipadx 1m -ipady 1m -anchor w
      pack $itk_component(NewSystem) -side top -ipadx 1m -ipady 1m -anchor w

      pack $itk_component(actionframe) -side bottom -fill x -pady 3 -padx 3
      pack $itk_component(accept) -side right -expand 1 -pady 1 -padx 1
      pack $itk_component(cancel) -side right -expand 1 -pady 1 -padx 1
      pack $itk_component(reset)  -side right -expand 1 -pady 1 -padx 1
      pack $itk_component(test)   -side right -expand 1 -pady 1 -padx 1
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Withdraw this window without accepting any new WCS information.
   public method cancel {} {

      #  Restore WCS system to the original (if available).
      if { $itk_option(-rtdimage) != {} } {
         catch {
            $itk_option(-rtdimage) astrestore original
         }
	 notify_
      }
      set testing_ 0
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Withdraw window and write new WCS to image -- permanently.
   public method accept {} {
      if { !$testing_ } {
         test
      }
      set testing_ 0
      $itk_option(-rtdimage) astfix
      set_system_defaults_
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Test the current system out.
   public method test {} {
      if { $itk_option(-rtdimage) != {} } {

         #  Use explicitly set system value, otherwise what the image
         #  is using (system default value), unless that's not available
         #  in which case we use nothing and see what happens.
         set system {}
	 if { $system_(system) != "default" } {
            set system "$system_(system)"
	 } else {
            if { $system_defaults_(default,system) != "" } {
               set system [string tolower $system_defaults_(default,system)]
            }
         }
         if { $system != {} } {
            set options "system=$system"
         }

         #  Epoch, if not at default then have explicit value. If default
         #  and the system is default then the epoch is set to the
         #  default epoch, otherwise the default for the current system is
         #  used.
	 set system_(epoch) [$itk_component(Epoch) get]
	 if { $system_(epoch) != "default" } {
	    append options ",epoch=$system_(epoch)"
         } else {
            if { $system_(system) == "default" &&
                 $system_defaults_(default,epoch) != "" } {
               append options ",epoch=$system_defaults_(default,epoch)"
            } elseif { $system_defaults_($system,epoch) != "" } {
               append options ",epoch=$system_defaults_($system,epoch)"
            }
         }

         #  Equinox, same reasoning as above.
	 set system_(equinox) [$itk_component(Equinox) get]
	 if { $system_(equinox) != "default" } {
	    append options ",equinox=$system_(equinox)"
         } else {
            if { $system_(system) == "default" &&
                 $system_defaults_(default,equinox) != "" } {
               append options ",equinox=$system_defaults_(default,equinox)"
            } elseif { $system_defaults_($system,equinox) != "" } {
               append options ",equinox=$system_defaults_($system,equinox)"
            }
         }

         #  Observer longitude and latitude, used for AZEL mainly.
         if { $obslat_ != {} && $obslat_ != "N0:00:00" } {
            append options ",obslat=$obslat_"
         }
         if { $obslon_ != {} && $obslon_ != "E0:00:00" } {
            append options ",obslon=$obslon_"
         }

	 if { $options != {} } {
	    $itk_option(-rtdimage) astsystem image $options 0 $newsystem_
	    $itk_option(-rtdimage) astreplace

            #  SkyRefIs only works afterwards, so do that now.
            $itk_option(-rtdimage) astset skyrefis $skyrefis_

	    notify_

	 } else {
	    if { $testing_ } {
	       #  Back to defaults.
	       $itk_option(-rtdimage) astrestore original
	       notify_
	    }
	 }
      }
   }

   #  Set the value of system and configure epoch and equinox as needed.
   protected method set_system_ {name value needepoch needequinox} {
      set system_($name) $value
      if { ! $needepoch } {
         $itk_component(Epoch) configure -value default
         $itk_component(Epoch) configure -state disabled
      } else {
         $itk_component(Epoch) configure -state normal
      }
      if { ! $needequinox } {
         $itk_component(Equinox) configure -value default
         $itk_component(Equinox) configure -state disabled
      } else {
         $itk_component(Equinox) configure -state normal
      }

      #  Make sure that the default labels are correct.
      set_system_defaults_ 0
   }

   #  Set the SkyRefIs value.
   protected method set_skyrefis_ {value} {
      set skyrefis_ $value
   }

   #  Reset the image and the system controls to their defaults.
   protected method reset_ {} {
      $itk_component(System) configure -value "default"
      set system_(system) default
      $itk_component(Epoch) configure -value "default"
      set system_(epoch) default
      $itk_component(Equinox) configure -value "default"
      set system_(equinox) default
      set_system_ system default 1 1

      #  And set the values that system "default" equates on the labels.
      set_system_defaults_ 0

      #  Restore WCS system to the original (if available).
      if { $itk_option(-rtdimage) != {} } {
         catch {
            $itk_option(-rtdimage) astrestore original
         }
         notify_
      }
      set testing_ 0
   }

   #  Set the labels of the defaults for the default system. Just
   #  done once at initialisation time and when image is modified for
   #  the default system, default epoch and equinox change when
   #  system changes.
   protected method set_system_defaults_ { {modsys 1} } {
      if { $modsys } {
	 set system_defaults_(default,system)  \
	       [$itk_option(-rtdimage) astget system]
      }
      set system_defaults_(default,epoch) \
	    [$itk_option(-rtdimage) astget epoch]
      set system_defaults_(default,equinox) \
	    [$itk_option(-rtdimage) astget equinox]

      set obslat_ [$itk_option(-rtdimage) astget ObsLat]
      set obslon_ [$itk_option(-rtdimage) astget ObsLon]

      #  Update the default labels to reflect the current system (which
      #  may not be the default one).
      if { $modsys } {
	 $itk_component(System).mb.m entryconfigure 0 -label \
	       "default ($system_defaults_(default,system))"
      }
      $itk_component(Epoch).mb.m entryconfigure 0 -label \
	    "default ($system_defaults_($system_(system),epoch))"
      $itk_component(Equinox).mb.m entryconfigure 0 -label \
	    "default ($system_defaults_($system_(system),equinox))"
   }

   #  Set observer longitude and latitude. Values in radians
   #  west (slaObs convention, opposite to AST) and north.
   protected method set_lonlat_ {long lat} {
      set obslon_ [expr -1.0*$long*$rad_to_deg_]
      set obslat_ [expr $lat*$rad_to_deg_]
   }

   #  Update after the image is changed.
   public method image_changed {} {
      set_system_defaults_
      reset_
   }

   #  Do the notify_cmd option if needed.
   protected method notify_ {} {
      if { $itk_option(-notify_cmd) != {} } {
         eval $itk_option(-notify_cmd)
      }
   }

   #  Set whether a new system should be used.
   protected method set_newsystem_ {value} {
      set newsystem_ $value
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of starrtdimage widget.
   itk_option define -rtdimage rtdimage Rtdimage {} {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0 {}

   #  Command to execute when the WCS is changed.
   itk_option define -notify_cmd notify_cmd Notify_Cmd {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Protected variables: (available to instance)
   #  --------------------
   #  Names of all the possible systems we can plot in and their
   #  need for epoch and equinox qualifiers.
   protected variable systemattrib_ \
	 {default 1 1 fk5 0 1 fk4 1 1 fk4-no-e 1 1 gappt 1 0 ecliptic 0 1 \
	 galactic 0 0 supergalactic 0 0 azel 1 0}

   #  Array of the various system names and their default
   #  epochs and equinoxes and the initialising list.
   protected variable systemmap_ \
      {fk5 {} J2000 fk4 B1950 B1950 fk4-no-e B1950 B1950 \
          gappt J2000 {} ecliptic {} J2000 galactic {} {} \
          supergalactic {} {} azel {} {} }
   protected variable system_defaults_

   #  Names of sensible epochs.
   protected variable epochmap_ \
      "default J2000.0 B1950.0 [clock format [clock seconds] -format {%Y-%b-%dT%H:%M:%S}]"

   #  Names of sensible equinoxes.
   protected variable equinoxmap_ \
	 {default J2000.0 B1950.0}

   #  System values.
   protected variable system_

   #  Should a totally new system be created, or should the existing one be
   #  modified.
   protected variable newsystem_ 0

   #  Whether testing new system.
   protected variable testing_ 0

   #  The SkyRefIs setting.
   protected variable skyrefis_

   #  The values of SkyRefIs. First is default.
   protected variable skyrefismap_ "ignored origin pole"

   #  Observers longitude and latitude.
   protected variable obslat_ {}
   protected variable obslon_ {}

   #  Common variables: (shared by all instances)
   #  -----------------

   common rad_to_deg_ 57.295779450887607


#  End of class definition.
}
