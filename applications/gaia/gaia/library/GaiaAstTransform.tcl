#+
#  Name:
#     GaiaAstTransform

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Transforms a set of RA/Dec positions from a specified celestial
#     coordinate system to that of a displayed rtdimage.

#  Description:
#     This toolbox provides the ability to transform a set of sky
#     coordinates (the coordinate system being specified using a
#     system, epoch and equinox) to that of a displayed image.  This
#     class is intended for use in transforming input coordinates (say
#     from external catalogue or text files) to the same celestial
#     coordinates as the image.

#  Invocations:
#
#        GaiaAstTransform object_name [configuration options]
#
#     This creates an instance of a GaiaAstTransform object. The return is
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
#     See itk_option define statements below.

#  Methods:
#     See method declaration below.

#  Inheritance:
#     This object inherits no other classes.

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
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
#     08-MAY-2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaAstTransform {}

itcl::class gaia::GaiaAstTransform {

   #  Inheritances:
   #  -------------
   inherit util::TopLevelWidget

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options.
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ \
         "GAIA: transform sky coordinates ($itk_option(-number))"

      #  Create the short help window.
      make_short_help

      #  Add the File menu.
      add_menubar
      set File [add_menubutton "File"]
      configure_menubutton File -underline 0
      add_short_help $itk_component(menubar).file {File menu: close window}

      #  Set the exit menu items.
      $File add command -label {Cancel changes and close window   } \
         -command [code $this cancel] \
         -accelerator {Control-c}
      bind $w_ <Control-c> [code $this cancel]

      $File add command -label {Accept changes and close window   } \
         -command [code $this accept] \
         -accelerator {Control-a}
      bind $w_ <Control-a> [code $this accept]

      #  Add window help.
      add_help_button transform "On Window..."

      #  Markers menu
      set Markers [add_menubutton Markers]

      #  Add a table for displaying the reference coordinates. These
      #  are not modified and are transformed to show in the second
      #  table. Note we only use id, ra and dec.
      itk_component add reftable {
         util::TableList $w_.reftable \
            -title "Reference positions (table coordinates)" \
             -hscroll 1 \
             -selectmode extended \
             -exportselection 0 \
             -headings {id ra dec} \
             -width 40
      }
      add_short_help $itk_component(reftable) \
         {Reference positions on sky}

      #  Add the table for displaying the transformed coordinates
      #  positions. Most of the controls are disabled and only the
      #  Marker menu is used in this case.
      itk_component add trantable {
         GaiaPosTable $w_.trantable \
            -title "Transformed positions (image coordinates)" \
            -editmenu {} \
            -markmenu $Markers \
            -rtdimage $itk_option(-rtdimage) \
            -canvas $itk_option(-canvas) \
            -image $itk_option(-image) \
            -editcontrols 0 \
            -mcolour red
      }
      add_short_help $itk_component(trantable) \
         {Transformed positions and associated X,Y (reset to update)}

      #  There are only three elements that can be controlled: epoch,
      #  equinox and system. Each of these defaults to the string
      #  "default" which is taken to mean do not set this option.
      #  The default label (which is always first in the list)
      #  is appended with the string showing the actual value, when a
      #  system is selected.
      itk_component add rule {
         gaia::LabelRule $w_.rule -text "Reference celestial coordinate system:"
      }

      #  System.
      itk_component add System {
         util::LabelMenu $w_.system \
	       -text "System:" \
	       -relief raised \
	       -labelwidth 8 \
               -valuewidth 18
      }
      add_short_help $itk_component(System) \
	    {Coordinate system of table sky positions}
      foreach {system needepoch needequinox} $systemattrib_ {
         $itk_component(System) add \
	       -command [code $this set_system_ system $system $needepoch $needequinox] \
	       -label $system \
	       -value $system
      }
      set system_(system) default

      #  Epoch (date of observation usually).
      itk_component add Epoch {
         gaia::LabelEntryMenu $w_.epoch \
	       -text "Epoch:" \
	       -labelwidth 8
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
         gaia::LabelEntryMenu $w_.equinox \
	       -text "Equinox:" \
	       -labelwidth 8
      }
      add_short_help $itk_component(Equinox) \
	    {Equinox of coordinates ((B/J)decimal years)}
      foreach equinox $equinoxmap_ {
         $itk_component(Equinox) add \
	       -label $equinox \
	       -value $equinox
      }
      set system_(equinox) default

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

      #  Add a button to close window and accept the changed coordinates
      itk_component add accept {
         button $itk_component(actionframe).accept -text Accept \
	       -command [code $this accept]
      }
      add_short_help $itk_component(accept) \
	    {Accept transformed coordinates and close window}

      #  Add a button to test the transformation.
      itk_component add test {
         button $itk_component(actionframe).test -text Test \
	       -command [code $this test]
      }
      add_short_help $itk_component(test) \
	    {Test celestial coordinate transformation and redraw}

      #  Add a button to close window and not accept the transformations.
      itk_component add cancel {
         button $itk_component(actionframe).cancel -text Cancel \
	       -command [code $this cancel]
      }
      add_short_help $itk_component(cancel) \
	    {Ignored transformed positions and close window}

      #  Add a button to reset the entries.
      itk_component add reset {
         button $itk_component(actionframe).reset -text Reset \
	       -command [code $this reset_]
      }
      add_short_help $itk_component(reset) \
	    {Reset system, equinox and epoch to defaults}

      #  Pack window.
      pack $itk_component(reftable) -side top -fill both -expand 1 \
         -ipadx 1m -ipady 1m
      pack $itk_component(trantable) -side top -fill both -expand 1 \
         -ipadx 1m -ipady 1m
      pack $itk_component(rule) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(System) -side top -ipadx 1m -ipady 1m -anchor w
      pack $itk_component(Epoch) -side top -ipadx 1m -ipady 1m -anchor w
      pack $itk_component(Equinox) -side top -ipadx 1m -ipady 1m -anchor w

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

   #  Close window accepting changes.
   public method accept {} {
      wm withdraw $w_
      $itk_component(trantable) undraw
      set accepted_ 1
   }

   #  Close window cancelling changes.
   public method cancel {} {
      wm withdraw $w_
      $itk_component(trantable) undraw
      set accepted_ 0
   }

   #  Copy coordinates from an existing instance of a GaiaPosTable.
   #  Note this object is also used to reset coordinates.
   public method grab {table} {
      $itk_component(reftable) clear
      $itk_component(trantable) clear_table
      foreach line [$table get_contents] {
         lassign $line id ra dec x y
         $itk_component(reftable) append_row "$id $ra $dec"
         $itk_component(trantable) append_row $line
      }
      $itk_component(reftable) new_info
      $itk_component(trantable) new_info
      $itk_component(trantable) redraw
   }

   #  Activate (i.e. wait for user to request interface to complete).
   #  Returns true if changes are accepted, false otherwise.
   #  Note window is only withdrawn on exit, you must delete it to
   #  really get rid of it (needed to retain table state until read).
   public method activate {} {
      wm deiconify $w_
      tkwait variable [scope accepted_]
      return $accepted_
   }

   #  Return the contents of the transformed table.
   public method get_contents {} {
      return [$itk_component(trantable) get_contents]
   }

   #  Test the transformation by applying it and viewing the new X and
   #  Y coordinates. Rtdimage does most of the work here.
   public method test {} {
      if { $itk_option(-rtdimage) != {} } {
	 set options {}
	 if { $system_(system) != "default" } {
	    append options "system=$system_(system)"
	 }
         set system_(epoch) [$itk_component(Epoch) get]
         if { $system_(epoch) != "default" } {
            if { $options == "" } {
               append options "epoch=$system_(epoch)"
            } else {
               append options ",epoch=$system_(epoch)"
            }
         }
	 set system_(equinox) [$itk_component(Equinox) get]
	 if { $system_(equinox) != "default" } {
            if { $options == "" } {
               append options "equinox=$system_(equinox)"
            } else {
               append options ",equinox=$system_(equinox)"
            }
	 }

	 if { $options != {} } {

            #  Create AstFrameSet that describes the mapping from this
            #  SkyFrame to that of the image.
	    $itk_option(-rtdimage) astsystem image $options 1

            #  Use the local transformation to transform the table
            #  positions to the image celestial coordinates.
            set coords ""
            set content [$itk_component(reftable) get_contents]
            set nrows [$itk_component(reftable) total_rows]
            for { set i 0 } { $i < $nrows } { incr i } {
               lassign [lindex $content $i] id ra dec
               append coords "$ra $dec "
            }
            set result [$itk_option(-rtdimage) asttran2 local $coords]
            set newcon ""
            set i 0
            if { $result != {} } {
               $itk_component(trantable) clear_table
               foreach {ra dec} $result {
                  lassign [lindex $content $i] id oldra olddec
                  lappend newcon [list $id $ra $dec 0 0]
                  incr i
               }

               eval $itk_component(trantable) set_contents $newcon
               $itk_component(trantable) update_x_and_y
            }
         } else {

            #  Options set to "default", so need to unset transformed
            #  positions.
            reset_trantable_
         }
      }
   }

   #  Reset trantable to echo reference positions.
   protected method reset_trantable_ {} {
      $itk_component(trantable) clear_table
      foreach line [$itk_component(reftable) get_contents] {
         $itk_component(trantable) append_row "$line 0 0"
      }
      $itk_component(trantable) new_info
      $itk_component(trantable) update_x_and_y
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

   #  Reset controls and transformed positions.
   protected method reset_ {} {
      $itk_component(System) configure -value "default"
      set system_(system) default
      $itk_component(Epoch) configure -value "default"
      set system_(epoch) default
      $itk_component(Equinox) configure -value "default"
      set system_(equinox) default
      set_system_ system default 1 1
      reset_trantable_
   }

   #  Configuration options: (public variables)
   #  ----------------------
   #  Name of starrtdimage widget.
   itk_option define -rtdimage rtdimage RtdImage {}

   #  Name of the canvas holding the starrtdimage widget.
   itk_option define -canvas canvas Canvas {}

   #  Name of the RtdImage widget or derived class.
   itk_option define -image image Image {}

   #  Protected variables: (available to instance)
   #  --------------------

   #  Widths of various fields.
   protected variable vwidth_ 25
   protected variable lwidth_ 16

   #  Whether changes are accepted or not when closed.
   protected variable accepted_ 0

   #  Names of all the possible systems we can plot in and their
   #  need for epoch and equinox qualifiers.
   protected variable systemattrib_ \
	 {default 1 1 fk5 0 1 fk4 1 1 fk4-no-e 1 1 gappt 1 0 ecliptic 0 1 \
	 galactic 0 0 supergalactic 0 0}

   #  Array of the various system names and their default
   #  epochs and equinoxes and the initialising list.
   protected variable systemmap_ \
      {fk5 {} J2000 fk4 B1950 B1950 fk4-no-e B1950 B1950 \
          gappt J2000 {} ecliptic {} J2000 galactic {} {} \
          supergalactic {} {}}
   protected variable system_defaults_

   #  Names of sensible epochs.
   protected variable epochmap_ \
	 "default J2000.0 B1950.0 [clock format [clock seconds] -format {%Y-%b-%d}]"

   #  Names of sensible equinoxes.
   protected variable equinoxmap_ \
	 {default J2000.0 B1950.0}

   #  System values.
   protected variable system_

   #  Common variables: (shared by all instances)
   #  -----------------

   #  Variable to share amongst all widgets. This is indexed by the
   #  object name ($this)
   common values_

#  End of class definition.
}
