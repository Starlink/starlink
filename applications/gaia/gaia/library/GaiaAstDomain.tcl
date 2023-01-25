#+
#  Name:
#     GaiaAstDomain

#  Type of Module:
#     [incr Tk] class

#  Purpose:
#     Creates a toolbox for changing the domain (i.e. coordinate
#     system) displayed for an image.

#  Description:
#     This class creates a window that contains controls for changing
#     to one of the available image domains.

#  Invocations:
#
#        GaiaAstDomain object_name [configuration options]
#
#     This creates an instance of a GaiaAstDomain object. The return is
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
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     Copyright (C) 2008 Science and Technology Facilities Council.
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
#     08-DEC-2000 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itk::usual GaiaAstDomain {}

itcl::class gaia::GaiaAstDomain {

   #  Inheritances:
   #  -------------

   inherit util::TopLevelWidget

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Evaluate any options
      eval itk_initialize $args

      #  Set the top-level window title.
      wm title $w_ "GAIA: image built-in coordinate systems ($itk_option(-number))"

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
      add_help_button domain "On Window..."

      #  Only one element is available. A dropdown box showing the AST
      #  domains for the current image.
      itk_component add rule {
         LabelRule $w_.rule -text {Built-in coordinate systems:}
      }

      #  System.
      itk_component add System {
         util::LabelMenu $w_.system \
               -text "System:" \
               -relief raised \
               -labelwidth 8 \
               -valuewidth 30
      }
      add_short_help $itk_component(System) \
         {Select the built-in coordinate system}

      #  Add buttons to keep the change, reset it to the original or
      #  cancel the change.
      itk_component add actionframe {frame $w_.action}

      #  Add a button to close window and accept the new system.
      itk_component add accept {
         button $itk_component(actionframe).accept -text Accept \
               -command [code $this accept]
      }
      add_short_help $itk_component(accept) \
            {Accept new coordinate system and close window}

      #  Add a button to close window and not accept the new WCS.
      itk_component add cancel {
         button $itk_component(actionframe).cancel -text Cancel \
               -command [code $this cancel]
      }
      add_short_help $itk_component(cancel) \
            {Close window and restore original system}

      #  Add a button to reset to the original image domain.
      itk_component add reset {
         button $itk_component(actionframe).reset -text Reset \
               -command [code $this reset_]
      }
      add_short_help $itk_component(reset) \
            {Reset to default system}

      #  Pack window.
      pack $itk_component(rule) -side top -fill x -ipadx 1m -ipady 1m
      pack $itk_component(System) -side top -ipadx 1m -ipady 1m -anchor w \
         -fill x -expand 1

      pack $itk_component(actionframe) -side bottom -fill x -pady 3 -padx 3
      pack $itk_component(accept) -side right -expand 1 -pady 1 -padx 1
      pack $itk_component(cancel) -side right -expand 1 -pady 1 -padx 1
      pack $itk_component(reset)  -side right -expand 1 -pady 1 -padx 1
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Called after UI is completed. Do tweaks to help and labels.
   public method init {} {
      #  Set up the system menu. Also defines the default state.
      set_system_menu_
   }

   #  Create a new instance of this object.
   protected method clone_me_ {} {
      if { $itk_option(-clone_cmd) != {} } {
         eval $itk_option(-clone_cmd)
      }
   }

   #  Withdraw this window without accepting any new WCS information.
   public method cancel {} {

      #  Restore WCS system to the original (if available).
      reset_
      notify_
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Withdraw window and make the selected domain permanent.
   public method accept {} {
      if { $itk_option(-really_die) } {
         delete object $this
      } else {
         wm withdraw $w_
      }
   }

   #  Change the current domain. The iframe value is the index of the frame
   #  that is to be selected.
   public method set_domain {iframe} {
      if { [catch {
         set_current_domain_ $iframe
         notify_
      } msg ] } {
         error_dialog "Failed to switch coordinates: $msg"
      }
   }

   #  Reset the domain back to the default.
   protected method reset_ {} {
      set_domain $original_
      set_menu_default_
   }

   #  Set/reset system menu to the default value.
   protected method set_menu_default_ {} {
      $itk_component(System) configure -value $original_
   }

   #  Find out the available image domains and add these to the system
   #  choice menu (overrides any existing choices).
   protected method set_system_menu_ {} {

      #  Clear any existing values.
      $itk_component(System) clear

      #  Get the current domain.
      get_current_domain_

      #  Add the available domains. Do not pick duplicates and mark
      #  the default.
      get_domains_
      set ndomains [llength $domains_]
      for {set i 0} {$i < $ndomains} {incr i} {
         set system [lindex $domains_ $i]
         set index [expr $i+1]
         set label $system
         if { $index == $original_ } {
            append label " (default)"
         }
         $itk_component(System) add \
            -command [code $this set_domain $index] \
            -label $label \
            -value $index
         set added($system) 1
      }
      set_menu_default_
   }

   #  Method to call when the displayed image changes.
   public method image_changed {} {
      set_system_menu_
   }

   #  Do the notify_cmd option if needed.
   protected method notify_ {} {
      if { $itk_option(-notify_cmd) != {} } {
         eval $itk_option(-notify_cmd)
      }
   }

   #  Methods that should be overridden by subclasses not using an rtdimage.
   #  ======================================================================

   #  Set the current domain.
   protected method set_current_domain_ {iframe} {
      $itk_option(-rtdimage) astset current $iframe
   }

   #  Get the current domain and assign to original_
   protected method get_current_domain_ {} {
      set original_ [$itk_option(-rtdimage) astget current]
      if { $original_ == {} } {
         set original_ 1
      }
   }

   #  Get a list of the currently available domains.
   protected method get_domains_ {} {
      set domains_ [$itk_option(-rtdimage) astdomains]
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Name of rtdimage widget, must be defined.
   itk_option define -rtdimage rtdimage Rtdimage {}

   #  Identifying number for toolbox (shown in () in window title).
   itk_option define -number number Number 0

   #  Command to execute when the WCS is changed.
   itk_option define -notify_cmd notify_cmd Notify_Cmd {}

   #  Command to execute to create a new instance of this object.
   itk_option define -clone_cmd clone_cmd Clone_Cmd {}

   #  If this is a clone, then it should die rather than be withdrawn.
   itk_option define -really_die really_die Really_Die 0

   #  Protected variables: (available to instance)
   #  --------------------

   #  List of domains that are available.
   protected variable domains_ {}

   #  The default domain AST index.
   protected variable original_ 1

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
