#+
#  Name:
#     StarArdPrim

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Provides a base class for implementing common ARD region related
#     operations and for storing ARD related information.

#  Description:
#     This class provides methods and public variables for performing
#     common ARD operations and for storing ARD information that is
#     not directly related to the representation of the region
#     (c.f. the region binary and unary operators).

#  Invocations:
#
#        StarArdPrim object_name [configuration options]
#
#     This creates an instance of a StarArdPrim object. The return is
#     the name of the object. This class should not normally be
#     directly accessed in this way.
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
#     binary_operator value
#       This configuation option stores the binary operator associated
#       with the current region. Note this can be set by the
#       check_description method.
#
#     unary_operator value
#        This configuration option stores the unary operator
#        associated with the current region. This should only be set to
#        .NOT. or "". A minus sign is recognised as .NOT.. Note this
#        can be set by the check_description method.

#  Methods:
#     check_description region_name desc
#        Checks that a description is a valid one for the type of ARD
#        region given. The return is true or false. The result of this
#        operation is to set the values of the public operators and to
#        set the protected variable "qualifiers_" to the values
#        following the keyword (without the parenthesis and commas).
#
#     show_properties top-level
#
#        Creates a top-level widget for containing widgets that
#        describe the properties of the ARD region and also allow them
#        to be changed

#  Notes:
#     unary operators disabled as not used yet.

#  Inheritance:
#     This widget inherits StarCanvasObject.

#  Copyright:
#     Copyright (C) 1998 Central Laboratory of the Research Councils
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
#     14-MAY-1996 (PWD):
#        Original version.
#     5-JUL-1996 (PWD):
#        Converted to itcl2.0.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarArdPrim {

   #  Inheritances:
   #  -------------

   inherit gaia::StarCanvasObject

   #  Constructor:
   #  ------------
   constructor {args} {

      #  Process args.
      eval configure $args

      #  All ARD regions should supply a show_properties method to
      #  fullfil this request.
      set show_properties_cmd [code $this show_properties]

      #  Get notification of any changes to the region.
      set notify_change_cmd [code $this properties_changed]
   }

   #  Destructor:
   #  -----------
   destructor  {
      if { [winfo exists $Properties_] } {
         delete object $Properties_
         unset notify_created_cmd_
         unset notify_update_cmd_
      }
   }

   #  Methods:
   #  --------

   #  Check that a description is valid and parse it to extract the
   #  qualifiers and any operators. Note the region name match in just
   #  done on the first three characters which the ARD specification says
   #  are unique.
   method check_description {region_name desc} {
      set short_name [string range "[string toupper $region_name]" 0 2]
      set local_desc [string toupper $desc]
      if { [regexp "(.*)$short_name\[^\(\]*(\[^\)\]*)" $local_desc \
               match before after] } {
         set qualifiers_ [split [string range $after 1 end] {,}]
         if { $before != {} } {
            set binary_operator {}
            set unary_operator {}
            regexp {[\ ]*(\.OR\.|\.AND\.|\.EQV\.|\.XOR\.)*[\ ]*(\.NOT\.|-)*} \
               $before match binary_operator unary_operator
            if { $unary_operator == {-} } {
               set unary_operator {.NOT.}
            }
         }
         return 1
      } else {
         return 0
      }
   }

   #  Create a top-level widget for containing the region
   #  description. If the window already exists then a false return is
   #  made.
   method create_properties_window {name} {
      set exists 0
      if { ! [winfo exists $Properties_] } {
         set Properties_ [util::TopLevelWidget $name]
         wm title $name "ARD Properties ($this)"

         if { [info exists mode] } {
            #  Add label for type of region. Note this relies on "mode"
            #  which is inherited indirectly from StarCanvasObject.
            label $Properties_.label -text "[string toupper  $mode]"
            frame $Properties_.line -height 3
            pack $Properties_.label -anchor c
            pack $Properties_.line -fill x
         }

         #  Add a check button for toggling the include/exclude nature of the
         #  region.
         set Frame_ [frame $Properties_.frame]
         #         set Exclude_ [StarLabelCheck $Frame_.exclude \
         #                          -text {Exclude region:} \
         #                          -labelwidth $labelwidth_ \
         #                          -command [code $this toggle_unary_operator]]

         #  Add a button to close the window (actually just withdraws
         #  it).
         set Close_ [button $Properties_.close -text {Close} \
                        -command [code $Properties_ quit]]

         pack $Frame_ -side top -fill both -expand true
         #         pack $Exclude_ -side top -fill x
         pack $Close_ -side bottom -fill x
      } else {

         #  Raise the window to make sure it is visible.
         wm deiconify $Properties_
         raise $Properties_
         set exists 1
      }

      #  Update check button with current properties.
      #      if { $unary_operator == {} } {
      #         $Exclude_ select
      #      } else {
      #         $Exclude_ deselect
      #      }
      return $exists
   }

   #  Receive notification of updates to the objects and dispatch
   #  these to the derived classes. The main ARD region class should
   #  have a method update_properties to deal with this
   #  request. Notification is also dispatched via the
   #  notify_update_cmd, which may be used for additional purposes
   #  (such as annular region updates).
   method properties_changed {} {
      update_properties
      if { $notify_update_cmd_ != {} } {
         eval $notify_update_cmd_
      }
   }

   #  Dummy method for update_properties, real one is virtual.
   method update_properties {} {}

   #  Toggle the value of the unary operator.
   method toggle_unary_operator {} {
      if { [winfo exists $Properties_] } {
         if { $unary_operator == {} } {
            $Exclude_ deselect
            set unary_operator {.NOT.}
         } else {
            $Exclude_ select
            set unary_operator {}
         }
      }
   }

   #  Create an item and allow it to be resized (this appends
   #  notify_created_cmd_ if set to allow derived classes to see this
   #  event as well as the user of the class.
   method create_and_resize {cmd} {
      if { $notify_created_cmd_ == {} } {
         gaia::StarCanvasObject::create_and_resize $cmd
      } else {
         if { $cmd == {} } {
            gaia::StarCanvasObject::create_and_resize "$notify_created_cmd_"
         } else {
            gaia::StarCanvasObject::create_and_resize "$notify_created_cmd_;$cmd"
         }
      }
   }

   #  Create an item using the current coordinates.
   method create_no_resize {cmd tcoords} {
      if { $notify_created_cmd_ == {} } {
         gaia::StarCanvasObject::create_no_resize $cmd $coords
      } else {
         if { $cmd == {} } {
            gaia::StarCanvasObject::create_no_resize "$notify_created_cmd_" $coords
         } else {
            gaia::StarCanvasObject::create_no_resize "$notify_created_cmd_;$cmd" $coords
         }
      }
   }

   #  Return the ARD region shape from "shape(a1,a2..)".
   public proc get_ard_region {desc} {
      #  Replace all delimeters with spaces.
      regsub -all {\(|,|\)} $desc { } desc

      #  Return first word.
      return [lindex $desc 0]
   }

   #  Configuration options: (public variables)
   #  ----------------------
   public variable unary_operator {} {}
   public variable binary_operator {.OR.} {}

   #  Command to execute when updates occur. Subclasses are expected to
   #  access this directly (historical reasons).
   public variable notify_update_cmd {} {
      set notify_update_cmd_ $notify_update_cmd
   }

   #  Protected variables: (available to instance)
   #  --------------------

   #  The qualifiers extracted from an ARD description.
   protected variable qualifiers_ {}

   #  The name of the various widgets created.
   protected variable Properties_ {}
   protected variable Exclude_ {}
   protected variable Close_ {}
   protected variable Frame_ {}

   #  Width of any labels (used for alignment).
   protected variable labelwidth_ 14

   #  Commands for derived classes to use as hooks into creation,
   #  modification and deletion of an ARD item.
   protected variable notify_created_cmd_ {}
   protected variable notify_update_cmd_ {}


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
