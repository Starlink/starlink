#+
#  Name:
#     StarArdAnnTool

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Defines a class that extends StarArdTool to include the control
#     of region with annuli.

#  Description:

#  Invocations:
#
#        StarArdAnnTool object_name [configuration options]
#
#     This creates an instance of a StarArdAnnTool object. The return is
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
#     See public variable defintions.

#  Methods:
#     See method definitions.

#  Inheritance:
#     This object inherits StarArdTool.

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
#     24-JUN-1996 (PWD):
#        Original version.
#     16-AUG-1996 (PWD):
#        Converted to itcl2.0.
#     {enter_further_changes_here}

#-

#.

itcl::class gaia::StarArdAnnTool {

   #  Inheritances:
   #  -------------

   inherit gaia::StarArdTool

   #  Constructor:
   #  ------------
   constructor {args} {
      #  Initialise the routine prefix to the use annular versions of
      #  the ARD routines. Remove -scale from argument list.
      regsub {\-scale[\ ]+[^\ ]+} "$args" {} safeargs
      eval StarArdTool::constructor -routine_prefix StarArdAnn $safeargs
   } {
      eval configure $args
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Write the annuli description to the named file.
   public method save_annuli_description {filename} {
      if { $filename != {} } {
         set fid [open $filename w]
         set ok [{*}$object_list_ save_annuli_description $fid]
         ::close $fid
      }
      return $ok
   }

   #  Derive the bounding box the annuli.
   public method bbox_annuli {} {
      if { $object_list_ != {} } {
         return [{*}$object_list_ bbox_annuli]
      }
   }



   #  Configuration options: (public variables)
   #  ----------------------

   #  Whether annuli are displayed or not.
   public variable show_annuli {1} {
      if { $object_list_ != {} } {
         {*}$object_list_ configure -show_annuli $show_annuli
      }
   }

   #  Scale factor for annuli.
   public variable scale {1.5} {
      if { $object_list_ != {} } {
         {*}$object_list_ configure -scale $scale
      }
   }

   #  Protected variables: (available to instance)
   #  --------------------


   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
