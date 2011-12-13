#+
#  Name:
#     Gaia3dVtk

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class of VTK utility procedures.

#  Description:
#     Defines utility procedures with no connection to an obvious class.

#  Copyright:
#     Copyright (C) 2007 Science and Technology Facilities Council
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
#     PWD: Peter Draper (JAC, Durham University)
#     {enter_new_authors_here}

#  History:
#     03-JUL-2007 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

itcl::class ::gaia3d::Gaia3dVtk {

   #  Inheritances:
   #  -------------

   #  None

   #  Constructor:
   #  ------------
   private constructor {args} {
      package require vtk
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods and procedures:
   #  -----------------------

   #  Release all VTK objects. Should be called during application
   #  closure.
   public proc release_all {} {
      ::vtkCommand DeleteAllObjects
   }

   #  Switch debugging on or off.
   public proc debug { on } {
      if { $on } {
         ::vtkCommand DebugOn
      } else {
         ::vtkCommand DebugOff
      }
   }

   #  Switch global warnings on or off for all objects.
   public proc warnings { on } {
      if { $on } {
         ::vtkObject GlobalWarningDisplayOn
      } else {
         ::vtkObject GlobalWarningDisplayOff
      }
   }

   #  Show all instances of VTK Tcl objects.
   public proc show_all { {brief 1 } } {
      puts "VTK instances:"
      if { $brief } {
         foreach obj [::vtkCommand ListAllInstances] {
            puts "   $obj: [$obj GetClassName]"
         }
      } else {
         foreach obj [::vtkCommand ListAllInstances] {
            puts "   $obj: [$obj Print]"
         }
      }
   }

   #  Show all the methods of a VTK object.
   public proc show_methods {obj} {
      puts "[$obj ListMethods]"
   }

   #  Simple proc to delete a VTK object. Use in callbacks.
   protected proc delete_vtk_object_ {obj} {
      $obj Delete
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Protected variables: (available to instance)
   #  --------------------

   #  Common variables: (shared by all instances)
   #  -----------------


#  End of class definition.
}
