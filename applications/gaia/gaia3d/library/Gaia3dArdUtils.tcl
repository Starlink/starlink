#+
#  Name:
#     Gaia3dArdUtils

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Contains utility procedures for handling ARD regions. No instances
#     of this class exist, it just provides namespace wrapping.

#  Copyright:
#     Copyright (C) 2009 Science and Technology Facilities Council
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
#     27-APR-2009 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

itcl::class ::gaia3d::Gaia3dArdUtils {

   #  Extract the arguments from an ARD description of the form shape(a1,a2..).
   public proc get_ard_args {desc} {
      #  Replace all delimeters with spaces.
      regsub -all {\(|,|\)} $desc { } desc

      #  Return all words except first.
      return [lrange $desc 1 end]
   }

   #  Return the ARD region shape from "shape(a1,a2..)".
   public proc get_ard_region {desc} {
      #  Replace all delimeters with spaces.
      regsub -all {\(|,|\)} $desc { } desc

      #  Return first word.
      return [lindex $desc 0]
   }
}
