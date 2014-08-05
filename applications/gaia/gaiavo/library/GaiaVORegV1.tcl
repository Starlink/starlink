#+
#  Name:
#     GaiaVORegV1

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Utilities methods for RI1.0.

#  Description:
#     Provides a number of methods that describe the registries
#     interface version 1.0.

#  Invocations:
#
#        GaiaVORegV1 object_name [configuration options]
#
#     This creates an instance of a GaiaTAPQuery object. The return is
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

#  Copyright:
#     Copyright (C) 2014 Science and Technology Facilities Council
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
#     04-AUG-2014 (PWD):
#        Original version. 
#     {enter_further_changes_here}

#-

#.

itcl::class gaiavo::GaiaVORegV1 {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      #  Nothing to do, just a holder for static methods.
   }

   #  Destructor:
   #  -----------
   destructor  {
   }

   #  Methods:
   #  --------

   #  Get the default registry endpoints. Returns a list of pairs of values,
   #  a symbolic name followed by the endpoint.
   public proc get_registries {} {
      return [::array get registries_]
   }

   #  Add a new registry to the default list. Not peristent.
   public proc add_registry {shortname url} {
      set registries_($shortname) $url
   }

   #  Get the "default" registry.
   public proc default_registry {} {
      return [list AstroGrid $registries_(AstroGrid)]
   }

   #  Configuration options: (public variables)
   #  ----------------------


   #  Common variables: (shared by all instances)
   #  -----------------

   #  Known RegV1 servers. Needed to boot this process.
   protected common registries_
   set registries_(NVO) "http://nvo.stsci.edu/vor10/ristandardservice.asmx"
   set registries_(AstroGrid) \
      "http://registry.astrogrid.org/astrogrid-registry/services/RegistryQueryv1_0"

#  End of class definition.
}
