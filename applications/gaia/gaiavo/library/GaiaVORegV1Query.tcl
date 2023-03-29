#+
#  Name:
#     GaiaVORegV1Query

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class defining utilities methods for RI1.0.

#  Description:
#     Provides a number of methods that describe the registries
#     interface version 1.0.

#  Invocations:
#
#        GaiaVORegV1Query object_name [configuration options]
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

itcl::class gaiavo::GaiaVORegV1Query {

   #  Singleton entry point:
   #  ----------------------
   proc instance {} {
      if { $instance_ == {} } {
         set instance_ [::gaiavo::GaiaVORegV1Query ::\#auto]
      }
      return $instance_
   }

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

   #  Return the service type.
   public method get_type {} {
      return "RegV1"
   }

   #  Get the default registry endpoints. Returns a list of pairs of values,
   #  a symbolic name followed by the endpoint.
   public method get_registries {} {
      return [::array get registries_]
   }

   #  Add a new registry to the default list. Not peristent.
   public method add_registry {shortname url} {
      set registries_($shortname) $url
   }

   #  Get the "default" registry.
   public method default_registry {} {
      return [list AstroGrid $registries_(AstroGrid)]
   }

   #  Get the standard id for a service type shortname.
   public method get_standard_id {name} {
      return $standardIDs_($name)
   }

   #  Get the default columns to show.
   public method default_columns {} {
      return "$default_columns_"
   }

   #  Extract the access url from a row of values. The headers are the 
   #  names of the associated columns.
   public method get_access_url {headers row} {
      eval lassign \$row $headers
      if { [info exists accessURL] } {
         return $accessURL
      }
      return {}
   }

   #  Extract a name for a service from a row of values. The headers are the
   #  names of the associated columns.
   public method get_name {headers row} {
      eval lassign \$row $headers
      if { [info exists shortName] && $shortName != {} } {
         return $shortName
      }
      if { [info exists title] } {
         return $title
      }
      return {}
   }

   #  Extract the IVOA identifier for the service from a list of headers
   #  and the associated data row.
   public method get_identifier {headers row} {
      eval lassign \$row $headers
      if { [info exists identifier] } {
         return $identifier
      }
      return {}
   }

   #  Configuration options: (public variables)
   #  ----------------------


   #  Common variables: (shared by all instances)
   #  -----------------

   #  The instance of this class.
   protected common instance_ {}

   #  Known RegV1 servers. Needed to boot this process.
   protected common registries_
   set registries_(NVO) "http://nvo.stsci.edu/vor10/ristandardservice.asmx"
   set registries_(AstroGrid) \
      "http://registry.astrogrid.org/astrogrid-registry/services/RegistryQueryv1_0"

   #  Mapping of short service names to their standard ids. Yes these differ
   #  in case from RegTAP.
   protected common standardIDs_
   set standardIDs_(SIAP) "ivo://ivoa.net/std/SIA"
   set standardIDs_(SSAP) "ivo://ivoa.net/std/SSA"
   set standardIDs_(CONE) "ivo://ivoa.net/std/ConeSearch"
   set standardIDs_(TAP)  "ivo://ivoa.net/std/TAP"

   #  Default columns to show in table views.
   protected common default_columns_ "shortName title"

#  End of class definition.
}
