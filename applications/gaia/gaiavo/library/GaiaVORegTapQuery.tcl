#+
#  Name:
#     GaiaVORegTapQuery

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class of utility methods for making RegTAP queries.

#  Description:
#     Provides a number of methods that can be used to construct
#     ADQL queries for making RegTAP queries. The methods are meant
#     to cover the standard cases, i.e. TAP services, Registries,
#     SIA servers etc.

#  Invocations:
#
#        GaiaVORegTapQuery object_name [configuration options]
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
#        Original version. Based on RegTapRegistryQuery.java from STARJAVA and
#        the RegTAP example queries.
#     {enter_further_changes_here}

#-

#.

itcl::class gaiavo::GaiaVORegTapQuery {

   #  Singleton entry point:
   #  ----------------------
   proc instance {} {
      if { $instance_ == {} } {
         set instance_ [GaiaVORegTapQuery ::\#auto]
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
      return "RegTAP"
   }

   #  Get ADQL for list of TAP servers.
   public method get_taps_query {{substring {}}} {
      return [get_servers ivo://ivoa.net/std/tap $substring]
   }

   #  Get ADQL for list of conesearch servers.
   public method get_cones_query {{substring {}}} {
      return [get_servers ivo://ivoa.net/std/ConeSearch $substring]
   }

   #  Get ADQL to get list of SIA servers, qualified by some optional string
   #  that should be in a descriptive content element. 
   public method get_sias_query {{substring {}}} {
      return [get_servers ivo://ivoa.net/std/sia $substring]
   }

   #  Get ADQL to get list of registries.
   public method get_registries_query {} {
      set query {}
      append query "SELECT access_url "
      append query "FROM rr.interface "
      append query "NATURAL JOIN rr.capability "
      append query "NATURAL JOIN rr.res_detail "
      append query   "WHERE standard_id='ivo://ivoa.net/std/tap' "
      append query   "AND intf_type='vs:paramhttp' "
      append query   "AND detail_xpath='/capability/dataModel/@ivo-id' "
      append query   "AND 1=ivo_nocasematch(detail_value, 'ivo://ivoa.net/std/regtap#1.0)'"
      return $query
   }


   #  Get ADQL to get list of servers with the given standard_id, qualified by
   #  some optional string that should be in a descriptive content element. 
   #  XXX only apply subtring to some elements.
   public method get_servers_query {standard_id {substring {}}} {
      set query {}
      append query "SELECT ivoid, short_name, res_title, reference_url, "
      append query    "intf_index, access_url, standard_id, cap_type, cap_description, "
      append query    "std_version, res_subjects "
      append query "FROM rr.resource AS res "
      append query "NATURAL JOIN rr.interface "
      append query "NATURAL JOIN rr.capability "
      append query "NATURAL LEFT OUTER JOIN "
      append query "(SELECT ivoid, ivo_string_agg(res_subject, ', ') AS res_subjects FROM rr.res_subject GROUP BY ivoid) AS sbj "
      append query "WHERE "
      append query   "standard_id='${standard_id}' AND "
      append query   "intf_type='vs:paramhttp'"
      if { $substring != {} } {
         append query " AND "
         append query "(("
         append query    "1=ivo_nocasematch(short_name, '%${substring}%') OR "
         append query    "1=ivo_hasword(res_title, '${substring}') OR "
         append query    "1=ivo_hasword(res_subjects, '${substring}') OR "
         append query    "1=ivo_nocasematch(ivoid, '%${substring}%') OR "
         append query    "1=ivo_hasword(res_description, '${substring}') "
         append query "))"
      }
      return $query
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
      return [list GAVO_AIP $registries_(GAVO_AIP)]
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
      if { [info exists access_url] } {
         return $access_url
      }
      return {}
   }

   #  Extract a name for a service from a row of values. The headers are the
   #  names of the associated columns.
   public method get_name {headers row} {
      eval lassign \$row $headers
      if { [info exists short_name] && $short_name != {} } {
         return $short_name
      }
      if { [info exists res_title] } {
         return $res_title
      }
      return {}
   }

   #  Extract the IVOA identifier for the service from a list of headers
   #  and the associated data row.
   public method get_identifier {headers row} {
      eval lassign \$row $headers
      if { [info exists ivoid] } {
         return $ivoid
      }
      return {}
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Common variables: (shared by all instances)
   #  -----------------

   #  The instance of this class.
   protected common instance_ {}

   #  Known RegTAP servers. Needed to boot this process.
   protected common registries_

   #  TAP endpoint for GAVO registry hosted at AIP.
   set registries_(GAVO_AIP) "http://gavo.aip.de/__system__/tap/run/tap"

   #  TAP endpoint for GAVO registry currently hosted at ARI Heidelberg
   set registries_(GAVO_ARI) "http://dc.zah.uni-heidelberg.de/__system__/tap/run/tap"

   #  Mapping of short service names to their standard ids. Yes these differ
   #  in case from RI1.0...
   protected common standardIDs_
   set standardIDs_(SIAP) "ivo://ivoa.net/std/sia"
   set standardIDs_(SSAP) "ivo://ivoa.net/std/ssa"
   set standardIDs_(CONE) "ivo://ivoa.net/std/conesearch"
   set standardIDs_(TAP)  "ivo://ivoa.net/std/tap"

   #  Default columns to show in table views.
   protected common default_columns_ "short_name res_title"

   #  Useful columns.
   protected common columns_  \
      "ivoid short_name res_title reference_url intf_index access_url standard_id \
cap_type cap_description std_version res_subjects"

#  End of class definition.
}
