#+
#  Name:
#     GaiaVORegTapQuery

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Class of utility methods for making RegTAP queries.

#  Description:
#     Provides a number of methods that can be used to construct
#     AQDL queries for making RegTAP queries. The methods are meant
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

   #  Get ADQL for list of TAP servers.
   public proc get_taps {{substring {}}} {
      return [get_servers ivo://ivoa.net/std/tap $substring]
   }

   #  Get ADQL for list of conesearch servers.
   public proc get_taps {{substring {}}} {
      return [get_servers ivo://ivoa.net/std/ConeSearch $substring]
   }

   #  Get ADQL to get list of SIA servers, qualified by some optional string
   #  that should be in a descriptive content element. 
   public proc get_sias {{substring {}}} {
      return [get_servers ivo://ivoa.net/std/sia $substring]
   }

   #  Get ADQL to get list of registries.
   public proc get_registries {} {
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
   public proc get_servers {standard_id {substring {}}} {
      set query {}
      append query "SELECT ivoid, short_name, res_title, reference_url, base_role, role_name, "
      append query    "email, intf_index, access_url, standard_id, cap_type, cap_description, "
      append query    "std_version, res_subjects "
      append query "FROM rr.resource AS res "
      append query "NATURAL JOIN rr.interface "
      append query "NATURAL JOIN rr.capability "
      append query "NATURAL LEFT OUTER JOIN rr.res_role "
      append query "NATURAL LEFT OUTER JOIN "
      append query "(SELECT ivoid, ivo_string_agg(res_subject, ', ') AS res_subjects FROM rr.res_subject GROUP BY ivoid) AS sbj "
      append query "WHERE "
      append query   "(base_role='contact' OR base_role='publisher' OR  base_role IS NULL ) AND "
      append query   "standard_id='${standard_id}' AND "
      append query   "intf_type='vs:paramhttp'"
      if { $substring != {} } {
         append query " AND "
         append query "(("
         append query    "1=ivo_nocasematch(short_name, '%${substring}%') OR "
         append query    "1=ivo_hasword(res_title, '${substring}') OR "
         append query    "1=ivo_hasword(res_subjects, '${substring}') OR "
         append query    "1=ivo_nocasematch(ivoid, '%${substring}%') OR "
         append query    "(base_role='publisher' AND 1=ivo_nocasematch(role_name,'%${substring}%')) OR "
         append query    "1=ivo_hasword(res_description, '${substring}') "
         append query "))"
      }
      return $query
   }
   
   #  Configuration options: (public variables)
   #  ----------------------


   #  Common variables: (shared by all instances)
   #  -----------------

   #  Known RegTAP servers. Needed to boot this process.
   public common registries

   #  TAP endpoint for GAVO registry currently hosted at ARI Heidelberg
   set registries(ARI) "http://dc.g-vo.org/tap"

   #  TAP endpoint for GAVO registry hosted at AIP.
   set registries(AIP) "http://gavo.aip.de/tap"


#  End of class definition.
}
