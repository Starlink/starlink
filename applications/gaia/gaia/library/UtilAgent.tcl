#+

#  Name:
#     UtilAgent

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Implemenation of generic SAMP utility MTypes.

#  Description:
#     Does the work for responding to some generic utility SAMP MTypes.
#     It should be installed in a samp::SampClient.

#  Copyright:
#     Copyright (C) 2009 Science and Technology Facilities Council.
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
#     MBT: Mark Taylor
#     {enter_new_authors_here}

#  History:
#     26-JUN-2009 (MBT):
#        Original version.
#     {enter_further_changes_here}

#-

itcl::class samp::UtilAgent {

   #  Standard method listing MTypes implemented by this class.
   public method get_subscribed_mtypes {} {
      return {
         client.env.get
      }
   }

   #  Implementation for client.env.get MType.
   public method client.env.get {sender_id param_list} {
      array set params $param_list
      set name $params(name)
      if {[info exists ::env($name)]} {
         set value $::env($name)
      } else {
         set value ""
      }
      set result(value) $value
      return [rpcvar struct [array get result]]
   }
}
