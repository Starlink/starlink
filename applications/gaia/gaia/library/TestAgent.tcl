#+
#  Name:
#     TestAgent

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Implementation of some test MTypes.

#  Description:
#     Does the work for responding to some baseline diagnostic SAMP MTypes.
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
#     24-JUN-2009 (MBT):
#        Original version.
#     {enter_further_changes_here}

#-

itcl::class samp::TestAgent {

   #  Standard method listing MTypes implemented by this class.
   public method get_subscribed_mtypes {} {
      return {
         test.echo
         test.fail
      }
   }

   #  Implementation for test.echo MType.
   public method test.echo {sender_id param_list} {
      return [rpcvar struct $params]
   }

   #  Implementation for test.fail MType.
   public method test.fail {sender_id param_list} {
      error "Failed."
   }
}
