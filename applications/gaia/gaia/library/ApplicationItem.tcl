#+

#  Name:
#     ApplicationItem

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Represents an external application registered with the PLASTIC hub.

#  Description:
#     This is a simple struct-type class which just holds information that
#     can be kept track of locally about applications currently 
#     registered with the PLASTIC hub.

#  Configuration options:
#
#     -id
#        PLASTIC identifier for the application (identifies it uniquely
#        to the hub).
#     -name
#        Human-readable name for the application.
#     -supported_messages
#        List of message IDs which this application claims to support.

#  Copyright:
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
#     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
#     02111-1307, USA

#  Authors:
#     MBT: Mark Taylor
#     {enter_new_authors_here}

#  History:
#     21-JUL-2006 (MBT)
#        Original version (server parts cribbed from GaiaXMLRPC).
#     {enter_further_changes_here}

#-

itcl::class plastic::ApplicationItem {

   constructor {args} {
      eval configure $args
   }

   #  Application ID.
   public variable id {}

   #  Human-readable application name.
   public variable name {}

   #  List of supported messages
   public variable supported_messages {}
}
