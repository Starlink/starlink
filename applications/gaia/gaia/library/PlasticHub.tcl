#+
#  Name:
#     PlasticHub

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Provides methods for communicating with a running PLASTIC hub.

#  Description:
#     This class serves as a proxy for a remote PLASTIC hub process.
#     On construction it attempts to locate a plastic hub's XML-RPC
#     server by searching for and reading the ~/.plastic file.
#     When constructed, hub methods can be executed using the
#     execute method.For instance, to determine the ID of the
#     running hub, do:
#
#        plastic::PlasticHub hub
#        set hub_id [hub execute getHubId]
#
#     See http://plastic.sourceforge.net/ for discussion of the PLASTIC
#     protocol.

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
#     13-JUL-2006 (MBT)
#        Original version.
#     {enter_further_changes_here}

#-

#  Import all the packages we need for XMLRPC.
package require XMLRPC

itcl::class plastic::PlasticHub {

   #  Constructor:
   #  ------------
   constructor {args} {
      eval configure $args

      #  Read the PLASTIC hub URL from the .plastic properties file.
      read_props_ [rendezvous_file] plastic_properties
      set hub_url_ $plastic_properties(plastic.xmlrpc.url)

      #  Create functions in this namespace matching the services that
      #  the hub should provide.
      set hubcmds {
         { getHubId {} }
         { getMessageRegisteredIds {message string} }
         { getName {plid string} }
         { getRegisteredIds {} }
         { getUnderstoodMessages {plid string} }
         { registerNoCallBack {name string} }
         { registerXMLRPC {name string supported_messages string()
                           callback_url string} }
         { request {sender string message string args array} }
         { requestAsynch {sender string message string args array} }
         { requestToSubset {sender string message string args array
                            recipient_ids string()} }
         { requestToSubsetAsynch {sender string message string args array
                                  recipient_ids string()} }
         { unregister {id string} }
      }
      foreach hubcmd $hubcmds {
         set name [lindex $hubcmd 0]
         set params [lindex $hubcmd 1]
         XMLRPC::create plastic.hub.$name \
                        -proxy $hub_url_ \
                        -params $params
      }
   }

   #  Methods:
   #  --------

   #  Executes one of the hub methods using XML-RPC on the remote hub.
   #  See the PLASTIC specification for the descriptions of these methods.
   public method execute {hubMethod args} {
      return [eval plastic.hub.$hubMethod $args]
   }

   #  Common Procedures:
   #  ------------------

   #  Returns 1 if a PLASTIC hub is apparently running, 0 if not.
   public proc is_hub_running {} {
      return [file exists [rendezvous_file]]
   }

   #  Returns the location of the .plastic file which contains information
   #  concerning a currently-running hub.
   public proc rendezvous_file {} {

      # If in CYGWIN environment look for contact file in the Windows home
      # directory.  Most plastic applications (like SPLAT) will be native
      # windows ones.
      if { [string match {CYGWIN*} $::tcl_platform(os)] } {
         return "[exec cygpath -D]/../.plastic"
      }
      return $::env(HOME)/.plastic
   }

   #  Reads a java properties file into a given associative array.
   protected proc read_props_ {filename props} {
      upvar $props _props
      set fid [open $filename r]
      while {[gets $fid line] >= 0} {
         if {[regexp {^([A-Za-z0-9\.]+) *= *(.*)$} [string trim $line] \
                                                   all key value]} {
            set _props([unescape_ $key]) [unescape_ $value]
         }
      }
      close $fid
   }

   #  Unescapes a line from a java properties file.  The format is
   #  documented at
   #  http://java.sun.com/j2se/1.5.0/docs/api/java/util/Properties.html#load(java.io.InputStream).
   protected proc unescape_ {in} {

      #  The 'algorithm' used here is not foolproof; however the main
      #  form of funny business likely to be encountered in the .plastic
      #  file is backslash quoting, which fortuitously can be handled
      #  pretty well by shoving it through an eval.  However \uNNNN-type
      #  unicode characters amongst other things would fox this routine.
      eval "return $in"
   }

   #  Private variables: (available to instance)
   #  ------------------
   private variable hub_url_
}
