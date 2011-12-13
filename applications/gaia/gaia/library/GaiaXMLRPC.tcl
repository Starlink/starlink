#+
#  Name:
#     GaiaXMLRPC

#  Type of Module:
#     [incr Tcl] class

#  Purpose:
#     Controls and defines the XMLRPC services offered by GAIA.

#  Description:
#     This class starts an embedded webserver (using tclhttpd) and
#     then adds various XMLRPC services (using tclsoap). These services
#     can be remoted contacted using the URL:
#
#         http://localhost:$port/gaia
#
#     The names of each service are defined in the proc declarations
#     below.

#  Invocations:
#
#        GaiaXMLRPC object_name [configuration options]
#
#     This creates an instance of a GaiaXMLRPC object. The return is
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

#  Configuration options:

#  Methods:

#  Inheritance:
#     This object inherits no other classes.

#  Copyright:
#     Copyright (C) 2000-2005 Central Laboratory of the Research Councils.
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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     PWD: Peter Draper (STARLINK - Durham University)
#     {enter_new_authors_here}

#  History:
#     07-JUN-2006 (PWD):
#        Original version.
#     {enter_further_changes_here}

#-

#.

#  Import all the packages we need to support tclsoap.
package require XMLRPC::Domain
package require rpcvar
package require httpd
package require httpd::url
package require httpd::threadmgr
package require httpd::utils
package require httpd::version
package require httpd::counter
package require httpd::doc
package require httpd::log
package require httpd::auth
package require httpd::mtype

namespace import -force rpcvar::*

itcl::class gaia::GaiaXMLRPC {

   #  Inheritances:
   #  -------------

   #  Nothing

   #  Constructor:
   #  ------------
   constructor {args} {
      eval configure $args

      #  Start the webservice and load the domain handler for XMLRPC.
      start

      #  Register all locally available procs.
      register
   }

   #  Destructor:
   #  -----------
   destructor  {
      stop
   }

   #  Methods:
   #  --------

   #  Start the embedded web server on the current port and load the XMLRPC
   #  URL domain handler into it. Register under the /gaia URL.
   public method start {} {
      start_server_
      if {[catch {XMLRPC::Domain::register -prefix /gaia \
                     -namespace gaia::GaiaXMLRPC} msg]} {
         if { $msg != "URL prefix \"/gaia\" already registered"} {
            error "Warning: $msg"
         }
      }
   }

   #  Stop the embedded web server.
   public method stop {} {
      stop_server_
   }

   #  Start the web server.
   protected method start_server_ {} {

      Httpd_Init

      #  Open the listening sockets
      Httpd_Server $port localhost 127.0.0.1

      #  This seems to be necessary... No doubt there are many more we should
      #  be setting.
      Counter_Init 60

      #  Make root directory etc. show the GAIA hypertext help.
      Mtype_ReadTypes nofile
      global gaia_help
      Doc_Root $gaia_help
      DirList_IndexFile index.html
   }

   #  Stop the web server.
   protected method stop_server_ {} {
      Httpd_Shutdown
   }

   #  Locate an instance of Gaia for displaying images.
   proc get_gaia_ {} {
      foreach image [::skycat::SkyCat::get_skycat_images] {
         return [winfo parent $image]
      }
      return ""
   }

   #  ===============================================================
   #  Entry points for services.
   #  ===============================================================

   #  Register the local procs that can accessed.
   protected method register {} {
      XMLRPC::export hello display
   }

   #  Simple hello world echo service.
   proc hello {value} {
      puts "GaiaXMLRPC: someone says hello: $value"
      return "hello to you (from gaiaHello)"
   }

   #  Display an image in GAIA.
   proc display {image} {
      set basegaia [get_gaia_]
      if { $basegaia != {} } {
         return [$basegaia open $image]
      }
      return 0
   }

   #  Configuration options: (public variables)
   #  ----------------------

   #  Port for the web server.
   public variable port 8015

   #  Protected variables: (available to instance)
   #  --------------------


   #  Common variables: (shared by all instances)
   #  -----------------

#  End of class definition.
}
