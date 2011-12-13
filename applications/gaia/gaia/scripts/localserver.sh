#!/bin/sh
#!/bin/sh
# The next line is executed by /bin/sh, but not Tcl \
exec $GAIA_DIR/gaia_stcl $0 ${1+"$@"}
#+
#   Name:
#      localserver
#
#   Purpose:
#      Serve a local query catalogue to GAIA.
#
#   Usage:
#      localserver [port]
#
#   Description:
#      This script is a template for writing local servers that
#      can deal with a catalogue query from GAIA. You may find
#      this method is useful when faced with a large catalogue that is
#      to big to just display in GAIA as a local catalogue and the
#      option to serve queries through a web server is not
#      available to you.
#
#      To use this, edit it as appropriate, then run the script in the
#      background with a suitable user socket. This will edit your
#      your local ~/.skycat/skycat.cfg file to reference it:
#
#         serv_type:      catalog
#         long_name:      My catalogue of things
#         short_name:     mycat@localhost
#         url:            http://hostname:${port}/&ra=%ra&dec=%dec&radius=%r2&nout=%n
#
#      Obviously ":8888" should be set to your port number.
#
#      Now start up GAIA and look the the new entry in the catalogues
#      list (Browse Catalog Directories...).
#
#   Authors:
#      Peter W. Draper (PWD):
#
#  Copyright:
#     Copyright (C) 2001-2005 Central Laboratory of the Research Councils.
#     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
#     All Rights Reserved.
#
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
#
#   History:
#      09-APR-2001 (PWD):
#         Original version.
#-
#.

#=====================================================================
#  Procedure to edit the local ~/.skycat/skycat.cfg file. Only editted
#  if existing entry is not found, so you need to remove this file
#  when testing.
proc edit_config_file { port } {
   global env
   set located 0
   if { [file exists $env(HOME)/.skycat/skycat.cfg] } {
      set iostream [open $env(HOME)/.skycat/skycat.cfg r]
      while { [gets $iostream line] > -1 } {
         if { [string match {*mycat@localhost*} $line] } {
            set located 1
         }
      }
      close $iostream
   } else {
      #  Copy the default file into place.
      if { ! [file exists $env(HOME)/.skycat] } {
         file mkdir $env(HOME)/.skycat
      }
      file copy -force "$env(GAIA_DIR)/skycat2.0.cfg" "$env(HOME)/.skycat/skycat.cfg"
   }
   if { ! $located } {
      set iostream [open $env(HOME)/.skycat/skycat.cfg a]
      puts $iostream ""
      puts $iostream "serv_type:      catalog"
      puts $iostream "long_name:      My catalogue of things"
      puts $iostream "short_name:     mycat@localhost"
      puts $iostream "url:            http://[id host]:${port}/query&ra=%ra&dec=%dec&radius=%r2&nout=%n"
      puts "Local configuration file updated"
      close $iostream
   } else {
      puts "Local configuration file not updated"
   }
}


#=====================================================================
#  Procedure to handle incoming requests, dispatching to the actual
#  catalogue reading proceedure.
proc server_handler {sock} {

   #  Get the client message.
   set message [gets $sock]

   #  If client detaches then close the socket.
   if {[eof $sock]} {
      close $sock

   } else {
      #  Process the message and return catalogue query.
      process_request $sock $message
   }
}

#=====================================================================
#  Procedure defining the accept connection handler for the server.
#  Called when client makes a connection.
proc accept {sock addr port} {

   # Setup handler for future communication on client socket
   fileevent $sock readable [list server_handler $sock]

   # Note we've accepted a connection.
   puts "Accept from [fconfigure $sock -peername]"

   # Read client input in lines, disable blocking I/O
   fconfigure $sock -buffering line -blocking 0

   # Send Acceptance string to client
   puts $sock "$addr:$port, You are connected to the echo server."
   puts $sock "It is now [exec date]"

   # log the connection
   puts "Accepted connection from $addr at [exec date]"
}

#=====================================================================
#  Procedure to create a server socket.
proc start_server { port } {
   socket -server accept $port

   #  Process now waits until events is set. This makes it wait for
   #  client connections, until the process is killed.
   global events
   vwait events
}

#=====================================================================
#  Procedure to interface with the local catalogue and return the
#  given query as a streamed TAB table down the port.
proc process_request {sock message} {

   #  Parse the query message. This should be a GET followed by a
   #  User-Agent and finally a blank line. So ignore all except GET.
   if { [regexp {GET\ ([^\ ]*).*}  $message match query] } {
      puts "query = $query"
      regsub -all {&} $query { } newquery
      regsub -all {=} $newquery { } query
      set ra [lindex $query 2]
      set dec [lindex $query 4]
      set rad [lindex $query 6]
      set nout [lindex $query 8]
      process_catalogue $sock $ra $dec $rad
   } else {

      #  Blank message is last?
      if { $message == {} } {
         close $sock
      }
   }

}

#=====================================================================
#  Procedure to read and parse the local catalogue. The query elements
#  are a position on the sky and a radius around it. This example uses
#  a local file "2mass.tbl" (a 2MASS catalogue), but schemes based on
#  CURSA and catview etc. could be easily done.
proc process_catalogue {sock ra dec rad} {
   puts "process_catalogue"
   if { [file exists "2mass.tbl"] } {

      #  write the document output header.
      puts $sock "Content-type: text/plain"
      puts $sock ""
      puts $sock ""
      puts $sock "Local catalogue server results"


      #  Write the catalogue header.
      puts $sock "\#"
      puts $sock "x_col: -1"
      puts $sock "y_col: -1"
      puts $sock "ra_col: 0"
      puts $sock "dec_col: 1"
      #puts $sock "id_col: 29"
      puts $sock "symbol {} circle 4"
      #puts $sock "ra	dec	emaj	emin	eang	designation	j_m	jmsig	jscom	h_m	hmsig	hscom	k_m	kmsig	kscom	rdf	blfl	ccfl	extd	m	idop	bmopt	rmopt	diopt	phio	nopt	date	hemi	scan	id	glon	glat	jpsfch	hpsfch	kpsfch	j_mstd	jsstd	h_mstd	hsstd	k_mstd	ksstd	ndetfl	j_h	h_k	j_k"
      puts $sock "ra	dec"
      puts $sock "--"
      set io [open "2mass.tbl" r]
      set i 0
      while {[gets $io line] > -1 } {
         lappend $line thisra thisdec
         set thisra [lindex $line 0]
         set thisdec [lindex $line 1]
         puts $sock "$thisra\t $thisdec"
         incr i
         #if { $i > 1000 } {
         #   break;
         #}
      }
      puts $sock {[EOD]}
      close $io
   }
}


#=====================================================================
#  Activation of interface code.
#  Default port number.
set port 8921

#  Look for local ~/.skycat/skycat.cfg file and add our entry.
edit_config_file $port

#  Start listening on socket for queries and process them.
start_server $port
