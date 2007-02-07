#!/bin/sh
# The next line is executed by /bin/sh, but not Tcl \
exec $GAIA_DIR/gaia_stcl $0 ${1+"$@"}
#+
#   Name:
#      gaiacmap

#   Purpose:
#      Set colormap for use in GAIA.

#   Usage:
#         gaiacmap cmap_file

#   Description:
#      

#   Notes:

#   Authors:
#      Peter W. Draper (PWD):

#  Copyright:
#     Copyright (C) 2007 Particle Physics & Astronomy Research Council.
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


#   History:
#      25-NOV-2007 (PWD):
#         Original version.
#-
#.

#  Check the command-line arguments.
if { $argc == 1 } {
   set cmap [lindex $argv 0]
} else {
   puts stderr {Usage: gaiacmap filename}
   exit
}

#  Add GAIA_DIR to autopath for some GAIA classes.
lappend auto_path $env(GAIA_DIR)

#  Useful to know when instance is created.
set created_instance 0

#  Open a socket to a GAIA application and return the file descriptor
#  for remote commands. If a GAIA isn't found then start one up.
proc connect_to_gaia {} {
   global env
   global created_instance

   #  Get the hostname and port info from the file ~/.rtd-remote,
   #  which is created by rtdimage when the remote subcommand is
   #  used.
   set tries 0
   while { 1 } {
      set needed 0

      #  Open the file containing the GAIA process information and read it.
      if {[catch {set fd [open $env(HOME)/.rtd-remote]} msg]} {
         set needed 1
      } else {
         lassign [read $fd] pid host port
         close $fd
      }

      #  See if the process is listening to this socket.
      if { ! $needed } {
         if {[catch {socket $host $port} msg]} {
            set needed 1
         } else {
            fconfigure $msg -buffering line
            return $msg
         }
      }

      #  If the process doesn't exist and we've not been around the
      #  loop already, then start a new GAIA.
      if { $needed && $tries == 0 } {
         puts stderr "Failed to connect to GAIA, starting new instance..."
         set created_instance 1
         exec $env(GAIA_DIR)/gaia.sh &
         #exec $env(GAIA_DIR)/tgaia &
      }

      #  Now either wait and try again or give up if waited too long.
      if { $needed && $tries < 500 } {
         #  Wait for a while and then try again.
         incr tries
         after 1000
      } elseif { $needed } {
         puts stderr "Sorry timed out: failed to display image in GAIA"
         exit 1
      }
   }
}

#  Send the command to GAIA and return the results or generate an error.
proc send_to_gaia {args} {
   global gaia_fd
   puts $gaia_fd "$args"
   lassign [gets $gaia_fd] status length
   set result {}
   if {$length > 0} {
      set result [read $gaia_fd $length]
   }
   if {$status != 0} {
      error "$result"
   }
   return "$result"
}

#  Open up connection to GAIA.
set gaia_fd [connect_to_gaia]

#  Command needs to performed by Skycat or derived object. We just
#  talk to the first window on the list.
set cmd "skycat::SkyCat::get_skycat_images"
set images [send_to_gaia remotetcl $cmd]

#  Got list so select first and ask about the parent (should be
#  top-level GAIA).
set ctrlwidget [lindex $images 0]
set cmd "winfo parent $ctrlwidget"
set gaia [send_to_gaia remotetcl $cmd]

set cmd "$gaia get_image"
set imagectrl [send_to_gaia remotetcl $cmd]
set cmd "$imagectrl get_image"
set rtdimage [send_to_gaia remotetcl $cmd]

set cmd "$rtdimage cmap file $cmap"
set result [send_to_gaia remotetcl $cmd]
puts "result = $result"

close $gaia_fd
exit
