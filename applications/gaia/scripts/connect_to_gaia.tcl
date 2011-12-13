#+
#   Name:
#     connect_to_gaia
#
#   Purpose:
#     Attach to an existing GAIA or start a new instance.
#
#   Usage:
#     lassign [connect_to_gaia] gaia_fd connected
#
#   Description:
#     A procedure that tries to connect to an existing GAIA by connecting
#     to the remote socket interface. If that fails a new instance of GAIA
#     will be started. The return is a file descriptor to the socket, this
#     can be used to send "remotetcl" commands, and a flag indicating if a
#     new instance of GAIA was started.
#
#     To detach from GAIA just close the given file descriptor.
#
#     To send to GAIA see the simple "send_to_gaia" proc also included,
#     and to make queries about the GAIA basic components "get_gaia_parts".
#
#   Authors:
#     PWD: Peter W. Draper (JAC, Durham University)
#
#  Copyright:
#     Copyright (C) 2009: Science and Technology Facilities Council
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
#      24-FEB-2009 (PWD):
#         Original version.
#-
#.

#  Open a socket to a GAIA application and return the file descriptor
#  for remote commands. If a GAIA isn't found then start one up.
proc connect_to_gaia {} {
   global env
   set created_instance 0

   #  Get the hostname and port info from the file .rtd-remote, which is
   #  created by rtdimage when the remote subcommand is used. Normally the
   #  .rtd-remote file lives in the $HOME directory, but this can be changed
   #  using the environment variable RTD_REMOTE_DIR. GAIA doesn't create the
   #  RTD_REMOTE_DIR directory so neither should we, which means if it doesn't
   #  exist we should give up now as no contact file will be written.
   if { [info exists env(RTD_REMOTE_DIR)] } {
      if { ! [file isdirectory $env(RTD_REMOTE_DIR)] } {
         error "Remote access to GAIA is currently disabled.\
The RTD_REMOTE_DIR directory: $env(RTD_REMOTE_DIR), does not exist."
      }
      set rtd-remote "$env(RTD_REMOTE_DIR)/.rtd-remote"
   } else {
      set rtd-remote "$env(HOME)/.rtd-remote"
   }

   set tries 0
   while { 1 } {
      set needed 0

      #  Open the file containing the GAIA process information and read it.
      if {[catch {set fd [open ${rtd-remote}]} msg]} {
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
            return [list $msg $created_instance]
         }
      }

      #  If the process doesn't exist and we've not been around the
      #  loop already, then start a new GAIA.
      if { $needed && $tries == 0 } {
         set created_instance 1
         puts stderr "Failed to connect to GAIA, starting new instance..."
         exec $env(GAIA_DIR)/gaia.sh &
      }

      #  Now either wait and try again or give up if waited too long.
      if { $needed && $tries < 500 } {
         #  Wait for a while and then try again.
         incr tries
         after 1000
      } elseif { $needed } {
         error "Sorry timed out: failed to display image in GAIA"
      }
   }
}

#  Send the command "args" to GAIA which is listening on the sockect
#  "gaia_fd" and return the results or generate an error.
proc send_to_gaia {gaia_fd args} {
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

#  Get access to the components of the GAIA connected to a given socket.
#  Returns a list of "ctrlwidget", "rtdimage" and "gaia", which can be used
#  to effect various actions (see GaiaImageCtrl, rtdimage and Gaia).
proc get_gaia_parts {gaia_fd} {
   #  Get a list of Skycat images.
   set cmd "skycat::SkyCat::get_skycat_images"
   set images [send_to_gaia $gaia_fd remotetcl $cmd]

   #  Got list so select first and ask about the parent (should be
   #  top-level GAIA) and the rtdimage widget.
   set ctrlwidget [lindex $images 0]

   set cmd "$ctrlwidget get_image"
   set rtdimage [send_to_gaia $gaia_fd remotetcl $cmd]

   set cmd "winfo parent $ctrlwidget"
   set gaia [send_to_gaia $gaia_fd remotetcl $cmd]

   return [list $ctrlwidget $rtdimage $gaia]
}
