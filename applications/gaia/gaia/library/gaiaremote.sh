#!/bin/sh    
# The next line is executed by /bin/sh, but not Tcl \
exec $GAIA_DIR/gaia_stcl $0 ${1+"$@"}
#+
#   Name:
#      gaiaremote
#
#   Purpose:
#      Sends a remote command to an rtd widget.
#
#   Usage:
#      gaiaremote command
#
#   Description:
#      This procedure sends a command, using the RTD remote
#      control mechanism, to an instance of GAIA. It does not use the
#      Tk send mechanism, so provides a secure method of controlling
#      GAIA without any X security problems (but the commands set if
#      more restrictive).
#
#      If an instance of GAIA cannot be located a new one is started.
#
#   Authors:
#      Peter W. Draper (PDRAPER):
#
#   History:
#      09-MAR-1998 (PDRAPER):
#         Original version.
#      18-MAY-1999 (PDRAPER):
#         Converted to use standalone binary gaia_stcl.
#-
#.

#  Add GAIA_DIR to autopath for some GAIA classes.
lappend auto_path $env(GAIA_DIR)

#  Open a socket to a GAIA application and return the file descriptor
#  for remote commands. If a GAIA isn't found then start one up.
proc connect_to_gaia {} {
   global env

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

if { $argc == 0 } { 
   puts "Usage: gaiaremote gaia_command"
   exit
}

#  Open up connection to GAIA.
set gaia_fd [connect_to_gaia]

#  Send the command and output any result.
set result [eval send_to_gaia $argv]
if { $result != {} } { 
    puts stderr "gaiaremote: $result"
}
close $gaia_fd
exit
