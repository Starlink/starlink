#! /bin/sh
# The next line is executed by /bin/sh, but not Tcl \
TCL_LIBRARY=$GAIA_DIR; \
TCLX_LIBRARY=$GAIA_DIR; \
TK_LIBRARY=$GAIA_DIR; \
TKX_LIBRARY=$GAIA_DIR; \
ITCL_LIBRARY=$GAIA_DIR; \
ITK_LIBRARY=$GAIA_DIR; \
IWIDGETS_LIBRARY=$GAIA_DIR; \
RTD_LIBRARY=$GAIA_DIR; \
TCLADAM_DIR=$GAIA_DIR; \
export TCL_LIBRARY; \
export TCLX_LIBRARY; \
export TK_LIBRARY; \
export TKX_LIBRARY; \
export ITCL_LIBRARY; \
export ITK_LIBRARY; \
export IWIDGETS_LIBRARY; \
export RTD_LIBRARY; \
export TCLADAM_DIR; \
exec $GAIA_DIR/gaia_wish $0 ${1+"$@"}
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
#-
#.

#  Do not show the main window.
wm withdraw .

#  Open a socket to a GAIA application and return the file descriptor
#  for remote commands. If a GAIA isn't found then start one up.
proc connect_to_gaia {} {
   global env

   #  Get the hostname and port info from the file ~/.rtd-remote, 
   #  which is created by rtdimage when the remote subcommand is
   #  used. 
   set tries 0
   while { $tries < 1000 } {
      set needed 0

      #  Open the file containing the process id and read it.
      if {[catch {set fd [open $env(HOME)/.rtd-remote]} msg]} {
         set needed 1
      }
      lassign [read $fd] pid host port
      close $fd
      
      #  See if the process exists.
      if { ! $needed } { 
         if {[catch {exec kill -0 $pid} msg]} {
            set needed 1
         }
      }

      #  If the process doesn't exist and we've not been around the
      #  loop already, then start a new GAIA.
      if { $needed && $tries == 0 } { 
         puts "Starting a new GAIA instance. Please wait..."
         exec $env(GAIA_DIR)/gaia.sh &
         #      exec $env(GAIA_DIR)/tgaia &
      }

      #  Now either wait and try again or give up if waited too long.
      if { $needed && $tries < 1000 } { 
         #  Wait for a while and then try again.
         incr tries
         after 1000 
      } elseif { $needed } { 
         puts stderr "Failed to create a new GAIA"
         exit 1
      } else {

         #  Worked. Break out of loop and proceed.
         break;
      }
   }

   #  Make the connection.
   set fd [server_connect -nobuf $host $port]
   return $fd
}

#  Send the command to GAIA and return the results or generate an error.

proc send_to_gaia {args} {
    global gaia_fd
    puts $gaia_fd $args
    lassign [gets $gaia_fd] status length
    set result {}
    if {$length > 0} {
	set result [read $gaia_fd $length]
    }
    if {$status != 0} {
	error $result
    }
    return $result
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
exit
