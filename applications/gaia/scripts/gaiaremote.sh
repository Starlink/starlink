#!/bin/sh
# The next line is executed by /bin/sh, but not Tcl \
exec $GAIA_DIR/gaia_tcl $0 ${1+"$@"}
#+
#   Name:
#      gaiaremote

#   Purpose:
#      Sends a remote command to an rtd widget.

#   Usage:
#      gaiaremote command

#   Description:
#      This procedure sends a command, using the RTD remote
#      control mechanism, to an instance of GAIA. It does not use the
#      Tk send mechanism, so provides a secure method of controlling
#      GAIA without any X security problems (but the commands set if
#      more restrictive).
#
#      If an instance of GAIA cannot be located a new one is started.

#   Authors:
#      Peter W. Draper (PWD):

#  Copyright:
#     Copyright (C) 1998-2005 Central Laboratory of the Research Councils.
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

#   History:
#      09-MAR-1998 (PWD):
#         Original version.
#      18-MAY-1999 (PWD):
#         Converted to use standalone binary gaia_stcl.
#-
#.

if { $argc == 0 } {
   puts "Usage: gaiaremote gaia_command"
   exit
}

#  Get the standard contact scripts.
source $env(GAIA_DIR)/connect_to_gaia.tcl

#  Useful to know when instance is created.
set created_instance 0

#  Open up connection to GAIA.
lassign [connect_to_gaia] gaia_fd created_instance

#  Send the command and output any result.
set result [eval send_to_gaia $gaia_fd $argv]
if { $result != {} } {
    puts stderr "gaiaremote: $result"
}
close $gaia_fd
exit
