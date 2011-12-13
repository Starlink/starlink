#!/bin/sh
# The next line is executed by /bin/sh, but not Tcl \
exec $GAIA_DIR/gaia_stcl $0 ${1+"$@"}
#+
#   Name:
#      xmlrpcdisp

#   Purpose:
#      Display images in a GAIA tool.

#   Usage:
#         xmlrpcdisp image

#   Description:
#      This command displays images in GAIA. It has two basic modes
#      display a single image into a specified "clone", or display
#      a list of images into a series of automatically created clones.
#
#      Clones are specified by an integer number less than 1000.
#      The special number -1 indicates that a number should be generated.

#   Notes:
#      This is an XML-RPC based version of the gaiadisp script.

#   Authors:
#      Peter W. Draper (PWD):

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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA


#   History:
#      08-JUN-2006 (PWD):
#         Original version.
#-
#.

#  Make sure we have access to all the necessary packages.
package require XMLRPC
package require SOAP::http

#  Check for the command-line argument, the name of the image.
if { $argc != 1 } {
   puts stderr {Usage: xmlrpcdisp filename}
   exit
}

#  The URL of GAIA. This needs to be picked up the environment
#  (say ~/.skycat/xmlrpc-remote)
set URL http://localhost:8015/gaia

#  Define the command for sending display request to GAIA.
XMLRPC::create display -proxy $URL -params {filename string}

#  Send the message.
display $argv

exit
