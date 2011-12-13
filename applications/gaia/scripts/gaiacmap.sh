#!/bin/sh
# The next line is executed by /bin/sh, but not Tcl \
exec $GAIA_DIR/gaia_tcl $0 ${1+"$@"}
#+
#   Name:
#      gaiacmap

#   Purpose:
#      Set colormap for use in GAIA.

#   Usage:
#         gaiacmap cmap_file

#   Description:
#      Allows the colour map used by GAIA to be changed to use
#      an external file. The external file must be a simple text
#      file with three space separated columns. The columns give
#      the relative intensities of the red, green and blue channels.

#   Notes:
#      To convert a KAPPA LUT for use in GAIA you can do the following:
#
#      % convert
#      % ndf2ascii fixed=true noperec=3 in=$KAPPA_DIR/zebra_lut out=zebra_lut.lasc
#
#      The file "zebra_lut.lasc" can now be loaded by this script. Note that
#      an "lasc" file must have 256 rows, so you will need to scale or pad
#      luts with less or more rows.

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
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA


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

#  Get the standard contact scripts.
source $env(GAIA_DIR)/connect_to_gaia.tcl

#  Open up connection to GAIA.
lassign [connect_to_gaia] gaia_fd created_instance

#  Find useful objects.
lassign [get_gaia_parts $gaia_fd] imagectrl rtdimage gaia

#  Send the command.
set cmd "$rtdimage cmap file $cmap"
set result [send_to_gaia $gaia_fd remotetcl $cmd]
puts "result = $result"

close $gaia_fd
exit
