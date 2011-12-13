#!/bin/csh
#+
#  Name:
#     LUTREAD

#  Purpose:
#     Loads an image-display lookup table from an NDF.

#  Language:
#     C-shell

#  Type of Module:
#     C-shell script.

#  Description:
#     This application reads a lookup table stored in an NDF with
#     the standard format, and loads it into the current image-display
#     device.

#  Arguments:
#     LUT = LITERAL (Given)
#        The file containing the lookup table.  It is passed to the
#        parameter LUT but not validated.

#  Usage:
#     lutread lut

#  ADAM Parameters:
#     DEVICE = DEVICE (Read)
#        Name of the image display whose colour table is to be changed.
#        [Current image-display device]
#     LUT = NDF (Read)
#        Name of the NDF containing the lookup table as its data
#        array.  The LUT must be 2-dimensional, the first dimension
#        being 3, and the second being arbitrary.  Linear interpolation
#        is used to compress or expand the colour table if the second
#        dimension is different from the number of unreserved colour
#        indices.  Also the LUT's values must lie in the range 0.0--1.0.

#  Notes:
#     This is a procedure that calls LUTABLE.  Therefore, the
#     parameters cannot be specified on the command line.  You will
#     only be prompted for the parameters if the device is not suitable
#     or not available, and/or the lookup table file could not be
#     accessed.

#  Prior Requirements:
#     - The KAPPA definitions must be assigned.

#  Copyright:
#     Copyright (C) 1992 Science & Engineering Research Council. All
#     Rights Reserved.

#  Licence:
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2 of
#     the License, or (at your option) any later version.
#
#     This program is distributed in the hope that it will be
#     useful, but WITHOUT ANY WARRANTY; without even the implied
#     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#     PURPOSE. See the GNU General Public License for more details.
#
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
#     02110-1301, USA

#  Authors:
#     MJC: Malcolm J. Currie (STARLINK)
#     {enter_new_authors_here}

#  History:
#     1992 July 28 (MJC):
#         Original version.
#     1992 November 26 (MJC):
#        Made the script activate the .cshrc file.  Uses a predefined
#        variable, lutread_pars, to pass the parameters to the this
#        script.
#     {enter_further_changes_here}

#  Bugs:
#     {note_any_bugs_here}

#-
#
#   Check whether the LUT has been given.  If not inquire what it is
#   mimicking the LUTABLE prompt for parameter LUT.
#
if ($lutread_pars == "") then
   echo -n "LUT - NDF containing input LUT > "
   set lutname = $<
else
   set lutname = $lutread_pars
endif
#
#   Change the colour table.
#
lutable mapping=linear coltab=external lut=$lutname
#
#   Exit the procedure.
#
exit
