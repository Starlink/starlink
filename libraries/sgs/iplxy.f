      SUBROUTINE sgs_IPLXY (X,Y)
*+
*  Name:
*     IPLXY

*  Purpose:
*     Inquire end of current polyline.

*  Language:
*     Starlink Fortran 77

*  Description:
*     X,Y are unchanged if there is no current polyline.

*  Arguments:
*     X = REAL (Returned)
*         X coordinate of end of current polyline
*     Y = REAL (Returned)
*         Y     "      "   "  "     "       "

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PTW: P. T. Wallace (Starlink)
*     DLT: D. L. Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     14-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Read From Common:
*     NPOLY    i      length of current polyline
*     XPOLY    r()    current polyline (X)
*     YPOLY    r()       "       "     (Y)

*-

      IMPLICIT NONE

      REAL X,Y

      INCLUDE 'sgscom'




      IF (NPOLY.GT.0) THEN
         X = XPOLY(NPOLY)
         Y = YPOLY(NPOLY)
      END IF

      END
