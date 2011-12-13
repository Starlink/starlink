      SUBROUTINE sgs_APOLY (X,Y)
*+
*  Name:
*     APOLY

*  Purpose:
*     Append new point to current polyline.

*  Language:
*     Starlink Fortran 77

*  Description:
*     If the polyline buffer is full, call OPOLY to flush the polyline to
*     GKS and start a new polyline beginning at the last point of the old
*     one.  Copy the new point into the buffer and increment the number of
*     points.

*  Arguments:
*     X = REAL (Given)
*         X coordinate of new point
*     Y = REAL (Given)
*         Y     "      "   "    "

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
*     07-SEP-1991 (PTW/DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  Externals:
*     sgs_OPOLY

*  Read From Common:
*     NPOLY  i     Length of current polyline

*  Written To Common:
*     XPOLY  r()   Polyline buffer (X)
*     XPOLY  r()   Polyline buffer (Y)
*     NPOLY  i     Length of current polyline
*
*  Constants from SGSCOM:
*     LPOLY  i     Maximum number of points in polyline

*-

      IMPLICIT NONE

      REAL X,Y

      INCLUDE 'sgscom'



*  Accept append request only if polyline has been begun
      IF (NPOLY.GT.0) THEN

*     If buffer full, flush
         IF (NPOLY.GE.LPOLY) CALL sgs_OPOLY

*     Append new line
         NPOLY=NPOLY+1
         XPOLY(NPOLY)=X
         YPOLY(NPOLY)=Y
      END IF

      END
