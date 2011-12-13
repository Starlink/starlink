      SUBROUTINE sgs_LINE (X1,Y1, X2,Y2)
*+
*  Name:
*     LINE

*  Purpose:
*     Begin a new polyline with a single line.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     X1 = REAL (Given)
*         Starting x coordinate for polyline
*     Y2 = REAL (Given)
*         "     y     "       "     "
*     X2 = REAL (Given)
*         Ending   x     "       "     "
*     Y2 = REAL (Given)
*         "     y     "       "     "

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

*  Externals:
*     sgs_BPOLY, sgs_APOLY

*-

      IMPLICIT NONE

      REAL X1,Y1,X2,Y2



*  Begin the new polyline
      CALL sgs_BPOLY(X1,Y1)

*  Append the line
      CALL sgs_APOLY(X2,Y2)

      END
