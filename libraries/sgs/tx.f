      SUBROUTINE sgs_TX (X,Y, STRING)
*+
*  Name:
*     TX

*  Purpose:
*     Begin a new text string with a string.

*  Language:
*     Starlink Fortran 77

*  Description:
*     (In practice, this means simply "plot a string")

*  Arguments:
*     X = REAL (Given)
*         Position of string (x)
*     Y = REAL (Given)
*         "     "    "   (y)
*     STRING = CHAR (Given)
*         String which is to begin the new text string

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
*     sgs_BTEXT, sgs_ATEXT

*-

      IMPLICIT NONE

      REAL X,Y
      CHARACTER*(*) STRING



*  Begin a new text string
      CALL sgs_BTEXT(X,Y)

*  Append the string to it
      CALL sgs_ATEXT(STRING)

      END
