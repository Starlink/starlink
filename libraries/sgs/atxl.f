      SUBROUTINE sgs_ATXL (STRING)
*+
*  Name:
*     ATXL

*  Purpose:
*     Append left justified string onto text buffer.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     STRING = CHAR (Given)
*         Character string

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
*     sgs_ATEXT

*-

      IMPLICIT NONE

      CHARACTER*(*) STRING

      INTEGER I


*  Find length of left justified portion
      DO 10 I = LEN(STRING),1,-1
         IF (STRING(I:I).NE.' ') GO TO 20
   10 CONTINUE
      I = 0
   20 CONTINUE

*  Append
      IF (I.GE.1) CALL sgs_ATEXT(STRING(:I))

      END
