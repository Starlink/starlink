      SUBROUTINE sgs_ATXB (STRING, NSPACE)
*+
*  Name:
*     ATXB

*  Purpose:
*     Append a field onto the text buffer leaving a specified number
*     of spaces before the non-blank region of the field.

*  Language:
*     Starlink Fortran 77

*  Arguments:
*     STRING = CHAR (Given)
*         String to be appended
*     NSPACE = INTEGER (Given)
*         Number of spaces to leave

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

      INTEGER NSPACE
      CHARACTER*(*) STRING

      INTEGER N,IS,LS


*   String length
      LS=LEN(STRING)

*   Append the required number of spaces
      IF (NSPACE.GE.1) THEN
         DO 10 N=1,NSPACE
            CALL sgs_ATEXT(' ')
   10    CONTINUE
      END IF

*   Skip leading spaces in field
      DO 20 IS = 1,LS
         IF (STRING(IS:IS).NE.' ') GO TO 30
   20 CONTINUE
   30 CONTINUE

*  Append rest of field
      CALL sgs_ATEXT(STRING(IS:))

      END
