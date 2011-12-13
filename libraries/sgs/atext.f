      SUBROUTINE sgs_ATEXT (STRING)
*+
*  Name:
*     ATEXT

*  Purpose:
*     Append a field onto the text buffer.

*  Language:
*     Starlink Fortran 77

*  Description:
*     If the resulting string is longer than the text buffer, characters
*     will be lost from the end.

*  Arguments:
*     STRING = CHAR (Given)
*         String to be appended

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

*  Read From Common:
*     NTEXT    i       Length of current string
*     CTEXT    c()     Text string buffer
*
*  Constants from SGSCOM:
*     LTEXT    i       Size of text string buffer

*-

      IMPLICIT NONE

      CHARACTER*(*) STRING

      INCLUDE 'sgscom'


      INTEGER LS,NCHARS


*  Accept append request only if text string has been begun
      IF (NTEXT.GE.0) THEN

*     Append up to end of buffer
         LS=LEN(STRING)
         NCHARS=MIN(LS,LTEXT-NTEXT)
         IF (NCHARS.GT.0) CTEXT(NTEXT+1:NTEXT+NCHARS)=STRING(:NCHARS)

*     Update count as if whole string had been appended
         NTEXT=NTEXT+LS
      END IF

      END
