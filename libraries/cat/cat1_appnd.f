      SUBROUTINE CAT1_APPND (ADDSTR, STRING, POSN)
*+
*  Name:
*     CAT1_APPND
*  Purpose:
*     Append an additional string to a given position in a string.
*  Language:
*     Starlink Fortran 77
*  Invocation:
*     CALL CAT1_APPND (ADDSTR; STRING, POSN)
*  Description:
*     Append an additional string to a given position in a string.
*     The value is appended to the string beginning at position POSN+1.
*     POSN is updated to indicate the last element of STRING after the
*     insertion.  If no copying is done, POSN is returned unchanged. The
*     size of both STRING and the additional string are based on the
*     declared Fortran 77 size given by the intrinsic function LEN.

*     Maintenance note: in the future it may be possible to replace
*     this routine with a corresponding CHR routine.
*  Arguments:
*     ADDSTR  =  CHARACTER*(*) (Given)
*        The additional string to be appended to the given string.
*     STRING  =  CHARACTER*(*) (Given and Returned)
*        The string into which the given value is to be appended.
*     IPOSN  =  INTEGER (Given and Returned)
*        The position pointer within STRING.
*  Method:
*     Get the size of the returned string.
*     If the current position is within the range of the returned string
*     then
*       Determine the number of characters which can be copied.
*       If any characters are to be copied then
*         Copy the characters.
*         Update the position in the string
*       end if
*     end if
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     ACD: A.C. Davenhall (Leicester)
*  History:
*     13-SEP-1994 (ACD):
*        Original version.  But note that this routine is in practice
*        a re-write of the original CHR_PUTC.
*  Bugs:
*     None known.
*-
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing.
*  Arguments Given:
      CHARACTER
     :  ADDSTR*(*)
*  Arguments Given and Returned:
      CHARACTER
     :  STRING*(*)
      INTEGER
     :  POSN
*  Local Variables:
      INTEGER
     :  STRNGS,  ! Declared size of STRING (inc. trail. blanks).
     :  ADDSTS,  !    "      "   "  ADDSTR ( " .   "  .   "   ).
     :  REMAIN,  ! Number of free characters remaining in STRING.
     :  NCHAR    ! Number of characters to be copied.
*.

*
*    Get the size of the returned string.

      STRNGS = LEN(STRING)

*
*    Check that the current position is within the range of the returned
*    string and that there is space for at least one more character to
*    be added.

      IF (POSN .GE. 0  .AND.  POSN .LT. STRNGS) THEN

*
*       Determine the number of characters to be added.  If possible
*       all the characters in the additional string (including any
*       trailing blanks) are added.  However, if there is insufficient
*       space in the returned string for all the characters then the
*       maximum number which can be accommodated are copied.

         ADDSTS = LEN(ADDSTR)
         REMAIN = STRNGS - POSN

         NCHAR = MIN(ADDSTS, REMAIN)

*
*       If more than zero characters are to be copied then copy them
*       and update the position in the string.  The check for a positive
*       number of characters is necessary to exclude the case where, for
*       example, a zero length string is being appended.  If the check
*       is not made the routine would crash with a Fortran error.

         IF (NCHAR .GT. 0) THEN
            STRING(POSN+1 : POSN+NCHAR) = ADDSTR(1 : NCHAR)
            POSN = POSN + NCHAR
         END IF

      END IF

      END
