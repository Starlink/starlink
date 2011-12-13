      SUBROUTINE CAT5_GLFMT (EXFMT, LXFMT, STATUS)
*+
*  Name:
*     CAT5_GLFMT
*  Purpose:
*     Determine the width of an STL table format.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAT5_GLFMT (EXFMT; LXFMT; STATUS)
*  Description:
*     Determine the width (that is the number of characters required by)
*     of an STL table format.
*
*     Note that this version handles only simple angular formats.
*  Arguments:
*     EXFMT  =  CHARACTER*(*) (Given)
*           A StarBase external format specifier.
*     LXFMT  =  INTEGER (Returned)
*           The width (that is, the number of spaces required by) the
*           StarBase external format specifier.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     If the specifier is not blank then
*       Take a local copy of the specifier
*       Check whether the specifier contains a full stop.
*       If so then
*         Replace the full stop and all following characters with
*         spaces.
*       end if
*       Replace any alphabetic characters with spaces.
*       Remove any leading and embedded blanks.
*       Attempt to extract an integer number from the string.
*       If this extraction failed then
*         set the length to one.
*       end if
*     else (the specifier is blank)
*       set the length to one.
*     end if
*  Implementation Deficiencies:
*     Note that this version handles only simple angular formats.
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
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     19/11/96 (ACD): Original version (from CAP_GLFMT).
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'CAT_PAR'           ! External CAT constants.
*  Arguments Given:
      CHARACTER
     :  EXFMT*(*)
*  Arguments Returned:
      INTEGER
     :  LXFMT
*  Status:
      INTEGER STATUS             ! Global status
*  External References:
      INTEGER CHR_LEN
      LOGICAL CHR_ISDIG
*  Local Variables:
      CHARACTER
     :  CXFMT*(CAT__SZEXF)  ! Copy for STL table format specifier.
      INTEGER
     :  LCXFMT,  ! Length of CXFMT (excl. trail. blanks).
     :  LOOP,    ! Loop index.
     :  DOTPOS,  ! Position of first full stop in CXFMT.
     :  ISTAT    ! Local status.
      LOGICAL
     :  FOUND,   ! Flag: full stopp found in CXFMT?
     :  DIGIT    ! Flag: is the current character a digit?
*.

      IF (STATUS .EQ. CAT__OK) THEN

*
*       Check that the string is not blank.

         IF (EXFMT .NE. ' ') THEN

C           print3000, exfmt
C3000       format(1x, 'LXFMT on entry, exfmt: ', a / )

*
*          Take a local copy of the string (which will be modified) and
*          determine its length.

            CXFMT = EXFMT
            LCXFMT = CHR_LEN(CXFMT)

*
*          Check if this string contains a full stop.

            FOUND = .FALSE.

            DO LOOP = 1, LCXFMT
               IF (.NOT. FOUND) THEN
                  IF (CXFMT(LOOP : LOOP) .EQ. '.') THEN
                     FOUND = .TRUE.
                     DOTPOS = LOOP
                  END IF
               END IF
            END DO

*
*          If a full stop was found then replace it and all the
*          characters which follow it with spaces.

            IF (FOUND) THEN
               DO LOOP = DOTPOS, LCXFMT
                  CXFMT(LOOP : LOOP) = ' '
               END DO
            END IF

*
*          Replace any non-numeric characters with spaces.

            DO LOOP = 1, LCXFMT
               DIGIT = CHR_ISDIG(CXFMT(LOOP : LOOP) )

               IF (.NOT. DIGIT) THEN
                  CXFMT(LOOP : LOOP) = ' '
               END IF
            END DO

*
*          Remove all the leading and embedded blanks.

            CALL CHR_RMBLK (CXFMT)

*
*          Attempt to extract an integer number from the string.

            ISTAT = CAT__OK
            CALL CHR_CTOI (CXFMT, LXFMT, ISTAT)

*
*          Set the length to one if this extraction failed.

            IF (ISTAT .NE. CAT__OK) THEN
               LXFMT = 1
            END IF

         ELSE

*
*          The external format is blank.  Set the length to 1.

            LXFMT = 1
         END IF

C        print3001, exfmt, lxfmt
C3001    format(1x, 'LXFMT on exit, exfmt, lxfmt: ', a, 2x, i4/ )

      END IF

      END
