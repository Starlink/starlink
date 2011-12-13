      SUBROUTINE CCD1_HDRRN( KEYWRD, FIRST, LAST, INSERT, STATUS )
*+
*  Name:
*     CCD1_HDRRN

*  Purpose:
*     Determine a range of FITS keywords.

*  Language:
*     Fortran-77.

*  Invocation:
*     CALL CCD1_HDRRN( KEYWRD, FIRST, LAST, INSERT, STATUS )

*  Description:
*     This routine decodes an expression like "KEYWRD[0-9]" into a range
*     0 to 9 and also returns the position of first occurrence of "[".
*     It is used to decode such an expression into a possible range
*     of values and where to replace the range part of the string
*     from when encoding and decoding long strings into FITS headers.

*  Arguments:
*     KEYWRD = CHARACTER * ( * ) (Given)
*        The range containing expression to decode into a numeric
*        range and an replacement point.
*     FIRST = INTEGER (Returned)
*        The first value in the range part of the expression.
*     LAST = INTEGER (Returned)
*        The last value in the range part of the expression.
*     INSERT  = INTEGER (Returned)
*        The position in the expression of the first character of the
*        range.
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-MAR-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Starlink constants

*  Arguments Given:
      CHARACTER * ( * ) KEYWRD

*  Arguments Returned:
      INTEGER FIRST
      INTEGER LAST
      INTEGER INSERT

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of a string
      EXTERNAL CHR_ISDIG
      LOGICAL CHR_ISDIG         ! Is character a digit

*  Local Variables:
      INTEGER I1, I2, I3, I4    ! String points
      INTEGER I                 ! Loop variable
      LOGICAL FAILED            ! Expression parse failed

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN


*  Locate the first character of the range.
      I1 = INDEX( KEYWRD, '[' )
      IF ( I1 .EQ. 0 ) THEN

*  No range. Look for numeric end of string.
         I1 = CHR_LEN( KEYWRD )
         I2 = I1
         DO 1 I = I1, 1, -1
            IF ( .NOT. CHR_ISDIG( KEYWRD( I: I ) ) ) THEN
               I2 = I + 1
               IF ( I2 .LE. I1 ) THEN
                  CALL CHR_CTOI( KEYWRD( I2: I1 ), FIRST, STATUS )
                  LAST = FIRST
                  INSERT = I2
                  GO TO 2
               END IF
            END IF
 1       CONTINUE
*  Only arrive here if no numeric end to string. In this case give up.
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'KEY', KEYWRD )
         CALL ERR_REP( 'CCD1_HDRRN', '  CCD1_HDRRN: Failed to' //
     :   ' extract value in keyword expression: ^KEY',
     :                STATUS )
 2       CONTINUE
      ELSE

*  Locate "-" and decode first value.
         FAILED = .FALSE.
         I2 = INDEX( KEYWRD, '-' )
         IF ( I2 .NE. 0 ) THEN
            CALL CHR_CTOI( KEYWRD( I1 + 1: I2 - 1 ), FIRST, STATUS )

*  Now look for closing "]".
            I3 = INDEX( KEYWRD, ']' )
            IF ( I3 .NE. 0 ) THEN
               CALL CHR_CTOI( KEYWRD( I2 + 1: I3 - 1 ), LAST, STATUS )
               INSERT = I1
            ELSE
               FAILED = .TRUE.
            END IF
         ELSE
            FAILED = .TRUE.
         END IF
         IF ( FAILED ) THEN
            IF ( STATUS .EQ. SAI__OK ) STATUS = SAI__ERROR
            CALL MSG_SETC( 'KEY', KEYWRD )
            CALL ERR_REP( 'CCD1_HDRRN', '  CCD1_HDRRN: Failed to' //
     :      ' decode keyword expression - ^KEY - into a valid range',
     :                   STATUS )
         END IF
      END IF

      END
* $Id$
