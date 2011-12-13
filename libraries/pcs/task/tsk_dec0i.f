      SUBROUTINE TASK_DEC0I ( STRING, IVAL, STATUS )
*+
*  Name:
*     TASK_DEC0I

*  Purpose:
*     Decode a character string as a value

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_DEC0I ( STRING, IVAL, STATUS )

*  Description:
*     Convert the given character string into a value of type
*     INTEGER and return it in IVAL.
*     A routine exists for each type C, D, L, I, R.

*  Arguments:
*     STRING=CHARACTER*(*) (given)
*           the string to be decoded
*     IVAL=INTEGER (returned)
*           the returned value
*     STATUS=INTEGER

*  Algorithm:
*     Use CHR_CTOI. If that fails try CHR_CTOD and INT.
*     (This is a change from previous behaviour which found the nearest
*     integer but it now does the same as SUBPAR)
*     If that fails try CHR_CTOL setting 1 if true and 0 if false.
*     (This is a change, previously these values would not be converted.)

*  Copyright:
*     Copyright (C) 1989, 1992-1993 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     W.F.Lupton (AAOEPP::WFL)
*     A J Chpperifeld (RLVAD::AJC)
*     {enter_new_authors_here}

*  History:
*     29-APR-1989 (AAOEPP::WFL):
*        Original
*     04-OCT-1992 (RLVAD::AJC):
*        Use CHR for portability
*     02-SEP-1993 (RLVAD::AJC):
*        Remove unused ISTAT
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'TASK_ERR'

*  Arguments Given:
      CHARACTER*(*) STRING  ! the character string to be decoded

*  Arguments Returned:
      INTEGER IVAL         ! the returned value

*  Status:
      INTEGER STATUS

*  Local Variables:
      DOUBLE PRECISION DVAL ! value in double precision
      LOGICAL LVAL          ! value as logical

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Use appropriate CHR routine
      CALL CHR_CTOI( STRING, IVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
      CALL EMS_ANNUL( STATUS )
*     Attempt conversion via DOUBLE PRECISION
         CALL CHR_CTOD( STRING, DVAL, STATUS )

*     If OK find integer part of
         IF ( STATUS .EQ. SAI__OK ) THEN
            IVAL = INT( DVAL )

*     If it failed, try going via logical
         ELSE
            CALL EMS_ANNUL( STATUS )
            CALL CHR_CTOL( STRING, LVAL, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               IF ( LVAL ) THEN
                  IVAL = 1
               ELSE
                  IVAL = 0
               END IF

            ELSE
               CALL EMS_SETC( 'STR', STRING )
               CALL ERR_REP( 'TSK_DEC0L1',
     :         'TASK_DEC0I: Failed to convert ^STR to INTEGER',
     :          STATUS )
            END IF
         END IF

      END IF
      END
