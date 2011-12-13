      SUBROUTINE TASK_DEC0R ( STRING, RVAL, STATUS )
*+
*  Name:
*     TASK_DEC0R

*  Purpose:
*     Decode a character string as a value

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_DEC0R ( STRING, RVAL, STATUS )

*  Description:
*     Convert the given character string into a value of type
*     REAL and return it in RVAL.
*     A routine exists for each type C, D, L, I, R.

*  Arguments:
*     STRING=CHARACTER*(*) (given)
*           the string to be decoded
*     RVAL=REAL (returned)
*           the returned value
*     STATUS=INTEGER

*  Algorithm:
*     Use CHR_CTOR.

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
*     06-SEP-1993 (RLVAD::AJC):
*        Remove hagover from GENERIC system
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
      REAL RVAL         ! the returned value

*  Status:
      INTEGER STATUS

*  Local Variables:

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

*   Use appropriate CHR routine
      CALL CHR_CTOR( STRING, RVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL EMS_SETC( 'STR', STRING )
         CALL ERR_REP( 'TSK_DEC0L1',
     :   'TASK_DEC0R: Failed to convert ^STR to REAL',
     :    STATUS )
      ENDIF

      END
