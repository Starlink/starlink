      SUBROUTINE TASK_ENC0L ( LVAL, STRING, STATUS )
*+
*  Name:
*     TASK_ENC0L

*  Purpose:
*     Encode a value as a character string

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL TASK_ENC0L ( LVAL, STRING, STATUS )

*  Description:
*     Convert the given value of type LOGICAL into a character
*     string and return it in STRING.
*     A routine exists for each type C, D, L, I, R.

*  Arguments:
*     LVAL=LOGICAL (given)
*           the value to be encoded
*     STRING=CHARACTER*(*) (returned)
*           the returned character string
*     STATUS=INTEGER

*  Algorithm:
*     Use appropriate CHR routine

*  Copyright:
*     Copyright (C) 1987, 1989, 1992 Science & Engineering Research
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
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     06-NOV-1987 (REVAD::BDK):
*        Original
*     29-APR-1989 (AAOEPP::WFL):
*        Make it generic
*     04-OCT-1992 (RLVAD::AJC):
*        Use CHR
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      LOGICAL LVAL         ! the value to be encoded

*  Arguments Returned:
      CHARACTER*(*) STRING  ! the returned character string

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER NCHAR         ! length of encoded string
*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL CHR_LTOC( LVAL, STRING, NCHAR )

      END
