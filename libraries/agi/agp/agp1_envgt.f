      SUBROUTINE AGP1_ENVGT( NAME, TRANS, OK, STATUS )
*+
*  Name:
*     AGP1_ENVGT

*  Purpose:
*     Attempt to translate an environment variable.

*  Invocation:
*     CALL AGP1_ENVGT( NAME, TRANS, OK, STATUS )

*  Description:
*     This routine attempts to find the value of an environment variable
*     with the supplied name. If no such environment variable exists,
*     OK is returned .FALSE., but no error is reported.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The environment variable name.
*     TRANS = CHARACTER * ( * ) (Returned)
*        The value of the enviroment variable.
*     OK = LOGICAL (Returned)
*        .TRUE. if the environment variables exists, .FALSE. otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)

*  History:
*     31-OCT-2001 (DSB):
*        Original version.
*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSX_ERR'

*  Arguments Given:
      CHARACTER NAME*(*)

*  Arguments Returned:
      CHARACTER TRANS*(*)
      LOGICAL OK

*  Status:
      INTEGER STATUS
*.

*  Initialize.
      TRANS = ' '
      OK = .FALSE.

*  Check the inherited status. Also return if the NAME is blank.
      IF( STATUS .NE. SAI__OK .OR. NAME .EQ. ' ' ) RETURN

*  Mark the error stack so that we can annull any error that occurs
*  in PSX_GETENV (this routine may have been called from a non_ADAM task
*  without error deferall).
      CALL ERR_MARK

*  Attempt to translate the environment variable.
      CALL PSX_GETENV( NAME, TRANS, STATUS )

*  If no such environment variable exists, annul the error, and clear the
*  translation.
      IF( STATUS .EQ. PSX__NOENV ) THEN
         CALL ERR_ANNUL( STATUS )
         TRANS = ' '

*  If the environment variable exists, return a .TRUE. value for OK.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         OK = .TRUE.
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
