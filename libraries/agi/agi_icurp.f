************************************************************************

      SUBROUTINE AGI_ICURP ( PICID, STATUS )

*+
*  Name:
*     AGI_ICURP
*
*  Purpose:
*     Inquire the current picture
*
*  Invocation:
*     CALL AGI_ICURP( PICID, STATUS )
*
*  Description:
*     The picture identifier of the current picture is returned.
*
*  Arguments:
*     PICID = INTEGER (Returned)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     If the current picture identifier is valid then return it.
*
*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     NE: Nick Eaton (Durham University)
*
*  History:
*     July 1990 (NE):
*        Original version
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_pfree'

*  Arguments Returned :
      INTEGER PICID

*  Status :
      INTEGER STATUS
*.

*   Check status on entry
      PICID = 0
      IF ( STATUS .EQ. SAI__OK ) THEN

*   If the current picture identifier is valid then return it
         IF ( CURPID .GT. 0 ) THEN
            PICID = CURPID

*   Otherwise flag an error
         ELSE
            STATUS = AGI__NOCUP
            CALL ERR_REP( 'AGI_ICURP_NOCUP', 'No current picture',
     :                    STATUS )
         ENDIF

      ENDIF

      END

