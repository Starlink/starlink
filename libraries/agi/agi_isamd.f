************************************************************************

      SUBROUTINE AGI_ISAMD ( PICID, LSAME, STATUS )

*+
*  Name:
*     AGI_ISAMD
*
*  Purpose:
*     Inquire if pictures are on same device
*
*  Invocation:
*     CALL AGI_ISAMD( PICID, LSAME, STATUS )
*
*  Description:
*     Inquire if the given picture is on the same device as the current
*     picture.
*
*  Arguments:
*     PICID = INTEGER (Given)
*        Picture identifier
*     LSAME = LOGICAL (Returned)
*        True if pictures on same device, otherwise false.
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get the workstation name of the current picture.
*     Get the workstation name of the given picture.
*     Compare the names.
*
*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     Aug 1988 (NE):
*        Original version
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'

*  Global variables :
      INCLUDE 'agi_pfree'

*  Arguments Given :
      INTEGER PICID

*  Arguments Returned :
      LOGICAL LSAME

*  Status :
      INTEGER STATUS

*  Local variables :
      CHARACTER * ( DAT__SZNAM ) TWKNAM, WKNAME
*.

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Initialise LSAME
         LSAME = .FALSE.

*   Get details of the current picture
         IF ( CURPID .GT. 0 ) THEN
            WKNAME = CAGIWK( CURPID )

*   Get details of the given picture
            IF ( PICID .GT. 0 ) THEN
               TWKNAM = CAGIWK( PICID )

*   Test if the workstation names are the same
               IF ( WKNAME .EQ. TWKNAM ) THEN
                  LSAME = .TRUE.
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      END

