************************************************************************

      SUBROUTINE AGI_IMORE ( PICID, LMORE, STATUS )

*+
*  Name:
*     AGI_IMORE
*
*  Purpose:
*     Inquire if a MORE structure exists
*
*  Invocation:
*     CALL AGI_IMORE( PICID, LMORE, STATUS )
*
*  Description:
*     Inquire if a MORE structure exists for the given picture. If the
*     given value for PICID is negative then the current picture is used.
*     The return argument is true if a MORE structure exists for the
*     picture otherwise it is false.
*
*  Arguments:
*     PICID = INTEGER (Given)
*        Picture identifier
*     LMORE = LOGICAL (Returned)
*        Locator to the transformation structure
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get details of the specified picture.
*     Get a locator to the specified picture.
*     Inquire if a MORE structure exists for this picture.
*
*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     Jul 1992 (NE):
*        Original version
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'agi_nam'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_pfree'

*  Arguments Given :
      INTEGER PICID

*  Arguments Returned :
      LOGICAL LMORE

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND

      INTEGER PICNUM

      CHARACTER * ( DAT__SZLOC ) PICLOC, PSTLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

*   Check status on entry
      LMORE = .FALSE.
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Obtain the workstation and picture number from the picture id
*   If PICID is less than 0 then use the current picture
      IF ( PICID .LT. 0 ) THEN
         WKNAME = CAGIWK( CURPID )
         PICNUM = CPICNM( CURPID )

*   Otherwise use the specified picture
      ELSEIF ( ( PICID .GT. 0 ) .AND. ( PICID .LE. FRELEN ) ) THEN
         WKNAME = CAGIWK( PICID )
         PICNUM = CPICNM( PICID )

*   Else the picture identifier is invalid
      ELSE
         STATUS = AGI__IMPID
         CALL ERR_REP( 'AGI_IMORE_IMPID',
     :                 'Picture identifier is improper', STATUS )
         GOTO 99
      ENDIF

*   Get a locator to the required picture
      CALL AGI_1FDB( FOUND, STATUS )
      IF ( FOUND ) THEN
         WKSLOC = ' '
         CALL AGI_1FWORK( WKNAME, WKSLOC, FOUND, STATUS )
         IF ( FOUND ) THEN
            PSTLOC = ' '
            CALL AGI_1FPST( WKSLOC, PSTLOC, FOUND, STATUS )
            IF ( FOUND ) THEN
               PICLOC = ' '
               CALL AGI_1FPIC( PSTLOC, PICNUM, PICLOC, FOUND, STATUS )
               CALL DAT_ANNUL( PSTLOC, STATUS )
               PSTLOC = ' '
            ENDIF
            CALL DAT_ANNUL( WKSLOC, STATUS )
            WKSLOC = ' '
         ENDIF
      ENDIF

*   If the picture was found then see if there is a more object
      IF ( FOUND ) THEN

*   See if the transformation structure is there
         CALL DAT_THERE( PICLOC, AGI__MONAM, LMORE, STATUS )

*   Annul the picture locator
         CALL DAT_ANNUL( PICLOC, STATUS )
         PICLOC = ' '

*   Otherwise report an error
      ELSE
         STATUS = AGI__PICNF
         CALL ERR_REP( 'AGI_IMORE_PICNF', 'Picture not found', STATUS )
         GOTO 99
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_IMORE +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

