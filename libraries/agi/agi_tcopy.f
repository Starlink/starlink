************************************************************************

      SUBROUTINE AGI_TCOPY ( TRNLOC, PICID, STATUS )

*+
*  Name:
*     AGI_TCOPY
*
*  Purpose:
*     Copy a transformation structure to the database
*
*  Invocation:
*     CALL AGI_TCOPY( TRNLOC, PICID, STATUS )
*
*  Description:
*     The transformation pointed to by the HDS locator is stored in
*     the database. The picture identifier signifies which picture is
*     to receive the transformation structure. If this identifier is
*     negative then the current picture will be used. If a transformation
*     already exists for this picture then an error will be returned.
*     The supplied transformation should convert data coordinates into
*     the world coordinates of the database picture, as if it had been
*     created with a call to AGI_TNEW.
*
*  Arguments:
*     TRNLOC = CHARACTER*(DAT__SZLOC) (Given)
*        Locator to the transformation structure
*     PICID = INTEGER (Given)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Verify the supplied transformation.
*     Verify the number of data and world coordinates.
*     Obtain the workstation id and picture number from the picture id.
*     Get a locator to the picture.
*     Check that there is no existing transformation structure.
*     Copy the transformation structure to the database
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
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      CHARACTER * ( * ) TRNLOC
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND, GOTIT

      INTEGER NCD, NCW, PICNUM

      CHARACTER * ( DAT__SZLOC ) PICLOC, PSTLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Verify the supplied transformation
      CALL TRN_GTNV( TRNLOC, NCD, NCW, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Verify the number of world coordinates
      IF ( NCW .NE. 2 ) THEN
         STATUS = AGI__TRINV
         CALL ERR_REP( 'AGI_TCOPY_INVWC',
     :                 'Invalid number of world coordinates', STATUS )
         GOTO 99
      ENDIF

*   This implementation only allows 2-dimensional data coordinates
*   Verify the number of data coordinates
      IF ( NCD .NE. 2 ) THEN
         STATUS = AGI__TRINV
         CALL ERR_REP( 'AGI_TCOPY_INVDC',
     :                 'Invalid number of data coordinates', STATUS )
         GOTO 99
      ENDIF

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
         CALL ERR_REP( 'AGI_TCOPY_IMPID',
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

*   If the picture was found then find the transformation object
      IF ( FOUND ) THEN

*   See if the transformation structure is there
         CALL DAT_THERE( PICLOC, AGI__TRANS, GOTIT, STATUS )

*   If so report an error
         IF ( GOTIT ) THEN
            CALL DAT_ANNUL( PICLOC, STATUS )
            PICLOC = ' '
            STATUS = AGI__TRNEX
            CALL ERR_REP( 'AGI_TCOPY_TRNEX',
     :                    'Transformation already exists', STATUS )
            GOTO 99
         ENDIF

*   Copy the transformation structure to the database
         CALL DAT_COPY( TRNLOC, PICLOC, AGI__TRANS, STATUS )

*   Annul the picture locator
         CALL DAT_ANNUL( PICLOC, STATUS )
         PICLOC = ' '

*   Indicate that the database has been updated
         FLUSH = .TRUE.

*   Otherwise report an error
      ELSE
         STATUS = AGI__PICNF
         CALL ERR_REP( 'AGI_TCOPY_PICNF', 'Picture not found', STATUS )
         GOTO 99
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_TCOPY +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

