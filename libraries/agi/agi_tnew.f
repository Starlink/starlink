************************************************************************

      SUBROUTINE AGI_TNEW ( NCD, NCW, DTOW, WTOD, PICID, STATUS )

*+
*  Name:
*     AGI_TNEW
*
*  Purpose:
*     Store a transformation in the database
*
*  Invocation:
*     CALL AGI_TNEW ( NCD, NCW, DTOW, WTOD, PICID, STATUS )
*
*  Description:
*     The transformation defined by the pseudo-code FORTRAN statements
*     DTOW and WTOD is stored in the database. The picture identifier
*     signifies which picture is to receive the transformation
*     structure. If this identifier is negative then the current picture
*     will be used. If a transformation already exists for this picture
*     then an error will be returned. The number of world variables NCW
*     has to be equal to 2 otherwise an error will be returned. The
*     number of data variables NCD also has to equal 2 in the present
*     implementation.
*
*  Arguments:
*     NCD = INTEGER (Given)
*        Number of data variables
*     NCW = INTEGER (Given)
*        Number of world variables
*     DTOW = CHARACTER*(*)(NCW) (Given)
*        Array of forward transformation functions
*     WTOD = CHARACTER*(*)(NCD) (Given)
*        Array of inverse transformation functions
*     PICID = INTEGER (Given)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Verify the number of data and world coordinates.
*     Obtain the workstation id and picture number from the picture id.
*     Get a locator to the picture.
*     Check that there is no existing transformation structure.
*     Put the new transformation into the database.
*
*  Copyright:
*     Copyright (C) 1989, 1990, 1991 Science & Engineering Research Council.
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
*     Sep 1989 (NE):
*        Original version
*     Nov 1989 (NE):
*        Report error if transformation exists
*     Aug 1990 (NE):
*        Updated error reporting
*     Jun 1991 (NE):
*        Allow for elastic precision
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
      INTEGER NCD
      INTEGER NCW
      CHARACTER * ( * ) DTOW( NCW )
      CHARACTER * ( * ) WTOD( NCD )
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND, GOTIT

      INTEGER PICNUM

      CHARACTER * ( DAT__SZLOC ) PICLOC, PSTLOC, TRNLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) THEN
         GOTO 99
      ENDIF

*   Verify the number of world coordinates
      IF ( NCW .NE. 2 ) THEN
         STATUS = AGI__TRINV
         CALL ERR_REP( 'AGI_TNEW_INVWC',
     :                 'Invalid number of world coordinates', STATUS )
         GOTO 99
      ENDIF

*   This implementation only allows 2-dimensional data coordinates
*   Verify the number of data coordinates
      IF ( NCD .NE. 2 ) THEN
         STATUS = AGI__TRINV
         CALL ERR_REP( 'AGI_TNEW_INVDC',
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
         CALL ERR_REP( 'AGI_TNEW_IMPID',
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
            CALL ERR_REP( 'AGI_TNEW_TRNEX',
     :                    'Transformation already exists', STATUS )
            GOTO 99
         ENDIF

*   Create the new transformation in the picture structure
         CALL TRN_NEW( NCD, NCW, DTOW, WTOD, '_REAL:', 'Transformation',
     :                 PICLOC, AGI__TRANS, TRNLOC, STATUS )

*   Annul the tranformation locator as it is not needed
         CALL DAT_ANNUL( TRNLOC, STATUS )
         TRNLOC = ' '

*   Annul the picture locator
         CALL DAT_ANNUL( PICLOC, STATUS )
         PICLOC = ' '

*   Indicate that the database has been updated
         FLUSH = .TRUE.

*   Otherwise report an error
      ELSE
         STATUS = AGI__PICNF
         CALL ERR_REP( 'AGI_TNEW_PICNF', 'Picture not found', STATUS )
         GOTO 99
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_TNEW +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

