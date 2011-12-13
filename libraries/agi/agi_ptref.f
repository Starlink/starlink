************************************************************************

      SUBROUTINE AGI_PTREF ( DATREF, PICID, STATUS )

*+
*  Name:
*     AGI_PTREF
*
*  Purpose:
*     Store a reference object in a picture
*
*  Invocation:
*     CALL AGI_PTREF( DATREF, PICID, STATUS )
*
*  Description:
*     This creates a reference to a data object in the database. The
*     argument can be either an HDS locator or any character string
*     reference. If the string is a valid HDS locator then a reference
*     is constructed to point to the relevant object, otherwise the
*     string is assumed to be a reference itself and is stored as
*     supplied. The picture identifier signifies which picture to put
*     the reference into. If this identifier is negative then the
*     current picture is used. If a reference already exists for the
*     given picture then an error is returned.
*
*  Arguments:
*     DATREF = CHARACTER*(*) (Given)
*        String containing reference object
*     PICID = INTEGER (Given)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Check if the given reference is a valid HDS locator or not.
*     Obtain the workstation id and picture number from the picture id.
*     Get a locator to the required picture.
*     If the picture was found then create the reference object.
*
*  Copyright:
*     Copyright (C) 1989, 1991 Science & Engineering Research Council.
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
*     Aug 1989 (NE):
*        Original version
*     Mar 1991 (NE):
*        Accept any character string as a reference
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

*  Arguments Given
      CHARACTER * ( * ) DATREF
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND, HDSLOC

      INTEGER LENS, PICNUM

      CHARACTER * ( DAT__SZLOC ) PICLOC, PSTLOC, REFLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) THEN
         GOTO 99
      ENDIF

*   See if the input argument is a valid HDS locator
      CALL DAT_VALID( DATREF, HDSLOC, STATUS )

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
         CALL ERR_REP( 'AGI_PTREF_IMPID',
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

*   If the picture was found then create the reference object
*   Use a local status value to trap errors in REF.
      IF ( FOUND ) THEN

*   If the reference object is already there then indicate an error
         CALL DAT_THERE( PICLOC, AGI__REFOB, FOUND, STATUS )
         IF ( FOUND ) THEN
            CALL DAT_ANNUL( PICLOC, STATUS )
            PICLOC = ' '
            STATUS = AGI__REFEX
            CALL ERR_REP( 'AGI_PTREF_REFEX',
     :                    'Reference already exists', STATUS )
            GOTO 99

*   Otherwise create a new one
         ELSE

*   Use the REF facility if the reference is an HDS locator
            IF ( HDSLOC ) THEN
               CALL REF_CRPUT( PICLOC, AGI__REFOB, DATREF, .FALSE.,
     :                         STATUS )

*   Otherwise store the string as supplied
            ELSE
               LENS = LEN( DATREF )
               CALL DAT_NEW0C( PICLOC, AGI__REFOB, LENS, STATUS )
               CALL DAT_FIND( PICLOC, AGI__REFOB, REFLOC, STATUS )
               CALL DAT_PUT0C( REFLOC, DATREF, STATUS )
               CALL DAT_ANNUL( REFLOC, STATUS )
               REFLOC = ' '
            ENDIF

*   Annul the picture locator
            CALL DAT_ANNUL( PICLOC, STATUS )
            PICLOC = ' '
         ENDIF

*   Indicate that the database has been updated
         FLUSH = .TRUE.

*   Otherwise report an error
      ELSE
         STATUS = AGI__PICNF
         CALL ERR_REP( 'AGI_PTREF_PICNF', 'Picture not found', STATUS )
         GOTO 99
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_PTREF +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

