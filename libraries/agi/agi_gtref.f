************************************************************************

      SUBROUTINE AGI_GTREF ( PICID, MODE, DATREF, STATUS )

*+
*  Name:
*     AGI_GTREF
*
*  Purpose:
*     Get a reference object from a picture
*
*  Invocation:
*     CALL AGI_GTREF ( PICID, MODE, DATREF, STATUS )
*
*  Description:
*     This returns a reference to a data object which has been stored
*     in the database. The picture identifier signifies which picture
*     to obtain the reference from. If this identifier is negative then
*     the current picture is used. If no reference object is found for
*     the given picture an error is returned. If the reference was
*     created from an HDS locator then an HDS locator is returned, and
*     this should be annulled in the application using REF_ANNUL to
*     ensure the file is properly closed.
*
*  Arguments:
*     PICID = INTEGER (Given)
*        Picture identifier
*     MODE = CHARACTER*(*) (Given)
*        Access mode for object, 'READ', 'WRITE', or 'UPDATE'
*     DATREF = CHARACTER*(*) (Returned)
*        String containing reference object
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Annul the output string if it is a valid HDS locator.
*     Obtain the workstation id and picture number from the picture id.
*     Get a locator to the picture.
*     Check the reference object is there and of the correct type.
*     Get the data locator from the reference object.
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
*     NE: Nick Eaton  (Durham University)
*
*  History:
*     Aug 1989 (NE):
*        Original version
*     Mar 1991 (NE):
*        Allow any character string to be a reference
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
      INTEGER PICID

      CHARACTER * ( * ) MODE

*  Arguments Returned :
      CHARACTER * ( * ) DATREF

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND, GOTIT, YESNO

      INTEGER PICNUM

      CHARACTER * ( DAT__SZLOC ) PARLOC, PICLOC, PSTLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) WKNAME
      CHARACTER * ( DAT__SZTYP ) OTYPE
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Annul the locator if it is already valid
      CALL DAT_VALID( DATREF, YESNO, STATUS )
      IF ( YESNO ) THEN
         CALL DAT_ANNUL( DATREF, STATUS )
         DATREF = ' '
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
         CALL ERR_REP( 'AGI_GTREF_IMPID',
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

*   If the picture was found then find the reference object
      IF ( FOUND ) THEN

*   Check the reference structure is there
         CALL DAT_THERE( PICLOC, AGI__REFOB, GOTIT, STATUS )
         IF ( GOTIT ) THEN
            CALL DAT_FIND( PICLOC, AGI__REFOB, PARLOC, STATUS )

*   See if the reference was created by REF
            CALL DAT_TYPE( PARLOC, OTYPE, STATUS )
            IF ( OTYPE .EQ. 'REFERENCE_OBJ' ) THEN
               CALL REF_GET( PARLOC, MODE, DATREF, STATUS )

*   Otherwise read the reference into the output argument
            ELSE
               CALL DAT_GET0C( PARLOC, DATREF, STATUS )
            ENDIF

*   Annul the parameter and picture locators
            CALL DAT_ANNUL( PARLOC, STATUS )
            PARLOC = ' '
         ENDIF
         CALL DAT_ANNUL( PICLOC, STATUS )
         PICLOC = ' '

*   If the reference was not found then flag an error
         IF ( .NOT. GOTIT ) THEN
            STATUS = AGI__REFNF
            CALL ERR_REP( 'AGI_GTREF_REFNF',
     :                    'Reference object not found', STATUS )
            GOTO 99
         ENDIF

*   Otherwise report an error
      ELSE
         STATUS = AGI__PICNF
         CALL ERR_REP( 'AGI_GTREF_PICNF', 'Picture not found', STATUS )
         GOTO 99
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_GTREF +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

