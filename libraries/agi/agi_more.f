************************************************************************

      SUBROUTINE AGI_MORE ( PICID, ACMODE, MORLOC, STATUS )

*+
*  Name:
*     AGI_MORE
*
*  Purpose:
*     Return an HDS locator to a MORE structure
*
*  Invocation:
*     CALL AGI_MORE( PICID, ACMODE, MORLOC, STATUS )
*
*  Description:
*     An HDS locator that points to a MORE structure in the database
*     is returned. A MORE structure can be associated with any picture
*     and it can be used to store any application specific information.
*     If the given value for PICID is negative then the current picture
*     is used. If the access mode is 'WRITE' then an empty MORE structure
*     is created if none existed. If there is an existing structure then
*     'WRITE' mode will erase the existing contents and return a locator
*     to an empty structure. If the access mode is 'READ' or 'UPDATE' then
*     a locator to an existing structure is returned. In this case an
*     error is returned if there is not a MORE structure for the given
*     picture. The database is not responsible for what goes in the MORE
*     structure or how the information is used. The application is also
*     responsible for annulling the returned locator.
*
*  Arguments:
*     PICID = INTEGER (Given)
*        Picture identifier
*     ACMODE = CHARACTER*(*) (Given)
*        Access mode for MORE structure. 'READ', 'WRITE' or 'UPDATE'.
*     MORLOC = CHARACTER*(DAT__SZLOC) (Returned)
*        Locator to the transformation structure
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Convert the acces mode to upper case.
*     Get details of the specified picture.
*     Get a locator to the specified picture.
*     If there is a more structure already there then
*        If the mode is WRITE then
*           Erase the existing structure and create an empty one.
*        Endif.
*        Return a locator to the more structure.
*     Else if there is no more structure
*        If the mode is WRITE then
*           Create an empty structure and return the locator.
*        Else for READ or UPDATE access
*           Return an error as there is no structure to access.
*        Endif
*     Endif
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
      INTEGER PICID
      CHARACTER * ( * ) ACMODE

*  Arguments Returned :
      CHARACTER * ( DAT__SZLOC ) MORLOC

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND, GOTIT

      INTEGER PICNUM

      CHARACTER LACMOD * 64
      CHARACTER * ( DAT__SZLOC ) PICLOC, PSTLOC, WKSLOC
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Copy the access mode to a local variable and convert to upper case
      LACMOD = ACMODE
      CALL CHR_LDBLK( LACMOD )
      CALL CHR_UCASE( LACMOD )

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
         CALL ERR_REP( 'AGI_MORE_IMPID',
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
         CALL DAT_THERE( PICLOC, AGI__MONAM, GOTIT, STATUS )

*   If the more structure is there then
         IF ( GOTIT ) THEN

*   If the access mode is write then erase the existing structure
*   and create an empty one in its place
            IF ( LACMOD .EQ. 'WRITE' ) THEN
               CALL DAT_ERASE( PICLOC, AGI__MONAM, STATUS )
               CALL DAT_NEW( PICLOC, AGI__MONAM, AGI__MONAM, 0, 0,
     :                       STATUS )

*   Indicate that the database has been updated
               FLUSH = .TRUE.
            ENDIF

*   Get a locator to the more structure
            CALL DAT_FIND( PICLOC, AGI__MONAM, MORLOC, STATUS )

*   If the more structure is not there then
         ELSE

*   If the access mode is write then create an empty structure
*   and get a locator to it
            IF ( LACMOD .EQ. 'WRITE' ) THEN
               CALL DAT_NEW( PICLOC, AGI__MONAM, AGI__MONAM, 0, 0,
     :                       STATUS )
               CALL DAT_FIND( PICLOC, AGI__MONAM, MORLOC, STATUS )

*   Indicate that the database has been updated
               FLUSH = .TRUE.

*   For read and update access no structure equals an error
            ELSE
               MORLOC = ' '
               STATUS = AGI__MORNF
               CALL ERR_REP( 'AGI_MORE_MORNF',
     :                       'More structure not found', STATUS )
            ENDIF

         ENDIF

*   Annul the picture locator
         CALL DAT_ANNUL( PICLOC, STATUS )
         PICLOC = ' '

*   Otherwise report an error
      ELSE
         STATUS = AGI__PICNF
         CALL ERR_REP( 'AGI_MORE_PICNF', 'Picture not found', STATUS )
         GOTO 99
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_MORE +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

