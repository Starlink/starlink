************************************************************************

      SUBROUTINE AGI_ASSOC( PARAM, ACMODE, PICID, STATUS )

*+
*  Name:
*     AGI_ASSOC
*
*  Purpose:
*     Associate an AGI device with an ADAM parameter
*
*  Invocation:
*     CALL AGI_ASSOC( PARAM, ACMODE, PICID, STATUS )
*
*  Description:
*     Associate an AGI device with a parameter in the ADAM environment
*     and return an identifier to the current picture. If there are no
*     pictures on the device then a base picture is created and made
*     current. If the size of the display window has changed since a
*     previous database operation the database is cleared and a message
*     sent to the user. The access mode does not affect the database
*     operation, but it is used by the graphics system to determine if
*     the display should be cleared the first time the device is opened;
*     'READ' and 'UPDATE' access do not clear the display, but 'WRITE'
*     access does.
*
*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        Name of the parameter used for accessing the device
*     ACMODE = CHARACTER*(*) (Given)
*        Access mode: 'READ', 'WRITE' or 'UPDATE'
*     PICID = INTEGER (Returned)
*        Identifier for current picture on the given device
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Reset the PICID then check status on entry.
*     If the reference counter is zero initialise the common blocks.
*     Increment the reference counter.
*     If this parameter has been used before then return with the PICID.
*     Otherwise prompt for a device through the parameter system.
*     Translate the device name into an AGI name.
*     Get a picture identifier from the free list.
*     Fetch the current picture from the database.
*     Store the information in the common block.
*
*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1991, 1992 Science & Engineering Research Council.
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
*     Jul 1989 (NE):
*        Read database locator from common block
*     Aug 1989 (NE):
*        Reset PICID to zero
*     Jun 1990 (NE):
*         Argument list in AGI_1GWNAM  and AGI_1DEFBA changed
*         Initialise the graphics package flags
*         Added MEMID parameter
*     Nov 1990 (NE):
*         Added clear flag
*     Mar 1991 (NE):
*        Added call to AGI_1FETCU
*     Jul 1992 (NE):
*        Remove leading blanks from the parameter name
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'agi_nam'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_cref'
      INCLUDE 'agi_idips'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) ACMODE

*  Arguments Returned :
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  External references :
      EXTERNAL AGI_BLOCK

*  Local variables :
      LOGICAL DONE, GOTIT

      INTEGER CURPIC, I, ISTAT, NMCODE

      CHARACTER DEVNAM * 80, LACMOD * 64
      CHARACTER * ( DAT__SZNAM ) AGINAM
      CHARACTER * ( PAR__SZNAM ) LPARAM
*.

*   Initialise PICID to zero so that in the event of an abort
*   a valid PICID is not returned
      PICID = 0

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   If this is the first time round then do some initialisations
         IF ( CREF .EQ. 0 ) THEN

*   Initialise the picture identifier free list
            CALL AGI_1FINIT( FRELEN, FRELIS, NEXFRE )

*   Initialise the common blocks
            CALL AGI_1CINIT( STATUS )

*   Initialise the graphics package flags
            CGKSON = .FALSE.
            CPGPON = .FALSE.
            CIDION = .FALSE.
         ENDIF

*   Increment the reference counter
         CREF = CREF + 1

*   Copy parameter name to local variable and convert to upper case
         LPARAM = PARAM
         CALL CHR_LDBLK( LPARAM )
         CALL CHR_UCASE( LPARAM )

*   Look to see if this parameter name has been used already
         GOTIT = .FALSE.
         I = 1
         DO WHILE ( ( .NOT. GOTIT ) .AND. ( I .LE. FRELEN ) )
            IF ( LPARAM .EQ. PTNAME( I ) ) THEN
               GOTIT = .TRUE.
            ELSE
               I = I + 1
            ENDIF
         ENDDO

*   If the parameter has already been used then return the picid
         IF ( GOTIT ) THEN

            PICID = I

*   Otherwise read the name from the parameter system
         ELSE

            CALL SUBPAR_FINDPAR( LPARAM, NMCODE, STATUS )

*   Loop trying to get the name of the graphics device
            DONE = .FALSE.

            DO WHILE ( ( .NOT. DONE ) .AND. ( STATUS .EQ. SAI__OK ) )

*   Inquire the device name from the parameter system
               CALL SUBPAR_GETNAME( NMCODE, DEVNAM, STATUS )

*   If the user wants to abort then finish
               IF ( ( STATUS .EQ. PAR__NULL ) .OR.
     :              ( STATUS .EQ. PAR__ABORT ) .OR.
     :              ( STATUS .EQ. PAR__NOUSR ) ) THEN
                  DONE = .TRUE.

*   If some other error has occured then finish as well
               ELSEIF( STATUS .NE. SAI__OK ) THEN
                  DONE = .TRUE.

*   If status is OK check the device name
               ELSE

*   Try translating the device name
                  CALL AGI_1GWNAM( DEVNAM, ' ', AGINAM, STATUS )

*   An AGI name has been obtained if status is OK
                  IF ( STATUS .EQ. SAI__OK ) THEN

*   Use a local status for the free list access
                     ISTAT = SAI__OK

*   Get a picture identifier from the free list
                     CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, PICID,
     :                                ISTAT )

*   Check the status from the free list
                     IF ( ISTAT .NE. SAI__OK ) THEN
                        CALL PAR_CANCL( PARAM, STATUS )
                        STATUS = AGI__NOPID
                        CALL ERR_REP( 'AGI_ASSOC_NOPID',
     :                                'No more identifiers available',
     :                                STATUS )
                        GOTO 99
                     ELSE
                        CNUMID = CNUMID + 1
                     ENDIF

*   Fetch the current picture from the database
                     CALL AGI_1FETCU( DEVNAM, AGINAM, CURPIC, STATUS )

*   Copy the access mode to a local variable and convert to upper case
                     LACMOD = ACMODE
                     CALL CHR_LDBLK( LACMOD )
                     CALL CHR_UCASE( LACMOD )

*   Store the details in the common block
                     CGRAWK( PICID ) = DEVNAM
                     CAGIWK( PICID ) = AGINAM
                     CPICNM( PICID ) = CURPIC
                     PICACS( PICID ) = LACMOD
                     CLEVEL( PICID ) = CNEST
                     CURPID = PICID

*   If the access mode is 'WRITE' then remember this
                     IF ( LACMOD .EQ. 'WRITE' ) THEN

*   Look for an empty slot in the common blocks
                        GOTIT = .FALSE.
                        I = 1
                        DO WHILE ( ( .NOT. GOTIT ) .AND.
     :                             ( I .LE. CLRLEN ) )
                           IF ( CLEARF( I ) .EQ. 0 ) THEN
                              GOTIT = .TRUE.
                              CLEARF( I ) = -1
                              CLEARW( I ) = AGINAM
                           ELSE
                              I = I + 1
                           ENDIF
                        ENDDO
                     ENDIF

*   Save the parameter name in the common block to indicate that this
*   was the first picture to be accessed with this parameter.
*   This name is not copied to all the pictures on this device, which
*   enables subsequent calls to spot which picture was the first.
                     PTNAME( PICID ) = LPARAM

*   Everything is OK so do not repeat the loop
                     DONE = .TRUE.

*   Translation failed so get ready to try again
                  ELSE
                     CALL ERR_FLUSH( STATUS )
                     CALL PAR_CANCL( PARAM, STATUS )
                  ENDIF

               ENDIF

            ENDDO
         ENDIF

*   Flush HDS if the database file has been updated
         IF ( FLUSH ) THEN
*            CALL HDS_FREE( DABLOC, STATUS )
            FLUSH = .FALSE.
         ENDIF

      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_ASSOC +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

