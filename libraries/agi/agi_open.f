************************************************************************

      SUBROUTINE AGI_OPEN ( WKNAME, ACMODE, PICID, STATUS )

*+
*  Name:
*     AGI_OPEN
*
*  Purpose:
*     Open an AGI device in a non-ADAM environment
*
*  Invocation:
*     CALL AGI_OPEN( WKNAME, ACMODE, PICID, STATUS )
*
*  Description:
*     Open an AGI device and return an identifier to the current picture.
*     If there are no pictures on the device then a base picture is
*     created and made current. If the size of the display window has
*     changed since a previous database operation the database is cleared
*     and a message sent to the user. The access mode does not affect the
*     database operation, but it is used by the graphics system to
*     determine if the display should be cleared the first time a zone is
*     created; 'READ' and 'UPDATE' access do not clear the display, but
*     'WRITE' access does.
*
*  Arguments:
*     WKNAME = CHARACTER*(*) (Given)
*        Name of the device to open
*     ACMODE = CHARACTER*(*) (Given)
*        Access mode: 'READ', 'WRITE' or 'UPDATE'
*     PICID = INTEGER (Returned)
*        Identifier for current picture on the given device
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     If this is the first time this routine has run then
*        Initialise the common blocks and graphics package flags.
*     Endif
*     Get an AGI name for the given workstation name.
*     Get a picture identifier.
*     Open the database and the workstation structure.
*     If there is a current picture in the database then
*        Get its picture number.
*     Else
*        Define the base picture for this workstation.
*        Store the base picture in the database.
*     Endif
*     Put the details of the picture in the common block.
*
*  Copyright:
*     Copyright (C) 1988, 1990, 1991 Science & Engineering Research Council.
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
*        Orignal version
*     Jun 1990 (NE):
*        Argument list in AGI_1GWNAM and AGI_1DEFBA changed.
*        Initialise the graphics package flags.
*        Added MEMID parameter.
*     Nov 1990 (NE):
*        Added clear flag.
*     Mar 1991 (NE):
*        Added call to AGI_1FETCU
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
      INCLUDE 'agi_idips'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      CHARACTER * ( * ) WKNAME
      CHARACTER * ( * ) ACMODE

*  Arguments Returned :
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  External references :
      EXTERNAL AGI_BLOCK

*  Local variables :
      LOGICAL FIRST, GOTIT

      INTEGER CURPIC, I, ISTAT

      CHARACTER * ( DAT__SZNAM ) AGINAM
      CHARACTER LACMOD * 64

*  Local data :
      DATA FIRST /.TRUE./
      SAVE FIRST
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   If this is the first time round then do some initialisations
         IF ( FIRST ) THEN

*   Initialise the picture identifier free list
            CALL AGI_1FINIT( FRELEN, FRELIS, NEXFRE )

*   Initialise the common blocks
            CALL AGI_1CINIT( STATUS )

*   Initialise the graphics package flags
            CGKSON = .FALSE.
            CPGPON = .FALSE.
            CIDION = .FALSE.

*   Make sure this loop is not executed again
            FIRST = .FALSE.
         ENDIF

*   Translate the given workstation name into an AGI name
         CALL AGI_1GWNAM( WKNAME, ' ', AGINAM, STATUS )

*   If the translation has not succeeded then
         IF ( STATUS .NE. SAI__OK ) THEN
            GOTO 99
         ENDIF

*   Get a picture identifier from the free list
*   Use a local status for the free list access
         ISTAT = SAI__OK

*   Get a picture identifier from the free list
         CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, PICID, ISTAT )

*   If the exit status is not OK then the free list is full
         IF ( ISTAT .NE. SAI__OK ) THEN
            STATUS = AGI__NOPID
            CALL ERR_REP( 'AGI_OPEN_NOPID',
     :                    'No more identifiers available', STATUS )
            GOTO 99
         ELSE
            CNUMID = CNUMID + 1
         ENDIF

*   Fetch the current picture from the database
         CALL AGI_1FETCU( WKNAME, AGINAM, CURPIC, STATUS )

*   Copy the access mode to a local variable and convert to upper case
         LACMOD = ACMODE
         CALL CHR_LDBLK( LACMOD )
         CALL CHR_UCASE( LACMOD )

*   Put the workstation names and the current picture in the common block
         CGRAWK( PICID ) = WKNAME
         CAGIWK( PICID ) = AGINAM
         CPICNM( PICID ) = CURPIC
         PICACS( PICID ) = LACMOD
         CLEVEL( PICID ) = CNEST
         PTNAME( PICID ) = ' '
         CURPID = PICID

*   If the access mode is 'WRITE' then remember this
         IF ( LACMOD .EQ. 'WRITE' ) THEN

*   Look for an empty slot in the common blocks
            GOTIT = .FALSE.
            I = 1
            DO WHILE ( ( .NOT. GOTIT ) .AND.
     :                 ( I .LE. CLRLEN ) )
               IF ( CLEARF( I ) .EQ. 0 ) THEN
                  GOTIT = .TRUE.
                  CLEARF( I ) = -1
                  CLEARW( I ) = AGINAM
               ELSE
                  I = I + 1
               ENDIF
            ENDDO
         ENDIF
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_OPEN +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

