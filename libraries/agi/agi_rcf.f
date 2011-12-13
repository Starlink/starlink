************************************************************************

      SUBROUTINE AGI_RCF ( PNAME, PICID, STATUS )

*+
*  Name:
*     AGI_RCF
*
*  Purpose:
*     Recall first picture of specified name
*
*  Invocation:
*     CALL AGI_RCF( PNAME, PICID, STATUS )
*
*  Description:
*     Recall the first picture on the current device that has the
*     specified name and lies within the bounds of the current picture.
*     The name string has leading blanks removed and is converted to
*     upper case before being compared. An empty name string (just
*     spaces) results in a search for a picture of any name. This
*     picture becomes the current picture. If no picture fulfills the
*     conditions an error is returned.
*
*  Arguments:
*     PNAME = CHARACTER*(*) (Given)
*        Name of picture
*     PICID = INTEGER (Returned)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get details of the current picture.
*     Read the contents of the current picture.
*     Find a picture of the given name searching forwards.
*     If the current picture is not the root picture then
*        Repeat while the recovered picture is not inside the current one
*           Look for another picture of the same name.
*        Endwhile
*     Endif
*     Get a new picture identifier and store the details in the
*     common block.
*
*  Copyright:
*     Copyright (C) 1988, 1989, 1990 Science & Engineering Research Council.
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
*     Jul 1988 (NE):
*        Original version
*     Dec 1989 (NE):
*        Added CIDIID
*     Jun 1990 (NE):
*        Added MEMID parameter
*     Jul 1990 (NE):
*        Added root picture
*     Sep 1990 (NE):
*        Added fuzzy test
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_locs'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      CHARACTER * ( * ) PNAME

*  Arguments Returned :
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND

      INTEGER CURMID, ISTAT, MEMID, PICNUM

      REAL CURDEV( 4 ), CURNDC( 4 ), CURWOR( 4 ), SFA
      REAL DEVICE( 4 ), NDC( 4 ), WORLD( 4 )

      CHARACTER * ( AGI__CMAX ) CURCOM, COMENT
      CHARACTER * ( AGI__SZNAM ) CURNAM, OPNAME
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   Get details of the current picture
         IF ( CURPID .GT. 0 ) THEN
            WKNAME = CAGIWK( CURPID )
            PICNUM = CPICNM( CURPID )
         ELSE
            STATUS = AGI__NOCUP
            CALL ERR_REP( 'AGI_RCF_NOCUP',
     :                    'No current picture', STATUS )
            GOTO 99
         ENDIF

*   Read the contents of the current picture
         CALL AGI_1RPIC( WKNAME, PICNUM, CURNAM, CURCOM, CURDEV,
     :                   CURNDC, CURWOR, CURMID, FOUND, STATUS )

*   If picture is not there then indicate an error
         IF ( .NOT. FOUND ) THEN
            STATUS = AGI__PICNF
            CALL ERR_REP( 'AGI_RCF_PICNF', 'Picture not found', STATUS )
            GOTO 99
         ENDIF

*   Have to allow for fuzzy edges for the search because IDI only works
*   with whole pixels. However the device may be a metafile so if all
*   the device coordinates are less than one then assume it is a metafile.
         IF ( ( CURDEV( 1 ) .LT. 1.0 ) .AND.
     :        ( CURDEV( 2 ) .LT. 1.0 ) .AND.
     :        ( CURDEV( 3 ) .LT. 1.0 ) .AND.
     :        ( CURDEV( 4 ) .LT. 1.0 ) ) THEN
            SFA = 0.001
         ELSE
            SFA = 0.999
         ENDIF

*   Find a picture of that name searching forwards
         CALL AGI_1SEARP( WKNAME, PNAME, 'F', 'F', PICNUM, OPNAME,
     :                    COMENT, DEVICE, NDC, WORLD, MEMID, STATUS )

*   If the root flag is set then ignore the current picture limits
         IF ( CROOT .NE. 1 ) THEN

*   Check that the picture is inside the current one, otherwise look
*   for another one.
            DO WHILE( ( ( DEVICE( 1 ) .LT. CURDEV( 1 ) - SFA ) .OR.
     :                  ( DEVICE( 2 ) .GT. CURDEV( 2 ) + SFA ) .OR.
     :                  ( DEVICE( 3 ) .LT. CURDEV( 3 ) - SFA ) .OR.
     :                  ( DEVICE( 4 ) .GT. CURDEV( 4 ) + SFA ) ) .AND.
     :                ( STATUS .EQ. SAI__OK ) )
               PICNUM = PICNUM + 1
               CALL AGI_1SEARP( WKNAME, PNAME, 'P', 'F', PICNUM, OPNAME,
     :                          COMENT, DEVICE, NDC, WORLD, MEMID,
     :                          STATUS )
            ENDDO
         ENDIF
         IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Get a picture identifier
*   Use a local status for the free list access
         ISTAT = SAI__OK

*   Get a picture identifier from the free list
         CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, PICID, ISTAT )

*   If the exit status is not OK then the free list is full
         IF ( ISTAT .NE. SAI__OK ) THEN
            STATUS = AGI__NOPID
            CALL ERR_REP( 'AGI_RCF_NOPID',
     :                    'No more picture identifiers', STATUS )
            GOTO 99
         ELSE
            CNUMID = CNUMID + 1
         ENDIF

*   Store the picture details in the common block.
*   Copy some of the details from the current picture.
         CAGIWK( PICID ) = WKNAME
         CPICNM( PICID ) = PICNUM
         CGRAWK( PICID ) = CGRAWK( CURPID )
         PICACS( PICID ) = PICACS( CURPID )
         CIDIID( PICID ) = CIDIID( CURPID )
         CLEVEL( PICID ) = CNEST

*   Make this the current picture
         CURPID = PICID

*   Remember this in the database
         CALL AGI_1WPACT( WKNAME, PICNUM, STATUS )

*   Reset the root flag
         CROOT = 0

      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_RCF +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

