************************************************************************

      SUBROUTINE AGI_NUPIC ( WX1, WX2, WY1, WY2, PNAME, COMENT,
     :                       NEWX1, NEWX2, NEWY1, NEWY2, PICID, STATUS )

*+
*  Name:
*     AGI_NUPIC
*
*  Purpose:
*     Create a new picture in the database
*
*  Invocation:
*     CALL AGI_NUPIC( WX1, WX2, WY1, WY2, PNAME, COMENT,
*    :                NEWX1, NEWX2, NEWY1, NEWY2, PICID, STATUS )
*
*  Description:
*     Create a new picture in the database. The extent of the new
*     picture is defined in the world coordinate system of the current
*     picture. The world coordinates of this new picture are set to the
*     values passed. The name string has leading blanks removed and is
*     converted to upper case. The new picture is selected as the
*     current one and a picture identifier returned.
*
*  Arguments:
*     WX1 = REAL (Given)
*        Current world coordinate of left edge of picture
*     WX2 = REAL (Given)
*        Current world coordinate of right edge of picture
*     WY1 = REAL (Given)
*        Current world coordinate of bottom edge of picture
*     WY2 = REAL (Given)
*        Current world coordinate of top edge of picture
*     PNAME = CHARACTER*(*) (Given)
*        Name of new picture
*     COMENT = CHARACTER*(*) (Given)
*        Comment for new picture
*     NEWX1 = REAL (Given)
*        World coordinate of left edge of new picture
*     NEWX2 = REAL (Given)
*        World coordinate of right edge of new picture
*     NEWY1 = REAL (Given)
*        World coordinate of bottom edge of new picture
*     NEWY2 = REAL (Given)
*        World coordinate of top edge of new picture
*     PICID = INTEGER (Returned)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get the details of the current picture.
*     Read the contents of the current picture.
*     Check that the new picture lies within the current one.
*     Calculate the device and ndc coordinates of the new picture.
*     Save the new picture in the database.
*     Get a picture identifier and save the details in the common block.
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
*     Aug 1988 (NE):
*        Original version
*     Dec 1989 (NE):
*        Added CIDIID
*     Jun 1990 (NE):
*        Added MEMID parameter
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
      REAL WX1, WX2, WY1, WY2
      CHARACTER * ( * ) PNAME
      CHARACTER * ( * ) COMENT
      REAL NEWX1, NEWX2, NEWY1, NEWY2

*  Arguments Returned :
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local Constants :
      REAL SFA
      PARAMETER ( SFA = 1.0E-32 )

*  Local variables :
      LOGICAL FOUND

      INTEGER CURMID, ISTAT, NEWPIC, PICNUM

      REAL CURDEV( 4 ), CURNDC( 4 ), CURWOR( 4 ), DEVICE( 4 ),
     :     NDC( 4 ), WORLD( 4 ), XFACT1, XFACT2, YFACT3, YFACT4

      CHARACTER * ( AGI__CMAX ) CURCOM
      CHARACTER * ( AGI__SZNAM ) CURNAM
      CHARACTER * ( DAT__SZNAM ) WKNAME
*.

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Get the details of the current picture
         IF ( CURPID .GT. 0 ) THEN
            WKNAME = CAGIWK( CURPID )
            PICNUM = CPICNM( CURPID )
         ELSE
            STATUS = AGI__NOCUP
            CALL ERR_REP( 'AGI_NUPIC_NOCUP',
     :                    'No current picture', STATUS )
            GOTO 99
         ENDIF

*   Read the contents of the current picture
         CALL AGI_1RPIC( WKNAME, PICNUM, CURNAM, CURCOM, CURDEV,
     :                   CURNDC, CURWOR, CURMID, FOUND, STATUS )

*   If picture is not there then indicate an error
         IF ( .NOT. FOUND ) THEN
            STATUS = AGI__PICNF
            CALL ERR_REP( 'AGI_NUPIC_PICNF',
     :                    'Picture not found', STATUS )
            GOTO 99
         ENDIF

*   If the new picture lies outside the current one then flag an error
         IF ( ( WX1 .LT. CURWOR( 1 ) ) .OR.
     :        ( WX2 .GT. CURWOR( 2 ) ) .OR.
     :        ( WY1 .LT. CURWOR( 3 ) ) .OR.
     :        ( WY2 .GT. CURWOR( 4 ) ) ) THEN
            STATUS = AGI__PICOB
            CALL ERR_REP( 'AGI_NUPIC_PICOB',
     :                    'Picture out of bounds', STATUS )
            GOTO 99
         ENDIF

*   Calculate the database coordinates of the new picture.
         IF ( ABS( CURWOR( 2 ) - CURWOR( 1 ) ) .LT. SFA ) THEN
            STATUS = AGI__BADWO
            CALL ERR_REP( 'AGI_NUPIC_BADWX',
     :                    'Bad world coordinates', STATUS )
            GOTO 99
         ENDIF
         IF ( ABS( CURWOR( 4 ) - CURWOR( 3 ) ) .LT. SFA ) THEN
            STATUS = AGI__BADWO
            CALL ERR_REP( 'AGI_NUPIC_BADWY',
     :                    'Bad world coordinates', STATUS )
            GOTO 99
         ENDIF
         XFACT1 = ( WX1 - CURWOR( 1 ) ) / ( CURWOR( 2 ) - CURWOR( 1 ) )
         XFACT2 = ( WX2 - CURWOR( 1 ) ) / ( CURWOR( 2 ) - CURWOR( 1 ) )
         YFACT3 = ( WY1 - CURWOR( 3 ) ) / ( CURWOR( 4 ) - CURWOR( 3 ) )
         YFACT4 = ( WY2 - CURWOR( 3 ) ) / ( CURWOR( 4 ) - CURWOR( 3 ) )
         DEVICE( 1 ) = XFACT1 * CURDEV( 2 ) +
     :                 ( 1.0 - XFACT1 ) * CURDEV( 1 )
         DEVICE( 2 ) = XFACT2 * CURDEV( 2 ) +
     :                 ( 1.0 - XFACT2 ) * CURDEV( 1 )
         DEVICE( 3 ) = YFACT3 * CURDEV( 4 ) +
     :                 ( 1.0 - YFACT3 ) * CURDEV( 3 )
         DEVICE( 4 ) = YFACT4 * CURDEV( 4 ) +
     :                 ( 1.0 - YFACT4 ) * CURDEV( 3 )
         NDC( 1 ) = XFACT1 * CURNDC( 2 ) +
     :              ( 1.0 - XFACT1 ) * CURNDC( 1 )
         NDC( 2 ) = XFACT2 * CURNDC( 2 ) +
     :              ( 1.0 - XFACT2 ) * CURNDC( 1 )
         NDC( 3 ) = YFACT3 * CURNDC( 4 ) +
     :              ( 1.0 - YFACT3 ) * CURNDC( 3 )
         NDC( 4 ) = YFACT4 * CURNDC( 4 ) +
     :              ( 1.0 - YFACT4 ) * CURNDC( 3 )
         WORLD( 1 ) = NEWX1
         WORLD( 2 ) = NEWX2
         WORLD( 3 ) = NEWY1
         WORLD( 4 ) = NEWY2

*   Save the new picture in the database
*   Use the MEMID of the current picture for the new one
         CALL AGI_1PNEW( WKNAME, PNAME, COMENT, DEVICE, NDC, WORLD,
     :                   CURMID, NEWPIC, STATUS )

*   Get a picture identifier
*   Use a local status for the free list access
         ISTAT = SAI__OK

*   Get a picture identifier from the free list
         CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, PICID, ISTAT )

*   If the exit status is not OK then the free list is full
         IF ( ISTAT .NE. SAI__OK ) THEN
            STATUS = AGI__NOPID
            CALL ERR_REP( 'AGI_NUPIC_NOPID',
     :                    'No more picture identifiers', STATUS )
            GOTO 99
         ELSE
            CNUMID = CNUMID + 1
         ENDIF

*   Store the picture details in the common block
*   Copy some of the details from the current picture
         CAGIWK( PICID ) = WKNAME
         CPICNM( PICID ) = NEWPIC
         CGRAWK( PICID ) = CGRAWK( CURPID )
         PICACS( PICID ) = PICACS( CURPID )
         CIDIID( PICID ) = CIDIID( CURPID )
         CLEVEL( PICID ) = CNEST

*   Make this the current picture
         CURPID = PICID

      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGI_NUPIC +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

