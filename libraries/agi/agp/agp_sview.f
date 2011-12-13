************************************************************************

      SUBROUTINE AGP_SVIEW ( PICNAM, COMENT, PICID, STATUS )

*+
*  Name:
*     AGP_SVIEW
*
*  Purpose:
*     Save the current PGPLOT viewport in the database
*     (Native PGPLOT version)
*
*  Invocation:
*     CALL AGP_SVIEW( PICNAM, COMENT, PICID, STATUS )
*
*  Description:
*     Save the current PGPLOT viewport as a picture in the database.
*     The new picture must be equal in size or smaller than the current
*     picture in the database. The name of the picture and a comment are
*     used to identify the picture in the database. The name string has
*     leading blanks removed and is converted to upper case. If the
*     picture was successfully created then a valid picture identifier
*     is returned and the new picture becomes the current picture.
*
*  Arguments:
*     PICNAM = CHARACTER*(*) (Given)
*        Name of picture
*     COMENT = CHARACTER*(*) (Given)
*        Description of picture
*     PICID = INTEGER (Returned)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Get the details of the current picture.
*     Check PGPLOT has been activated.
*     Obtain an AGI name for the current PGPLOT workstation.
*     Check the PGPLOT workstation matches that of the current picture.
*     Get the world and ndc coordinates of the current viewport.
*     Calculate the device coordinates of the current viewport.
*     If there is a current picture in the database then
*        Read the contents of the current picture.
*        Check that the viewport lies within the current picture.
*     Endif
*     Save the viewport as a picture in the database.
*     Get a picture identifier and save the details in the common blocks.
*
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*
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
*     NE:  Nick Eaton (Durham University)
*     BKM: Brian McIlwrath (Starlink, RAL)
*     DSB: David Berry (Starlink)
*
*  History:
*     Oct 1988 (NE):
*        Original version
*     Oct 1992 (NE):
*        Final edit of Starlink (GKS based) PGPLOT version
*     Dec 1999 (BKM):
*        New version for native PGPLOT
*     18-JAN-2001 (BKM):
*        Correct device coordinate mismatch with GKS based version.
*     16-NOV-2001 (DSB):
*        Use AGP1_CHKDV to check the device has not changed.
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'GKS_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_idips'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'
      INCLUDE 'ags_wkids'

*  Arguments Given :
      CHARACTER * ( * ) PICNAM
      CHARACTER * ( * ) COMENT

*  Arguments Returned :
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND

      INTEGER CURPIC, CURMID, BASMID, IERR, INUM, ISLASH, IWKID, LSTAT,
     :        NAMLEN, PICNUM

      REAL CURDEV( 4 ), CURNDC( 4 ), CURWOR( 4 ), BASDEV( 4 ),
     :     BASNDC( 4 ), BASWOR( 4),  VNDC( 4 ), FACTOR, NDC( 4 ), SFA,
     :     VDEV( 4 ), WORLD( 4 )

      CHARACTER * ( AGI__CMAX ) CURCOM, BASCOM
      CHARACTER * ( AGI__SZNAM ) CURNAM, BASNAM
      CHARACTER * ( DAT__SZNAM ) CURWKN
      CHARACTER GNAME * 40
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Get the details of the current picture
      IF ( CURPID .GT. 0 ) THEN
         CURWKN = CAGIWK( CURPID )
         CURPIC = CPICNM( CURPID )
      ELSE
         STATUS = AGI__NOCUP
         CALL ERR_REP( 'AGP_SVIEW_NOCUP', 'No current picture',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Check that PGPLOT has started
      IF ( .NOT. CPGPON ) THEN
         STATUS = AGI__GRPNA
         CALL ERR_REP( 'AGP_SVIEW_GRPNA', 'Graphics package not active',
     :                 STATUS )
         GOTO 99
      ENDIF

*  Check that the current device is the one associated with the current
*  picture.
      CALL AGP1_CHKDV( CURWKN, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Get the device, ndc and world coordinates of the current viewport
      CALL PGQVP( 0, VNDC( 1 ), VNDC( 2 ), VNDC( 3 ), VNDC( 4 ) )
      CALL PGQVP( 3, VDEV( 1 ), VDEV( 2 ), VDEV( 3 ), VDEV( 4 ) )
      CALL PGQWIN( WORLD( 1 ), WORLD( 2 ), WORLD( 3 ), WORLD( 4 ) )

*   Get the coordinates of the base picture for this device
      CALL AGI_1RPIC( CURWKN, 1, BASNAM, BASCOM, BASDEV,
     :                BASNDC, BASWOR, BASMID, FOUND, STATUS )
      IF( (.NOT. FOUND) .OR. ( BASNAM .NE. 'BASE' ) ) THEN
         STATUS = AGI__PICNF
         CALL ERR_REP( 'AGP_SVIEW_PICNF',
     :                  'Data on base picture not found',
     :                  STATUS )
         GOTO 99
      ENDIF

*   Convert the PGPLOT NDCs to GKS equivalents for storage in the
*   AGI database
      IF( BASNDC(2) .NE. 1.0 ) THEN
         VNDC( 1 ) = VNDC( 1 ) * BASNDC( 2 )
         VNDC( 2 ) = VNDC( 2 ) * BASNDC( 2 )
      ENDIF
      IF( BASNDC(4) .NE. 1.0 ) THEN
         VNDC( 3 ) = VNDC( 3 ) * BASNDC( 4 )
         VNDC( 4 ) = VNDC( 4 ) * BASNDC( 4 )
      ENDIF

*  Also convert the native PGPLOT device coordinates. These are a fraction
*  of a pixel different as, for example, with a 768x512 X Window-native
*  PGPLOT returns the maximum X,Y coordinate of a viewport as (768,512)
*  whereas GKS-based Starlink PGPLOT as (767,511). Note that the BASE
*  picture is the same (768,512) in both systems as this is obtained
*  directly from GKS in Starlink PGPLOT.

      VDEV( 1 ) = (VDEV( 1 ) * (BASDEV( 2 ) - 1))/ BASDEV( 2 )
      VDEV( 2 ) = (VDEV( 2 ) * (BASDEV( 2 ) - 1))/ BASDEV( 2 )
      VDEV( 3 ) = (VDEV( 3 ) * (BASDEV( 4 ) - 1))/ BASDEV( 4 )
      VDEV( 4 ) = (VDEV( 4 ) * (BASDEV( 4 ) - 1))/ BASDEV( 4 )

*   If the current picture number is negative then there must be no
*   pictures on this device so just save the given picture as the first one.
*   If the current picture number is positive then make sure the current
*   one lies within it.
      IF ( CURPIC .GT. 0 ) THEN

*   Read the contents of the current picture
         CALL AGI_1RPIC( CURWKN, CURPIC, CURNAM, CURCOM, CURDEV,
     :                   CURNDC, CURWOR, CURMID, FOUND, STATUS )

*   If picture is not there then indicate an error
         IF ( .NOT. FOUND ) THEN
            STATUS = AGI__PICNF
            CALL ERR_REP( 'AGP_SVIEW_PICNF', 'Picture not found',
     :                    STATUS )
            GOTO 99
         ENDIF


*   Check that the given viewport lies within the current picture
*   Have to allow for fuzzy edges because IDI only works with whole
*   pixels.
         SFA = 0.999
         IF ( ( VDEV( 1 ) .LT. CURDEV( 1 ) - SFA ) .OR.
     :        ( VDEV( 2 ) .GT. CURDEV( 2 ) + SFA ) .OR.
     :        ( VDEV( 3 ) .LT. CURDEV( 3 ) - SFA ) .OR.
     :        ( VDEV( 4 ) .GT. CURDEV( 4 ) + SFA ) ) THEN
            STATUS = AGI__PICOB
            CALL ERR_REP( 'AGP_SVIEW_PICOB', 'Picture out of bounds',
     :                    STATUS )
            GOTO 99
         ENDIF
      ENDIF

*   Save the zone in the database
      CALL AGI_1PNEW( CURWKN, PICNAM, COMENT, VDEV, VNDC, WORLD,
     :                CURMID, PICNUM, STATUS )

*   Get a picture identifier
*   Use a local status for the free list access
      LSTAT = SAI__OK
      CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, PICID, LSTAT )

*   If the status is not OK then the free list is full
      IF ( LSTAT .NE. SAI__OK ) THEN
         STATUS = AGI__NOPID
         CALL ERR_REP( 'AGP_SVIEW_NOPID',
     :                 'No more picture identifiers', STATUS )
         GOTO 99
      ELSE
         CNUMID = CNUMID + 1
      ENDIF

*   Store the details in the common block
      CAGIWK( PICID ) = CURWKN
      CPICNM( PICID ) = PICNUM
      CGRAWK( PICID ) = CGRAWK( CURPID )
      PICACS( PICID ) = PICACS( CURPID )
      CLEVEL( PICID ) = CNEST

*   Make this the current picture
      CURPID = PICID

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGP_SVIEW +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

