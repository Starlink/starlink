************************************************************************

      SUBROUTINE AGP_SVIEW ( PICNAM, COMENT, PICID, STATUS )

*+
*  Name:
*     AGP_SVIEW
*
*  Purpose:
*     Save the current PGPLOT viewport in the database
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
*     Copyright (C) 1988, 1990, 1991, 1992 Science & Engineering Research Council.
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
*     Oct 1988 (NE):
*        Original version
*     Jun 1990 (NE):
*        Argument list in AGI_1GWNAM changed
*        Added MEMID parameter and fudge factors
*     Sep 1990 (NE):
*        Convert device coordinates for hard copy devices
*     Jun 1991 (NE):
*        Remove common block storage for fudge factors
*     Feb 1992 (NE):
*        GKS 7.4 version: New GKS calls and remove fudge factors
*     Jul 1992 (NE):
*        Test status from all GKS inquiry routines.
*        Use DEV/TYPE in PGQINF unless device opened in SGS zone.
*     Oct 1992 (NE):
*        Improve error reports from GKS inquiry rotuines.
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

      INTEGER CURMID, CURPIC, IERR, INUM, ISLASH, IWKID, LSTAT,
     :        NAMLEN, PICNUM

      REAL CURDEV( 4 ), CURNDC( 4 ), CURWOR( 4 ), DEVICE( 4 ),
     :     DUMMY( 4 ), FACTOR, NDC( 4 ), SFA, VDEV( 4 ), WORLD( 4 )

      CHARACTER * ( AGI__CMAX ) CURCOM
      CHARACTER * ( AGI__SZNAM ) CURNAM
      CHARACTER * ( DAT__SZNAM ) CURWKN, WKNAME
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

*   Inquire the current PGPLOT workstation
      IF ( .NOT. CGKSON ) THEN
         CALL PGQINF( 'DEV/TYPE', GNAME, NAMLEN )

*   Remove any /APPEND from the name
         ISLASH = INDEX( GNAME, '/' )
         IF ( ISLASH .GT. 0 ) THEN
            NAMLEN = ISLASH - 1
         ELSE
            NAMLEN = INDEX( GNAME, ' ' ) - 1
         ENDIF

*   If the device was opened in an SGS zone then use the stored name
      ELSE
         CALL SGS_ICURW( IWKID )
         GNAME = CGONAM( IWKID )
         NAMLEN = INDEX( GNAME, ' ' ) - 1
      ENDIF

*   Convert this into an AGI name
      CALL AGI_1GWNAM( GNAME( :NAMLEN ), 'GKS', WKNAME, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Check that the same device is being used
      IF ( CURWKN .NE. WKNAME ) THEN
         STATUS = AGI__DIFDV
         CALL ERR_REP( 'AGP_SVIEW_DIFDV', 'Different devices',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Get the device and world coordinates of the current viewport
      CALL PGQVP( 3, VDEV( 1 ), VDEV( 2 ), VDEV( 3 ), VDEV( 4 ) )
      CALL PGQWIN( WORLD( 1 ), WORLD( 2 ), WORLD( 3 ), WORLD( 4 ) )

*   Get the GKS normalised device coordinates for the current viewport
      CALL GQCNTN( IERR, INUM )
      IF ( IERR .NE. 0 ) THEN
         STATUS = AGI__GKERR
         CALL MSG_SETI( 'ISTAT', IERR )
         CALL ERR_REP( 'AGP_SVIEW_GKERR',
     :                 'Error ^ISTAT returned by GQCNTN', STATUS )
         CALL MSG_SETC( 'NAME', GNAME(:NAMLEN) )
         CALL ERR_REP( 'AGP_SVIEW_GKERR',
     :                 'Unable to query device ^NAME', STATUS )
         GOTO 99
      ENDIF
      CALL GQNT( INUM, IERR, DUMMY, NDC )
      IF ( IERR .NE. 0 ) THEN
         STATUS = AGI__GKERR
         CALL MSG_SETI( 'ISTAT', IERR )
         CALL ERR_REP( 'AGP_SVIEW_GKERR',
     :                 'Error ^ISTAT returned by GQNT', STATUS )
         CALL MSG_SETC( 'NAME', GNAME(:NAMLEN) )
         CALL ERR_REP( 'AGP_SVIEW_GKERR',
     :                 'Unable to query device ^NAME', STATUS )
         GOTO 99
      ENDIF

*   Calculate the GKS device coordinates of the PGPLOT viewport
      FACTOR = NDC( 1 ) / ( NDC( 2 ) - NDC( 1 ) )
      DEVICE( 1 ) = FACTOR * ( VDEV( 2 ) - VDEV( 1 ) )
      FACTOR = NDC( 2 ) / ( NDC( 2 ) - NDC( 1 ) )
      DEVICE( 2 ) = FACTOR * ( VDEV( 2 ) - VDEV( 1 ) )
      FACTOR = NDC( 3 ) / ( NDC( 4 ) - NDC( 3 ) )
      DEVICE( 3 ) = FACTOR * ( VDEV( 4 ) - VDEV( 3 ) )
      FACTOR = NDC( 4 ) / ( NDC( 4 ) - NDC( 3 ) )
      DEVICE( 4 ) = FACTOR * ( VDEV( 4 ) - VDEV( 3 ) )

*   If the current picture number is negative then there must be no
*   pictures on this device so just save the given zone as the first one.
*   If the current picture number is positive then make sure the given
*   zone lies within it.
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
*   Have to allow for fuzzy edges because IDI only works with whole pixels
*   However the device may be a metafile so if all the device coordinates
*   are less than one then assume it is a metafile.
         IF ( ( DEVICE( 1 ) .LT. 1.0 ) .AND.
     :        ( DEVICE( 2 ) .LT. 1.0 ) .AND.
     :        ( DEVICE( 3 ) .LT. 1.0 ) .AND.
     :        ( DEVICE( 4 ) .LT. 1.0 ) ) THEN
            SFA = 0.001
         ELSE
            SFA = 0.999
         ENDIF
         IF ( ( DEVICE( 1 ) .LT. CURDEV( 1 ) - SFA ) .OR.
     :        ( DEVICE( 2 ) .GT. CURDEV( 2 ) + SFA ) .OR.
     :        ( DEVICE( 3 ) .LT. CURDEV( 3 ) - SFA ) .OR.
     :        ( DEVICE( 4 ) .GT. CURDEV( 4 ) + SFA ) ) THEN
            STATUS = AGI__PICOB
            CALL ERR_REP( 'AGP_SVIEW_PICOB', 'Picture out of bounds',
     :                    STATUS )
            GOTO 99
         ENDIF
      ENDIF

*   Save the zone in the database
      CALL AGI_1PNEW( WKNAME, PICNAM, COMENT, DEVICE, NDC, WORLD,
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
      CAGIWK( PICID ) = WKNAME
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

