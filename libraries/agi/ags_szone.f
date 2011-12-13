************************************************************************

      SUBROUTINE AGS_SZONE ( PNAME, COMENT, PICID, STATUS )

*+
*  Name:
*     AGS_SZONE
*
*  Purpose:
*     Save the current SGS zone in the database
*
*  Invocation:
*     CALL AGS_SZONE( PNAME, COMENT, PICID, STATUS )
*
*  Description:
*     Save the current SGS zone as a picture in the database. The new
*     picture must be equal in size or smaller than the current picture
*     in the database. The name of the picture and a comment are used to
*     identify the picture in the database. The name string has leading
*     blanks removed and is converted to upper case. If the picture was
*     successfully created then a valid picture identifier is returned
*     and the new picture becomes the current picture.
*
*  Arguments:
*     PNAME = CHARACTER*(*) (Given)
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
*     Check SGS has been activated.
*     Obtain an AGI name for the current SGS workstation.
*     Get the world and ndc coordinates for the current zone.
*     Calculate the device coordinates of the current zone.
*     Get the details of the current picture.
*     Check that the SGS workstation matches that of the current picture.
*     If there is a current picture in the database then
*        Read the contents of the current picture.
*        Check that the zone lies within the current picture.
*     Endif
*     Save the zone as a picture in the database.
*     Get a picture identifier and save the details in the common block.
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
*     Jul 1988 (NE):
*        Original version
*     Jun 1989 (NE):
*        Changed how the limits in device coords are calculated
*     Dec 1989 (NE):
*        Added fuzzy edges when testing for picture overlap
*     Jun 1990 (NE):
*        Argument list changed in AGI_1GWNAM
*        Added MEMID parameter and coordinate fudge factor
*     Jul 1990 (NE):
*        Changed bounds check to NDC's
*     Dec 1990 (NE):
*        Use raster units for device coordiantes
*     Jun 1991 (NE):
*        Remove common block storage for fudge factors
*     Feb 1992 (NE):
*        GKS 7.4 version: New GKS calls and remove fudge factors
*     Jul 1992 (NE):
*        Test status from all GKS inquiry routines.
*        Get current SGS device name from common block.
*     Oct 1992 (NE):
*        Improve error reports from GKS inquiry routines.
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'
      INCLUDE 'GKS_PAR'

*  Global variables :
      INCLUDE 'agi_idips'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_nest'
      INCLUDE 'agi_pfree'
      INCLUDE 'ags_wkids'

*  Arguments Given :
      CHARACTER * ( * ) PNAME
      CHARACTER * ( * ) COMENT

*  Arguments Returned :
      INTEGER PICID

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND

      INTEGER CURMID, CURPIC, ICON, IERR, INUM, ISTAT, ITUS, IWKID,
     :        IWTYPE, JUNIT, LNAME, LX, LY, PICNUM

      REAL CURDEV( 4 ), CURNDC( 4 ), CURWOR( 4 ), DEVICE( 4 ), DX, DY,
     :     FACTOR, NDC( 4 ), RQDEV( 4 ), RQNDC( 4 ), SFA, WDEV( 4 ),
     :     WORLD( 4 ), WNDC( 4 )

      CHARACTER * ( AGI__CMAX ) CURCOM
      CHARACTER * ( AGI__SZNAM ) CURNAM
      CHARACTER * ( DAT__SZNAM ) CURWKN, WKNAME
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Check that SGS has started
      IF ( .NOT. CGKSON ) THEN
         STATUS = AGI__GRPNA
         CALL ERR_REP( 'AGS_SZONE_GRPNA', 'Graphics package not active',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Get the workstation and connection identifiers
      CALL SGS_ICURW( IWKID )
      CALL GQWKC( IWKID, IERR, ICON, IWTYPE )

*   Check that there have not been any errors from GKS
      IF ( IERR .NE. 0 ) THEN
         STATUS = AGI__GKERR
         CALL MSG_SETI( 'ISTAT', IERR )
         CALL ERR_REP( 'AGS_SZONE_GKERR',
     :                 'Error ^ISTAT returned by GQWKC', STATUS )
         LNAME = INDEX( CGONAM( IWKID ), ' ' ) - 1
         CALL MSG_SETC( 'NAME', CGONAM( IWKID )(:LNAME) )
         CALL ERR_REP( 'AGS_SZONE_GKERR',
     :                 'Unable to query device ^NAME', STATUS )
         GOTO 99
      ENDIF

*   Get an AGI name from the name used to open the device
      CALL GNS_IANG( CGONAM( IWKID ), WKNAME, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Get current the world, ndc and device coordinates
*   First get the workstation transformation information
      CALL GQWKT( IWKID, IERR, ITUS, RQNDC, WNDC, RQDEV, WDEV )
      IF ( IERR .NE. 0 ) THEN
         STATUS = AGI__GKERR
         CALL MSG_SETI( 'ISTAT', IERR )
         CALL ERR_REP( 'AGS_SZONE_GKERR',
     :                 'Error ^ISTAT returned by GQWKT', STATUS )
         LNAME = INDEX( CGONAM( IWKID ), ' ' ) - 1
         CALL MSG_SETC( 'NAME', CGONAM( IWKID )(:LNAME) )
         CALL ERR_REP( 'AGS_SZONE_GKERR',
     :                 'Unable to query device ^NAME', STATUS )
         GOTO 99
      ENDIF

*   Get the display surface size
      CALL GQDSP( IWTYPE, IERR, JUNIT, DX, DY, LX, LY )
      IF ( IERR .NE. 0 ) THEN
         STATUS = AGI__GKERR
         CALL MSG_SETI( 'ISTAT', IERR )
         CALL ERR_REP( 'AGS_SZONE_GKERR',
     :                 'Error ^ISTAT returned by GQDSP', STATUS )
         LNAME = INDEX( CGONAM( IWKID ), ' ' ) - 1
         CALL MSG_SETC( 'NAME', CGONAM( IWKID )(:LNAME) )
         CALL ERR_REP( 'AGS_SZONE_GKERR',
     :                 'Unable to query device ^NAME', STATUS )
         GOTO 99
      ENDIF

*   Convert the workstation transformation to raster units
      IF ( JUNIT .EQ. GMETRE ) THEN
         WDEV( 1 ) = WDEV( 1 ) * ( REAL( LX ) / DX )
         WDEV( 2 ) = WDEV( 2 ) * ( REAL( LX ) / DX )
         WDEV( 3 ) = WDEV( 3 ) * ( REAL( LY ) / DY )
         WDEV( 4 ) = WDEV( 4 ) * ( REAL( LY ) / DY )
      ENDIF

*   Get the world and ndc coordinates from the current transformation
      CALL GQCNTN( IERR, INUM )
      IF ( IERR .NE. 0 ) THEN
         STATUS = AGI__GKERR
         CALL MSG_SETI( 'ISTAT', IERR )
         CALL ERR_REP( 'AGS_SZONE_GKERR',
     :                 'Error ^ISTAT returned by GQCNTN', STATUS )
         LNAME = INDEX( CGONAM( IWKID ), ' ' ) - 1
         CALL MSG_SETC( 'NAME', CGONAM( IWKID )(:LNAME) )
         CALL ERR_REP( 'AGS_SZONE_GKERR',
     :                 'Unable to query device ^NAME', STATUS )
         GOTO 99
      ENDIF
      CALL GQNT( INUM, IERR, WORLD, NDC )
      IF ( IERR .NE. 0 ) THEN
         STATUS = AGI__GKERR
         CALL MSG_SETI( 'ISTAT', IERR )
         CALL ERR_REP( 'AGS_SZONE_GKERR',
     :                 'Error ^ISTAT returned by GQNT', STATUS )
         LNAME = INDEX( CGONAM( IWKID ), ' ' ) - 1
         CALL MSG_SETC( 'NAME', CGONAM( IWKID )(:LNAME) )
         CALL ERR_REP( 'AGS_SZONE_GKERR',
     :                 'Unable to query device ^NAME', STATUS )
         GOTO 99
      ENDIF

*   Then calculate the device coordinates from the ndc
      FACTOR = ( NDC( 1 ) - WNDC( 1 ) ) / ( WNDC( 2 ) - WNDC( 1 ) )
      DEVICE( 1 ) = FACTOR * WDEV( 2 ) + ( 1.0 - FACTOR ) * WDEV( 1 )
      FACTOR = ( NDC( 2 ) - WNDC( 1 ) ) / ( WNDC( 2 ) - WNDC( 1 ) )
      DEVICE( 2 ) = FACTOR * WDEV( 2 ) + ( 1.0 - FACTOR ) * WDEV( 1 )
      FACTOR = ( NDC( 3 ) - WNDC( 3 ) ) / ( WNDC( 4 ) - WNDC( 3 ) )
      DEVICE( 3 ) = FACTOR * WDEV( 4 ) + ( 1.0 - FACTOR ) * WDEV( 3 )
      FACTOR = ( NDC( 4 ) - WNDC( 3 ) ) / ( WNDC( 4 ) - WNDC( 3 ) )
      DEVICE( 4 ) = FACTOR * WDEV( 4 ) + ( 1.0 - FACTOR ) * WDEV( 3 )

*   Get the details of the current picture
      IF ( CURPID .GT. 0 ) THEN
         CURWKN = CAGIWK( CURPID )
         CURPIC = CPICNM( CURPID )
      ELSE
         STATUS = AGI__NOCUP
         CALL ERR_REP( 'AGS_SZONE_NOCUP', 'No current picture',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Check that the same device is being used
      IF ( CURWKN .NE. WKNAME ) THEN
         STATUS = AGI__DIFDV
         CALL ERR_REP( 'AGS_SZONE_DIFDV', 'Different devices',
     :                 STATUS )
         GOTO 99
      ENDIF

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
            CALL ERR_REP( 'AGS_SZONE_PICNF', 'Picture not found',
     :                    STATUS )
            GOTO 99
         ENDIF

*   Check that the given zone lies within the current picture
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
            CALL ERR_REP( 'AGS_SZONE_PICOB', 'Picture out of bounds',
     :                    STATUS )
            GOTO 99
         ENDIF
      ENDIF

*   Save the zone in the database
      CALL AGI_1PNEW( WKNAME, PNAME, COMENT, DEVICE, NDC, WORLD,
     :                CURMID, PICNUM, STATUS )

*   Get a picture identifier
*   Use a local status for the free list access
      ISTAT = SAI__OK

*   Get a picture identifier from the free list
      CALL AGI_1FNEXT( FRELEN, FRELIS, NEXFRE, PICID, ISTAT )

*   If the exit status is not OK then the free list is full
      IF ( ISTAT .NE. SAI__OK ) THEN
         STATUS = AGI__NOPID
         CALL ERR_REP( 'AGS_SZONE_NOPID',
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

*      print*, '+++++ AGS_SZONE +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

