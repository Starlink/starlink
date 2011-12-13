************************************************************************

      SUBROUTINE AGS_NZONE ( NEWZON, STATUS )

*+
*  Name:
*     AGS_NZONE
*
*  Purpose:
*     Create a new SGS zone from the current picture
*
*  Invocation:
*     CALL AGS_NZONE ( NEWZON, STATUS )
*
*  Description:
*     Create a new SGS zone from the current picture. The zone will be
*     created with the coordinate system of the current picture.
*     If the device associated with the current picture is not already
*     open then this routine will open it, and additionally the zone
*     will be cleared if the access mode is 'WRITE' (in AGI_ASSOC or
*     AGI_OPEN).
*
*  Arguments:
*     NEWZON = INTEGER (Returned)
*        SGS zone identifier of new zone
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Check SGS has been activated.
*     Get the details of the current picture.
*     Get the GKS workstation identifiers for the current picture.
*     If SGS is open then
*        Get the GKS workstation identifiers for the open device.
*        Check these against the workstation identifiers for the current
*        picture.
*     Endif
*     If SGS is not open then
*        Set the no-clear flag.
*        Open SGS for this device.
*     Endif
*     Read the contents of the current picture.
*     Check the new zone will fit inside the current picture.
*     Calculate the world coordinates of the new zone.
*     Create the new SGS zone.
*     If the current picture has 'WRITE' access then
*        Clear the zone.
*     Endif
*
*  Implementation Deficiencies:
*     (1) This assumes that if there is an open GKS workstation then SGS
*     has been opened.
*     (2) SGS_OPNWK is used to switch between workstations if the
*     workstation defined by the current picture is not the current
*     SGS workstation. This means that a for a workstation that is
*     already open, but not current, a workstation identifier and
*     base zone will be created and the picture zone will be
*     created inside this, and not inside the zone that was the current
*     one the last time the workstation was used.
*     (3) The base zones are stored in an array indexed by the GKS
*     workstation id returned from SGS_ICURW.
*
*  Copyright:
*     Copyright (C) 1988, 1989, 1990, 1991, 1992 Science & Engineering
*     Research Council.  All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     NE: Nick Eaton (Durham University)
*     MJC: Malcolm J. Currie (Starlink)
*
*  History:
*     Aug 1988 (NE):
*        Original version
*     May 1989 (NE):
*        Changed how the test for an open workstation is done.
*     Jun 1989 (NE):
*        Changed how the limits of the new zone are calculated.
*     Aug 1989 (NE):
*        Use SGS_OPNWK to switch between workstations
*     Jun 1990 (NE):
*        Added call to GNS_IGAG, added MEMID parameter
*        and added device coordinate fudge factors
*     Sep 1990 (NE):
*        Save the base zone identifier
*     Nov 1990 (NE):
*        Added clear flag
*     Jun 1991 (NE):
*        Added an array of base zones to fix bug when switching
*        between devices.
*        Remove common block storage for fudge factors.
*     Feb 1992 (NE):
*        GKS 7.4 version: New GKS calls and remove fudge factors
*     Jul 1992 (NE):
*        Test status from all GKS inquiry routines.
*        Save device name used to open device.
*     Oct 1992 (NE):
*        Improve error reports from GKS inquiry routines.
*     2007 May 7 (MJC):
*        Doubled and parameterised the fuzzy-edge tolerance.
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
      INCLUDE 'agi_pfree'
      INCLUDE 'ags_wkids'

*  Arguments Returned :
      INTEGER NEWZON

*  Status :
      INTEGER STATUS

*  Local Constants:
      REAL ROUND
      PARAMETER ( ROUND = 2.0E-3 )

*  Local variables :
      LOGICAL FOUND, GOTIT, NOTOPN, USEGKS

      INTEGER CLRNUM, CONID, CURMID, I, ICONID, IERR, INUM, ISTAT,
     :        IWKID, IWKTY, JOPSTA, LNAME, PICNUM, STRLEN, TZONE, WKTY

      CHARACTER * ( AGI__CMAX ) CURCOM
      CHARACTER * ( AGI__SZNAM ) CURNAM
      CHARACTER * ( DAT__SZNAM ) WKNAME
      CHARACTER DEVNAM * 64, SGSNAM * 64

      REAL CURDEV( 4 ), CURNDC( 4 ), CURWOR( 4 ), FACTOR, SFA
      REAL WZONE( 4 ), ZCURN( 4 ), ZCURW( 4 )
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Check that SGS has started
      IF ( .NOT. CGKSON ) THEN
         STATUS = AGI__GRPNA
         CALL ERR_REP( 'AGS_NZONE_GRPNA', 'Graphics package not active',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Get the details of the current picture
      IF ( CURPID .GT. 0 ) THEN
         WKNAME = CAGIWK( CURPID )
         PICNUM = CPICNM( CURPID )
      ELSE
         STATUS = AGI__NOCUP
         CALL ERR_REP( 'AGS_NZONE_NOCUP', 'No current picture', STATUS )
         GOTO 99
      ENDIF

*   Get the type and connection id for this workstation
      ISTAT = SAI__OK
      USEGKS = .TRUE.
      CALL ERR_MARK
      CALL SGS_WIDEN( CGRAWK( CURPID ), WKTY, CONID, ISTAT )

*   If the translation failed it may be because the workstation name
*   is an IDI name, so try using the AGI name to get a device name
      IF ( ISTAT .NE. SAI__OK ) THEN
         USEGKS = .FALSE.
         CALL ERR_ANNUL( ISTAT )
         CALL GNS_IGAG( CAGIWK( CURPID ), DEVNAM, ISTAT )
         CALL SGS_WIDEN( DEVNAM, WKTY, CONID, ISTAT )

*   If it has still failed then give up
         IF ( ISTAT .NE. SAI__OK ) GOTO 99
      ENDIF
      CALL ERR_RLSE

*   Set the SGS closed flag to .TRUE.
      NOTOPN = .TRUE.

*   Find out if SGS is already open.
*   Since there is no proper way of doing this then will have to make
*   some assumptions.
*   Inquire the operating state of GKS
      CALL GQOPS( JOPSTA )

*   If at least one workstation open then assume SGS is open
      IF ( ( JOPSTA .EQ. GWSOP ) .OR. ( JOPSTA .EQ. GWSAC ) .OR.
     :     ( JOPSTA .EQ. GSGOP ) ) THEN

*   Find out the current workstation identifier
         CALL SGS_ICURW( IWKID )

*   Assume that if IWKID is non-zero then SGS is open
         IF ( IWKID .NE. 0 ) THEN

*   Inquire the type and connection id of the current workstation
            CALL GQWKC( IWKID, IERR, ICONID, IWKTY )

*   Check that there have not been any errors from GKS
            IF ( IERR .NE. 0 ) THEN
               STATUS = AGI__GKERR
               CALL MSG_SETI( 'ISTAT', IERR )
               CALL ERR_REP( 'AGS_NZONE_GKERR',
     :                       'Error ^ISTAT returned by GQWKC', STATUS )
               LNAME = INDEX( CGRAWK( CURPID ), ' ' ) - 1
               CALL MSG_SETC( 'NAME', CGRAWK( CURPID )(:LNAME) )
               CALL ERR_REP( 'AGS_NZONE_GKERR',
     :                       'Unable to query device ^NAME', STATUS )
               GOTO 99
            ENDIF

*   See if the current SGS workstation is the same as the one
*   associated with the current picture
            IF ( ( IWKTY .EQ. WKTY ) .AND. ( ICONID .EQ. CONID ) ) THEN
                 NOTOPN = .FALSE.
            ENDIF
         ENDIF
      ENDIF

*   If the workstation associated with the current picture is not
*   the current SGS workstation the use SGS_OPNWK to switch devices
      IF ( NOTOPN ) THEN

*   Make up a workstation name
         CALL AGS_1GKSNM( WKTY, CONID, SGSNAM, STRLEN, STATUS )

*   Make sure the workstation is not cleared
         CALL SGS_CLRFG( 1 )

*   Open this workstation
         CALL SGS_OPNWK( SGSNAM, TZONE, STATUS )

*   Store the base zone in the array
         CALL SGS_ICURW( IWKID )
         CBZONE( IWKID ) = TZONE

*   Store the name used to open the device in the array
         IF ( USEGKS ) THEN
            CGONAM( IWKID ) = CGRAWK( CURPID )
         ELSE
            CGONAM( IWKID ) = DEVNAM
         ENDIF

*   Check that no problems have occured
         IF ( STATUS .NE. SAI__OK ) GOTO 99
      ENDIF

*   If PICNUM is greater than 0 then get the coordinates for this
*   picture and create a corresponding new zone. Otherwise there are
*   no pictures and so just pass back the current zone.
      IF ( PICNUM .GT. 0 ) THEN

*   Read the contents of the picture
         CALL AGI_1RPIC( WKNAME, PICNUM, CURNAM, CURCOM, CURDEV,
     :                   CURNDC, CURWOR, CURMID, FOUND, STATUS )

*   If picture is not there then indicate an error
         IF ( .NOT. FOUND ) THEN
            STATUS = AGI__PICNF
            CALL ERR_REP( 'AGS_NZONE_PICNF', 'Picture not found',
     :                    STATUS )
            GOTO 99
         ENDIF

*   Select the base zone to get the largest possible area. Note if this
*   fails then the current zone will be used.
         CALL SGS_SELZ( CBZONE( IWKID ), STATUS )

*   Get the coordinates of the current SGS zone
         CALL GQCNTN( IERR, INUM )
         IF ( IERR .NE. 0 ) THEN
            STATUS = AGI__GKERR
            CALL MSG_SETI( 'ISTAT', IERR )
            CALL ERR_REP( 'AGS_NZONE_GKERR',
     :                    'Error ^ISTAT returned by GQCNTN', STATUS )
            LNAME = INDEX( CGRAWK( CURPID ), ' ' ) - 1
            CALL MSG_SETC( 'NAME', CGRAWK( CURPID )(:LNAME) )
            CALL ERR_REP( 'AGS_NZONE_GKERR',
     :                    'Unable to query device ^NAME', STATUS )
            GOTO 99
         ENDIF
         CALL GQNT( INUM, IERR, ZCURW, ZCURN )
         IF ( IERR .NE. 0 ) THEN
            STATUS = AGI__GKERR
            CALL MSG_SETI( 'ISTAT', IERR )
            CALL ERR_REP( 'AGS_NZONE_GKERR',
     :                    'Error ^ISTAT returned by GQNT', STATUS )
            LNAME = INDEX( CGRAWK( CURPID ), ' ' ) - 1
            CALL MSG_SETC( 'NAME', CGRAWK( CURPID )(:LNAME) )
            CALL ERR_REP( 'AGS_NZONE_GKERR',
     :                    'Unable to query device ^NAME', STATUS )
            GOTO 99
         ENDIF

*   If the ndc's of the database are larger than the current zone then
*   cannot create a new zone.
*   Have to use fuzzy edges to allow for any rounding errors in GKS/SGS
         SFA = MAX( ABS( ZCURN( 1 ) * ROUND ),
     :              ABS( ZCURN( 2 ) * ROUND ),
     :              ABS( ZCURN( 3 ) * ROUND ),
     :              ABS( ZCURN( 4 ) * ROUND ) )
         IF ( ( CURNDC( 1 ) .LT. ZCURN( 1 ) - SFA ) .OR.
     :        ( CURNDC( 2 ) .GT. ZCURN( 2 ) + SFA ) .OR.
     :        ( CURNDC( 3 ) .LT. ZCURN( 3 ) - SFA ) .OR.
     :        ( CURNDC( 4 ) .GT. ZCURN( 4 ) + SFA ) ) THEN
            STATUS = AGI__ZONTS
            CALL ERR_REP( 'AGS_NZONE_ZONTS', 'SGS zone too small',
     :                    STATUS )
            GOTO 99
         ELSE

*   The new zone has to be expressed in terms of the world coordinates
*   of the current zone, so calculate the transformations.
            FACTOR = ( CURNDC( 1 ) - ZCURN( 1 ) ) /
     :               ( ZCURN( 2 ) - ZCURN( 1 ) )
            WZONE( 1 ) = FACTOR * ZCURW( 2 ) +
     :                   ( 1.0 - FACTOR ) * ZCURW( 1 )
            FACTOR = ( CURNDC( 2 ) - ZCURN( 1 ) ) /
     :               ( ZCURN( 2 ) - ZCURN( 1 ) )
            WZONE( 2 ) = FACTOR * ZCURW( 2 ) +
     :                   ( 1.0 - FACTOR ) * ZCURW( 1 )
            FACTOR = ( CURNDC( 3 ) - ZCURN( 3 ) ) /
     :               ( ZCURN( 4 ) - ZCURN( 3 ) )
            WZONE( 3 ) = FACTOR * ZCURW( 4 ) +
     :                   ( 1.0 - FACTOR ) * ZCURW( 3 )
            FACTOR = ( CURNDC( 4 ) - ZCURN( 3 ) ) /
     :               ( ZCURN( 4 ) - ZCURN( 3 ) )
            WZONE( 4 ) = FACTOR * ZCURW( 4 ) +
     :                   ( 1.0 - FACTOR ) * ZCURW( 3 )

*   Allow for a bit of rounding error to ensure new zone does not
*   lie outside current zone.
            SFA = MAX( ABS( ZCURW( 1 ) * ROUND ),
     :                 ABS( ZCURW( 2 ) * ROUND ) )
            IF ( ( WZONE( 1 ) .LT. ZCURW( 1 ) ) .AND.
     :           ( WZONE( 1 ) .GT. ZCURW( 1 ) - SFA ) ) THEN
               WZONE( 1 ) = ZCURW( 1 ) + SFA
            ENDIF
            IF ( ( WZONE( 2 ) .GT. ZCURW( 2 ) ) .AND.
     :           ( WZONE( 2 ) .LT. ZCURW( 2 ) + SFA ) ) THEN
               WZONE( 2 ) = ZCURW( 2 ) - SFA
            ENDIF

            SFA = MAX( ABS( ZCURW( 3 ) * ROUND ),
     :                 ABS( ZCURW( 4 ) * ROUND ) )
            IF ( ( WZONE( 3 ) .LT. ZCURW( 3 ) ) .AND.
     :           ( WZONE( 3 ) .GT. ZCURW( 3 ) - SFA ) ) THEN
               WZONE( 3 ) = ZCURW( 3 ) + SFA
            ENDIF
            IF ( ( WZONE( 4 ) .GT. ZCURW( 4 ) ) .AND.
     :           ( WZONE( 4 ) .LT. ZCURW( 4 ) + SFA ) ) THEN
               WZONE( 4 ) = ZCURW( 4 ) - SFA
            ENDIF

*   Create the new zone and make its world coordinates as in the database
            CALL SGS_ZONE( WZONE( 1 ), WZONE( 2 ), WZONE( 3 ),
     :                     WZONE( 4 ), NEWZON, STATUS )
            CALL SGS_SW( CURWOR( 1 ), CURWOR( 2 ), CURWOR( 3 ),
     :                   CURWOR( 4 ), STATUS )
         ENDIF

*   Else return the current zone
      ELSE
         CALL SGS_ICURZ( NEWZON )
      ENDIF

*   If the current picture has write access and the workstation has
*   the clear flag set then clear the zone
      IF ( PICACS( CURPID ) .EQ. 'WRITE' ) THEN
         GOTIT = .FALSE.
         DO I = 1, CLRLEN
            IF ( ( CLEARF( I ) .NE. 0 ) .AND.
     :           ( CLEARW( I ) .EQ. WKNAME ) ) THEN
               GOTIT = .TRUE.
               CLRNUM = I
            ENDIF
         ENDDO

*   If the clear flag is set then clear the zone and reset the flag
         IF ( GOTIT ) THEN
            CALL SGS_CLRZ
            CALL SGS_FLUSH
            CLEARF( CLRNUM ) = 0
         ENDIF
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGS_NZONE +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

