************************************************************************

      SUBROUTINE AGP_NVIEW ( BORDER, STATUS )

*+
*  Name:
*     AGP_NVIEW
*
*  Purpose:
*     Create a new PGPLOT viewport from the current picture
*
*  Invocation:
*     CALL AGP_NVIEW ( BORDER, STATUS )
*
*  Description:
*     Create a new PGPLOT viewport from the current picture. The
*     viewport will be created with the coordinate system of the
*     current picture. The border flag allocates space around the plot
*     for annotation if required. If true the viewport is made
*     approximately 10% smaller than the picture to allow space for
*     annotation. If false the viewport matches the picture exactly.
*     If the device associated with the current picture is not open then
*     this routine will open it, and additionally the viewport will be
*     cleared if the access mode is 'WRITE' (in AGI_ASSOC or AGI_OPEN).
*     If the SGS interface is already active then the first call to
*     this routine will open PGPLOT in the current SGS zone. In this
*     case the viewport normalised device coordinates will not match the
*     coordinate system of the current picture, but will have the
*     default range of 0 to 1.
*
*  Arguments:
*     BORDER = LOGICAL (Given)
*        Flag to indicate if a border is to be left around the viewport
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm:
*     Check status on entry.
*     Check PGPLOT has been activated.
*     Get the details of the current picture.
*     If PGPLOT is already open then
*        Inquire the current workstation name.
*        Convert to an AGI name.
*        Compare this to the workstation of the current picture.
*     Endif
*     If PGPLOT is not open then
*        If AGS is activate then
*           Get the device name from SGS.
*        Else
*           Use the device name supplied to AGI.
*        Endif
*        Open PGPLOT for this device with the /APPEND option.
*        Check that PGPLOT has been opened successfully.
*     Endif
*     Calculate the ndc's of the current picture.
*     If PGPLOT has been opened in an SGS zone use the default viewport
*     Create the new PGPLOT viewport.
*     If the current picture has 'WRITE' access then
*        Use a blockfill to clear the viewport.
*     Endif
*     If a border has been requested then create a smaller viewport.
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
*     Oct 1988 (NE):
*        Original version
*     Aug 1989 (NE):
*        Check that the current PGPLOT device is the correct one
*     Jun 1990 (NE):
*        Argument list in AGI_1GWNAM changed
*        Added MEMID parameter and fudge factors
*     Sep 1990 (NE):
*        Added border
*     Nov 1990 (NE):
*        Added clear flag
*     Jan 1991 (NE):
*        Do not do block clear if a hardcopy device
*     Jun 1991 (NE):
*        Remove common block storage for fudge factors
*     Feb 1992 (NE):
*        GKS 7.4 version: New GKS calls and remove fudge factors
*     Mar 1992 (NE):
*        Use /APPEND in PGBEGIN rather than calling GESC
*     Jul 1992 (NE):
*        Test status from all GKS inquiry routines
*     Oct 1992 (NE):
*        Annul error reports from PGBEGIN
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_idips'
      INCLUDE 'agi_locs'
      INCLUDE 'agi_pfree'

*  Arguments Given :
      LOGICAL BORDER

*  Status :
      INTEGER STATUS

*  Local variables :
      LOGICAL FOUND, GOTIT, NOTOPN, SGSOPN

      INTEGER PGBEGIN
      INTEGER CF, CL, CLRNUM, CURMID, I, IERR, INUM, ISTAT, IWKID,
     :        LENSTR, OLDCOL, OLDFIL, PICNUM

      REAL CURDEV( 4 ), CURNDC( 4 ), CURWOR( 4 ), DUMMY( 4 ), FACTOR,
     :     NDC( 4 ), PNDC( 4 ), SPACE, VNDC( 4 ), XPTS( 5 ), XSIZE,
     :     YPTS( 5 ), YSIZE

      CHARACTER * ( AGI__CMAX ) CURCOM
      CHARACTER * ( AGI__SZNAM ) CURNAM
      CHARACTER * ( DAT__SZNAM ) PWKNAM, WKNAME
      CHARACTER DEVNAM * 64, STRING * 40
*.

*   Check status on entry
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Check that PGPLOT has started
      IF ( .NOT. CPGPON ) THEN
         STATUS = AGI__GRPNA
         CALL ERR_REP( 'AGP_NVIEW_GRPNA', 'Graphics package not active',
     :                 STATUS )
         GOTO 99
      ENDIF

*   Get the details of the current picture
      IF ( CURPID .GT. 0 ) THEN
         WKNAME = CAGIWK( CURPID )
         PICNUM = CPICNM( CURPID )

*   Read the contents of the current picture
         IF ( PICNUM .GT. 0 ) THEN
            CALL AGI_1RPIC( WKNAME, PICNUM, CURNAM, CURCOM, CURDEV,
     :                      CURNDC, CURWOR, CURMID, FOUND, STATUS )
         ENDIF

*   If picture is not there then indicate an error
         IF ( ( PICNUM .LE. 0 ) .OR. ( .NOT. FOUND ) ) THEN
            STATUS = AGI__PICNF
            CALL ERR_REP( 'AGP_NVIEW_PICNF', 'Picture not found',
     :                    STATUS )
            GOTO 99
         ENDIF

      ELSE
         STATUS = AGI__NOCUP
         CALL ERR_REP( 'AGP_NVIEW_NOCUP', 'No current picture', STATUS )
         GOTO 99
      ENDIF

*   Check to see if PGPLOT is open
      CALL PGQINF( 'STATE', STRING, LENSTR )
      IF ( STRING( :LENSTR ) .EQ. 'CLOSED' ) THEN
         NOTOPN = .TRUE.

*   If PGPLOT is open then find out some more
      ELSE
         NOTOPN = .FALSE.

*   If PGPLOT is open find out the current workstation name
         CALL PGQINF( 'TYPE', DEVNAM, LENSTR )

*   Convert this into an AGI name
         CALL AGI_1GWNAM( DEVNAM( :LENSTR ), 'GKS', PWKNAM, STATUS )
         IF ( STATUS .NE. SAI__OK ) GOTO 99

*   Compare this name to the name from the current database picture
*   Abort if they are not the same
         IF ( WKNAME .NE. PWKNAM ) THEN
            STATUS = AGI__DIFDV
            CALL ERR_REP( 'AGP_NVIEW_DIFDV', 'Different devices',
     :                    STATUS )
            GOTO 99
         ENDIF
      ENDIF

*   If PGPLOT is not open then do it now for the current workstation
      SGSOPN = .FALSE.
      IF ( NOTOPN ) THEN

*   If GKS is open then use the current viewport to plot in
         IF ( CGKSON ) THEN
            SGSOPN = .TRUE.
            CALL SGS_ICURW( IWKID )
            WRITE( DEVNAM, '(I10)' ) IWKID

*   Otherwise use the workstation specified through AGI
         ELSE
            DEVNAM = CGRAWK( CURPID )

*   Append the /APPEND option to the device name
            CALL CHR_FANDL( DEVNAM, CF, CL )
            CALL CHR_APPND( '/APPEND', DEVNAM, CL )
         ENDIF

*   Mark a new error context to trap errors from PGBEGIN
         CALL ERR_MARK

*   Now open the device for PGPLOT, using the /APPEND option to
*   prevent the screen clearing
         ISTAT = PGBEGIN( 0, DEVNAM, 1, 1 )

*   If the open failed it may be because the workstation name is an
*   IDI name, so try using the AGI name to get a devica name
         IF ( ISTAT .NE. 1 ) THEN
            ISTAT = SAI__OK
            CALL GNS_IGAG( CAGIWK( CURPID ), DEVNAM, ISTAT )
            CALL CHR_FANDL( DEVNAM, CF, CL )
            CALL CHR_APPND( '/APPEND', DEVNAM, CL )
            ISTAT = PGBEGIN( 0, DEVNAM, 1, 1 )
         ENDIF

*   Annul any errors caused by the /APPEND option in PGBEGIN.
*   Devices that cannot support /APPEND report an error but continue.
         IF ( ISTAT .EQ. 1 ) THEN
            CALL ERR_STAT( STATUS )
            IF ( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )
         ENDIF
         CALL ERR_RLSE

*   Check to see that PGPLOT has opened successfully
         CALL PGQINF( 'STATE', STRING, LENSTR )
         IF ( ( STRING( :LENSTR ) .EQ. 'CLOSED' ) .OR.
     :        ( ISTAT .NE. 1 ) ) THEN
            STATUS = AGI__WKNOP
            CALL ERR_REP( 'AGP_NVIEW_WKNOP',
     :                    'Problems opening workstation', STATUS )
            GOTO 99
         ENDIF
      ENDIF

*   Get the normalised device coordinates of the current viewport
      CALL PGQVP( 0, VNDC( 1 ), VNDC( 2 ), VNDC( 3 ), VNDC( 4 ) )

*   Find the length of the device name to use in error reprots
      CF = INDEX( DEVNAM, '/' )
      CL = INDEX( DEVNAM, ' ' ) - 1
      IF ( CF .GT. 0 ) CL = CF - 1

*   Get the viewport size in GKS normalised device coordinates
      CALL GQCNTN( IERR, INUM )
      IF ( IERR .NE. 0 ) THEN
         STATUS = AGI__GKERR
         CALL MSG_SETI( 'ISTAT', IERR )
         CALL ERR_REP( 'AGP_NVIEW_GKERR',
     :                 'Error ^ISTAT returned by GQCNTN', STATUS )
         CALL MSG_SETC( 'NAME', DEVNAM(:CL) )
         CALL ERR_REP( 'AGP_NVIEW_GKERR',
     :                 'Unable to query device ^NAME', STATUS )
         GOTO 99
      ENDIF
      CALL GQNT( INUM, IERR, DUMMY, NDC )
      IF ( IERR .NE. 0 ) THEN
         STATUS = AGI__GKERR
         CALL MSG_SETI( 'ISTAT', IERR )
         CALL ERR_REP( 'AGP_NVIEW_GKERR',
     :                 'Error ^ISTAT returned by GQNT', STATUS )
         CALL MSG_SETC( 'NAME', DEVNAM(:CL) )
         CALL ERR_REP( 'AGP_NVIEW_GKERR',
     :                 'Unable to query device ^NAME', STATUS )
         GOTO 99
      ENDIF

*   Calculate the size of the picture in ndc's of the current viewport
      FACTOR = ( CURNDC( 1 ) - NDC( 1 ) ) / ( NDC( 2 ) - NDC( 1 ) )
      PNDC( 1 ) = FACTOR * VNDC( 2 ) + ( 1.0 - FACTOR ) * VNDC( 1 )
      FACTOR = ( CURNDC( 2 ) - NDC( 1 ) ) / ( NDC( 2 ) - NDC( 1 ) )
      PNDC( 2 ) = FACTOR * VNDC( 2 ) + ( 1.0 - FACTOR ) * VNDC( 1 )
      FACTOR = ( CURNDC( 3 ) - NDC( 3 ) ) / ( NDC( 4 ) - NDC( 3 ) )
      PNDC( 3 ) = FACTOR * VNDC( 4 ) + ( 1.0 - FACTOR ) * VNDC( 3 )
      FACTOR = ( CURNDC( 4 ) - NDC( 3 ) ) / ( NDC( 4 ) - NDC( 3 ) )
      PNDC( 4 ) = FACTOR * VNDC( 4 ) + ( 1.0 - FACTOR ) * VNDC( 3 )

*   If PGPLOT has been opened in an SGS zone then use the default viewport
      IF ( SGSOPN ) THEN
         PNDC( 1 ) = 0.0
         PNDC( 2 ) = 1.0
         PNDC( 3 ) = 0.0
         PNDC( 4 ) = 1.0
         SGSOPN = .FALSE.
      ENDIF

*   Create the new viewport and make its world coordinates as in the database
      CALL PGVPORT( PNDC( 1 ), PNDC( 2 ), PNDC( 3 ), PNDC( 4 ) )
      CALL PGWINDOW( CURWOR( 1 ), CURWOR( 2 ), CURWOR( 3 ),
     :               CURWOR( 4 ) )

*   If the current picture has write access and the workstation has
*   the clear flag set then clear the viewport
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

*   Do not do the block fill if the device is a hardcopy
            CALL PGQINF( 'HARDCOPY', STRING, LENSTR )
            IF ( STRING( :LENSTR ) .EQ. 'NO' ) THEN

*   PGPLOT does not have a specific clearing routine so have to use
*   a polygon block fill.
               CALL PGQCI( OLDCOL )
               CALL PGQFS( OLDFIL )
               XPTS( 1 ) = CURWOR( 1 )
               XPTS( 2 ) = CURWOR( 1 )
               XPTS( 3 ) = CURWOR( 2 )
               XPTS( 4 ) = CURWOR( 2 )
               XPTS( 5 ) = CURWOR( 1 )
               YPTS( 1 ) = CURWOR( 3 )
               YPTS( 2 ) = CURWOR( 4 )
               YPTS( 3 ) = CURWOR( 4 )
               YPTS( 4 ) = CURWOR( 3 )
               YPTS( 5 ) = CURWOR( 3 )
               CALL PGSCI( 0 )
               CALL PGSFS( 1 )
               CALL PGPOLY( 5, XPTS, YPTS )
               CALL PGSCI( OLDCOL )
               CALL PGSFS( OLDFIL )
            ENDIF
            CLEARF( CLRNUM ) = 0
         ENDIF
      ENDIF

*   If a border has been requested then create a smaller viewport
      IF ( BORDER ) THEN

*   Set a border of 1/10 ( 4 standard character heights )
         XSIZE = PNDC( 2 ) - PNDC( 1 )
         YSIZE = PNDC( 4 ) - PNDC( 3 )
         SPACE = MIN( XSIZE, YSIZE ) / 10.0
         CALL PGVPORT( PNDC( 1 ) + SPACE, PNDC( 2 ) - SPACE,
     :                 PNDC( 3 ) + SPACE, PNDC( 4 ) - SPACE )

*   Set the world coordinates to be a like a default set
         CALL PGWINDOW( 0.0, 1.0, 0.0, 1.0 )
      ENDIF

*   Flush HDS if database file has been updated
      IF ( FLUSH ) THEN
*         CALL HDS_FREE( DABLOC, STATUS )
         FLUSH = .FALSE.
      ENDIF

  99  CONTINUE

*      print*, '+++++ AGP_NVIEW +++++'
*      call HDS_SHOW( 'FILES', status )
*      call HDS_SHOW( 'LOCATORS', status )

      END

