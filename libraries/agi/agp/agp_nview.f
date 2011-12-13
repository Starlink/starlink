************************************************************************

      SUBROUTINE AGP_NVIEW ( BORDER, STATUS )

*+
*  Name:
*     AGP_NVIEW
*
*  Purpose:
*     Create a new PGPLOT viewport from the current picture
*     (Version for use with native PGPLOT)
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
*        Open PGPLOT for the AGI device with the /APPEND option.
*        Check that PGPLOT has been opened successfully.
*     Endif
*     Calculate the ndc's of the current picture.
*     Create the new PGPLOT viewport.
*     If the current picture has 'WRITE' access then
*        Clear the viewport.
*     Endif
*     If a border has been requested then create a smaller viewport.
*
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

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
*     TIMJ: Tim Jenness (JAC, Hawaii)
*
*  History:
*     Oct 1988 (NE):
*        Original version
*     Oct 1992 (NE):
*        Last edit to Starlink GKS based version
*     Dec 1999 (BKM):
*        Totally revise logic for native PGPLOT version
*     20-Apr-2001: (BKM)
*        Repair logic for clearing a PGPLOT "Window" - erroneously clearing
*        the whole page.
*     1-NOV-2001 (DSB):
*        Open PGPLOT using AGP1_PGBEG.
*     16-NOV-2001 (DSB):
*        Use AGP1_CHKDV to check the device has not changed.
*     22-APR-2009 (TIMJ):
*        ISTAT should not be used. Use STATUS instead.
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
      LOGICAL FOUND, GOTIT

      INTEGER CLRNUM, CURMID, BASMID, I, LENSTR, PICNUM,
     :        OLDCOL, OLDFIL

      REAL CURDEV( 4 ), CURNDC( 4 ), CURWOR( 4 ), BASDEV( 4 ),
     :     BASNDC(4), BASWOR(4), PNDC( 4 ), SPACE, XSIZE, YSIZE

      CHARACTER * ( AGI__CMAX ) CURCOM, BASCOM
      CHARACTER * ( AGI__SZNAM ) CURNAM, BASNAM
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
            CALL ERR_REP( 'AGP_NVIEW_PICNF',
     :                    'Data on current picture not found',
     :                    STATUS )
            GOTO 99
         ENDIF

      ELSE
         STATUS = AGI__NOCUP
         CALL ERR_REP( 'AGP_NVIEW_NOCUP', 'No current picture', STATUS )
         GOTO 99
      ENDIF

*  Get the AGI workstation name for the currently open PGPLOT device.
      CALL AGP_CURAG( PWKNAM, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 99

*   If PGPLOT is not open then open it now for the current workstation
      IF( PWKNAM .EQ. ' ' ) THEN
         CALL AGP1_PGBEG( ' ', CGRAWK( CURPID ),STATUS )
         IF (STATUS .EQ. SAI__OK) THEN
*   Check to see that PGPLOT has opened successfully
            CALL PGQINF( 'STATE', STRING, LENSTR )
            IF ( STRING( :LENSTR ) .EQ. 'CLOSED' ) THEN
               STATUS = AGI__WKNOP
               CALL ERR_REP( 'AGP_NVIEW_WKNOP',
     :              'Problems opening workstation', STATUS )
               GOTO 99
            ENDIF
         ENDIF
      ENDIF

*  Check that the device associated with the current picture is the same
*  as the currently opened device.
      CALL AGP1_CHKDV( WKNAME, STATUS )

*   Check is the current picture is the base picture
      IF( CURNAM .EQ. 'BASE' ) THEN
         PNDC( 1 ) = 0.0
         PNDC( 2 ) = 1.0
         PNDC( 3 ) = 0.0
         PNDC( 4 ) = 1.0
      ELSE

*   Get the coordinates of the base picture
         CALL AGI_1RPIC( WKNAME, 1, BASNAM, BASCOM, BASDEV,
     :                   BASNDC, BASWOR, BASMID, FOUND, STATUS )
         IF( (.NOT. FOUND) .OR. ( BASNAM .NE. 'BASE' ) ) THEN
            STATUS = AGI__PICNF
            CALL ERR_REP( 'AGP_NVIEW_PICNF',
     :                    'Data on base picture not found',
     :                    STATUS )
            GOTO 99
         ENDIF

*   Calculate new PGPLOT NDC viewport size. The AGI NDC coordinates are
*   held in GKS/SGS format and will need scaled to PGPLOT form.
         IF( BASNDC( 2 ) .LT. 1.0 ) THEN
            PNDC( 1 ) = CURNDC( 1 ) / BASNDC( 2 )
            PNDC( 2 ) = CURNDC( 2 ) / BASNDC( 2 )
         ELSE
            PNDC( 1 ) = CURNDC( 1 )
            PNDC( 2 ) = CURNDC( 2 )
         ENDIF
         IF( BASNDC( 4 ) .LT. 1.0 ) THEN
            PNDC( 3 ) = CURNDC( 3 ) / BASNDC( 4 )
            PNDC( 4 ) = CURNDC( 4 ) / BASNDC( 4 )
         ELSE
            PNDC( 3 ) = CURNDC( 3 )
            PNDC( 4 ) = CURNDC( 4 )
         ENDIF
      ENDIF

*   Create the new viewport and make its world coordinates as in the database
      CALL PGSVP( PNDC( 1 ), PNDC( 2 ), PNDC( 3 ), PNDC( 4 ) )
      CALL PGSWIN( CURWOR( 1 ), CURWOR( 2 ), CURWOR( 3 ), CURWOR( 4 ) )

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

*   If the clear flag is set then clear the window and reset the flag
         IF ( GOTIT ) THEN

*   Do not do the block fill if the device is a hardcopy
            CALL PGQINF( 'HARDCOPY', STRING, LENSTR )
            IF ( STRING( :LENSTR ) .EQ. 'NO' ) THEN

*   PGPLOT does not have a specific clearing routine so have to use
*   a rectangle block fill.
               CALL PGQCI( OLDCOL )
               CALL PGQFS( OLDFIL )
               CALL PGSCI( 0 )
               CALL PGSFS( 1 )
               CALL PGRECT( CURWOR(1), CURWOR(2), CURWOR(3), CURWOR(4) )
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
         CALL PGSVP( PNDC( 1 ) + SPACE, PNDC( 2 ) - SPACE,
     :                 PNDC( 3 ) + SPACE, PNDC( 4 ) - SPACE )

*   Set the world coordinates to be a like a default set
         CALL PGSWIN( 0.0, 1.0, 0.0, 1.0 )
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
