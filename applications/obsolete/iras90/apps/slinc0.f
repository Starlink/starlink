      SUBROUTINE SLINC0( PNLON, PNLAT, PNARC, MODE, IRA, SCS, LBND,
     :                   UBND, MXNSCT, NMERD, MLON, MLAT, MARCLN,
     :                   STATUS )
*+
*  Name:
*     SLINC0

*  Purpose:
*     Interactively draw meridian sections

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINC0( PNLOG, PNLAT, PNARC, MODE, IRA, SCS, LBND, UBND,
*                  MXNSCT, NMERD, MLON, MLAT, MARCLN, STATUS )

*  Description:
*     This subroutine is used to draw sections of meridians
*     interactively. The user will be continuously prompted for the
*     new values to specify next meridian section until a null '!'
*     response is given by the user or in cursor mode, a position
*     outside the image is specified by the cursor. A meridian section
*     is specified by the sky coordinate of its begin position and its
*     length.  When the length supplied by the user is a zero, the
*     subroutine will draw a longest meridian section over the image
*     passing the given begin position. The spcifications of the ploted
*     sections are appended to the ones on entry and returned.

*  Arguments:
*     PNLON = CHARACTER (Given)
*        The name of the parameter used to get the longitude of begin
*        position of the meridian section.
*     PNLAT = CHARACTER (Given)
*        The name of the parameter used to get the latitude of begin
*        position of the meridian section.
*     PNARC = CHARACTER (Given)
*        The name of the parameter used to get the length of the
*        meridian section.
*     MODE = CHARACTER (Given)
*        A measure of the accuracy required for the section.
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     SCS = CHARACTER (Given)
*        Name of sky coordinate system used.
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        The SGS zone bound of the image in pixels.
*     MXNSCT = INTEGER (Given)
*        The max. number of meridian sections can be drawn.
*     NMERD = INDETER (Given and Returned)
*        The number of meridian sections have been drawn.
*     MLON( MXNSCT ) = DOUBLE PRECISION
*        The longitudes of begin positions of the meridian sections have
*        been drawn.
*     MLAT( MXNSCT ) = DOUBLE PRECISION
*        The latitudes of begin positions of the meridian sections have
*        been drawn.
*     MARCLN( MXNSCT ) = DOUBLE PRECISION
*        The length of the meridian sections have been drawn.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     24-JUN-1992 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PAR_ERR'          ! PAR_ error constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      CHARACTER*( * ) PNLON
      CHARACTER*( * ) PNLAT
      CHARACTER*( * ) PNARC
      CHARACTER*( * ) MODE
      INTEGER IRA
      CHARACTER*( * ) SCS
      REAL LBND( 2 ), UBND( 2 )
      INTEGER MXNSCT

*  Arguments Given and Returned:
      INTEGER NMERD
      DOUBLE PRECISION MLON( MXNSCT )
      DOUBLE PRECISION MLAT( MXNSCT )
      DOUBLE PRECISION MARCLN( MXNSCT )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION LON, LAT  ! Longitude & latitude of begin point
      LOGICAL EXIT               ! Null respones flag
      LOGICAL OUT                ! Outside image flag
      DOUBLE PRECISION SCT       ! Length of the the section
      DOUBLE PRECISION TMPLA     ! Temporary latitude value
      DOUBLE PRECISION TMPSCT    ! Temporary section length

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If on entry the number of meridian has been drawn exceed the upper
*  limit, report and exit.
      IF ( NMERD .GT. MXNSCT ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'M', MXNSCT )
         CALL ERR_REP( 'SLINC0_ERR1',
     :                 'SLINC0: Maximum number of curves (^M) exceeded',
     :                 STATUS )
         GO TO 999
      END IF


*  Enter a do loop until a null response is obtained or the position
*  specified by the cursor outside the zone, or the number of meridian
*  sections has been drawn exceed the uplimit.
      EXIT = .FALSE.
      DO WHILE ( .NOT.EXIT .AND. NMERD .LE. MXNSCT
     :           .AND. STATUS .EQ. SAI__OK )

*  If working in keyboard mode, getting the begin position of the
*  meridian sections from the keyboard.
         IF ( MODE( : 8 ) .EQ. 'KEYBOARD' ) THEN
            CALL IRA_GETCO( PNLON, PNLAT,
     :                      ' at the start of the meridian arc',
     :                      SCS, .FALSE., LON, LAT, STATUS )

*  If getting a null response, set the exit flag and annul the error
*  status.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               EXIT = .TRUE.
               CALL ERR_ANNUL( STATUS )
            END IF

*  Otherwise get the begin position from the cursor.
         ELSE

*  Write help messages.
            CALL MSG_BLANKIF( MSG__NORM, STATUS )
            CALL MSG_OUTIF( MSG__NORM, 'SLINC0_MSG1',
     :   '  Position the cursor at the start of the curve and press '//
     :   'any button (position the cursor outside the image to exit).',
     :                         STATUS )

            CALL IRM_SKCUR( IRA, SCS, LBND, UBND, LON, LAT, OUT,
     :                      STATUS )

*  If the cursor position is outside the image, set the exit flag.
            IF ( OUT ) EXIT = .TRUE.

         END IF

*  IF not exit and no other error, draw the section. And record the
*  specification.
         IF ( .NOT.EXIT .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the length of the arc in degrees, and convert to radians.
            CALL PAR_GET0D( PNARC, SCT, STATUS )
            SCT = SCT*IRA__DTOR

*  If null is obtained, set the exit flag.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               EXIT = .TRUE.
               CALL ERR_ANNUL( STATUS )

            ELSE

*  If the section length is zero, use 2*PI instead to ensure that the
*  maximum length of the curve is drawn.
               IF( SCT .EQ. 0.0 ) SCT = IRA__TWOPI

*  Draw the curve.
               CALL IRA_DRMER( IRA, LON, LAT, SCT, SCS, LBND, UBND,
     :                         STATUS )
            END IF

*  If a section is drawn successfully, flush out the drawing and
*  record the section specification.
            IF ( STATUS .EQ. SAI__OK .AND. .NOT.EXIT ) THEN
               CALL SGS_FLUSH
               NMERD = NMERD + 1
               MLON( NMERD ) = LON
               MLAT( NMERD ) = LAT
               MARCLN( NMERD ) = SCT
            END IF

         END IF

*  Canncel the parameters for the use of next entry.
         IF ( MODE( : 8 ) .EQ. 'KEYBOARD' ) THEN
            CALL PAR_CANCL( PNLON, STATUS )
            CALL PAR_CANCL( PNLAT, STATUS )
         END IF
         CALL PAR_CANCL( PNARC, STATUS )

      END DO

 999  CONTINUE

      END
