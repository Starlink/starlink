      SUBROUTINE SLINC2( PNLON, PNLAT, PNANG, PNARC, MODE, IRA, SCS,
     :                   LBND, UBND, MXNSCT, NGCRL, GLON, GLAT, GANG,
     :                   GSCTLN, STATUS )
*+
*  Name:
*     SLINC2

*  Purpose:
*     Interactively draw great circle sections

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINC2( PNLON, PNLAT, PNANG, PNARC, MODE, IRA, SCS, LBND,
*                  UBND, MXNSCT, NGCRL, GLON, GLAT, GANG, GSCTLN,
*                  STATUS )

*  Description:
*     This subroutine is used to draw sections of great circles
*     interactively. The user will be continuously prompted for the new
*     values to specify next great circle section until a null '!'
*     response is given by the user. A great circle section is specified
*     by the sky coordinate of its begin position, its position angle
*     and its length. When the length supplied by the user is a zeor,
*     the subroutine will draw a longest great circle section over the
*     image passing the given begin position. The spcifications of the
*     ploted sections are appended to the ones on entry and returned.

*  Arguments:
*     PNLON = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the longitude of begin
*        position of the great circle section.
*     PNLAT = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the latitude of begin
*        position of the great circle section.
*     PNANG = CHARACTER*( * )
*        The name of the parameter used to get the position of the great
*        circle section.
*     PNARC = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the length of the
*        great circle section.
*     MODE = CHARACTER (Given)
*        A measure of the accuracy required for the section.
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     SCS = CHARACTER*( * ) (Given)
*        Name of sky coordinate system used.
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        The SGS zone bound of the image in pixels.
*     MXNSCT = INTEGER (Given)
*        The max. number of parallel sections can be drawn.
*     NGCRL = INDETER (Given and Returned)
*        The number of great circle sections have been drawn.
*     GLON( MXNSCT ) = DOUBLE PRECISION
*        The longitudes of begin positions of the great circle sections
*        have been drawn.
*     GLAT( MXNSCT ) = DOUBLE PRECISION
*        The latitudes of begin positions of the great circle sections
*        have been drawn.
*     GANG( MXNSCT ) = DOUBLE PRECISION
*        The position angles of the great circle section have been
*        drawn.
*     GSCTLN( MXNSCT ) = DOUBLE PRECISION
*        The lengthes of the great circle sections have been drawn.
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
      INCLUDE 'MSG_PAR'          ! MSG_ constant

*  Arguments Given:
      CHARACTER*( * ) PNLON
      CHARACTER*( * ) PNLAT
      CHARACTER*( * ) PNANG
      CHARACTER*( * ) PNARC
      CHARACTER*( * ) MODE
      INTEGER IRA
      CHARACTER*( * ) SCS
      REAL LBND( 2 ), UBND( 2 )
      INTEGER MXNSCT

*  Arguments Given and Returned:
      INTEGER NGCRL
      DOUBLE PRECISION GLON( MXNSCT )
      DOUBLE PRECISION GLAT( MXNSCT )
      DOUBLE PRECISION GANG( MXNSCT )
      DOUBLE PRECISION GSCTLN( MXNSCT )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DBEAR ! Position angle from one point to
                                 ! another

*  Local Variables:
      DOUBLE PRECISION ANG       ! Position angle of a great circle
      DOUBLE PRECISION LON, LAT  ! Longitude & latitude of begin point
      DOUBLE PRECISION LONTEM, LATTEM
                                 ! Longitude & latitude of a temporary
                                 ! position
      LOGICAL EXIT               ! Exit flag
      LOGICAL OUT                ! Outside image flag
      DOUBLE PRECISION SCT       ! Length of the the section

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If on entry the number of meridian has been drawn exceed the upper
*  limit, report and exit.
      IF ( NGCRL .GT. MXNSCT ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'M', MXNSCT )
         CALL ERR_REP( 'SLINC2_ERR1',
     :                 'SLINC2: Maximum number of curves (^M) exceeded',
     :                 STATUS )
         GO TO 999
      END IF

*  Enter a do loop until a null response or an outside position is
*  obtained, or the number of great circle sections has been drawn
*  exceed the uplimit.
      EXIT = .FALSE.
      DO WHILE ( .NOT.EXIT .AND. NGCRL .LE. MXNSCT
     :           .AND. STATUS .EQ. SAI__OK )

*  Use keyboard if in keyboard mode.
         IF ( MODE( : 8 ) .EQ. 'KEYBOARD' ) THEN

*  Get begin position of the great circle section.
            CALL IRA_GETCO( PNLON, PNLAT,
     :                      ' at the start of the great circle arc',
     :                      SCS, .FALSE., LON, LAT, STATUS )

*  Get the postion angle of the great circle, convert from degrees to
*  radians.
            CALL PAR_GET0D( PNANG, ANG, STATUS )
            ANG = ANG*IRA__DTOR

*  If null reponse is obtained, set exit flag and annul the error
*  status.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               EXIT = .TRUE.
               CALL ERR_ANNUL( STATUS )
            END IF

*  Or use cursor in cursor mode.
         ELSE

*  Write help messages.
            CALL MSG_BLANKIF( MSG__NORM, STATUS )
            CALL MSG_OUTIF( MSG__NORM, 'SLINC2_MSG1',
     :   '  Position the cursor at the start of the curve and press '//
     :   'any button (position the cursor outside the image to exit).',
     :                            STATUS )
            CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get the begin position of the great circle section.
            CALL IRM_SKCUR( IRA, SCS, LBND, UBND, LON, LAT, OUT,
     :                      STATUS )

*  If the begin position is inside the image, get another position on
*  the great circle and calculate the position angle of the great
*  circle..
            IF ( .NOT.OUT ) THEN

               CALL MSG_OUTIF( MSG__NORM, 'SLINC2_MSG2',
     :      '  Position the cursor at another point on the curve and '//
     :      'press any button.', STATUS )
               CALL MSG_BLANKIF( MSG__NORM, STATUS )

               CALL IRM_SKCUR( IRA, SCS, LBND, UBND, LONTEM, LATTEM,
     :                         OUT, STATUS )
            END IF

            IF ( .NOT.OUT )
     :         ANG = SLA_DBEAR( LON, LAT, LONTEM, LATTEM )

*  If cursor position outside the image, set exit flag.
            IF ( OUT ) EXIT = .TRUE.

         END IF

*  If not exit and no error happened, draw the section. And record the
*  specification.
         IF ( .NOT.EXIT .AND. STATUS .EQ. SAI__OK ) THEN

*  Get the length of the curve, convet from degrees to radians.
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
               CALL IRA_DRGTC( IRA, LON, LAT, ANG, SCT, SCS, LBND, UBND,
     :                         STATUS )

            END IF

*  If a section is drawn successfully, flush out the drawing and
*  record the section specification.
            IF ( STATUS .EQ. SAI__OK .AND. .NOT.EXIT ) THEN
               CALL SGS_FLUSH
               NGCRL = NGCRL + 1
               GLON( NGCRL ) = LON
               GLAT( NGCRL ) = LAT
               GANG( NGCRL ) = ANG
               GSCTLN( NGCRL ) = SCT
            END IF
         END IF

*  Canncel the parameters for the use of next entry.
         IF ( MODE( : 8 ) .EQ. 'KEYBOARD' ) THEN
            CALL PAR_CANCL( PNLON, STATUS )
            CALL PAR_CANCL( PNLAT, STATUS )
            CALL PAR_CANCL( PNANG, STATUS )
         END IF
         CALL PAR_CANCL( PNARC, STATUS )

      END DO

 999  CONTINUE

      END
