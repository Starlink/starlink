      SUBROUTINE SLINC3( PNLON, PNLAT, MODE, IRA, SCS, LBND, UBND,
     :                   MXNSCT, MXVTCE, NPOLY, NVTCE, PLYLON, PLYLAT,
     :                   STATUS )
*+
*  Name:
*     SLINC3

*  Purpose:
*     Interactively draw a polyline

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SLINC3( PNLON, PNLAT, MODE, IRA, SCS, LBND, UBND,
*             MXNSCT, MXVTCE, NPOLY, NVTCE, PLYLON, PLYLAT, PSCTLN,
*             STATUS )
      
*  Description:
*     This subroutine is used to draw a polyline interactively. The user
*     will be continuously prompted for the next vertice of the polyline
*     until a null '!' response or a position outside the image is
*     given by the user. The lines connecting the vertices are in fact
*     the sections of great circles connecting these position.
*     The spcifications of the polyline are appended to the ones on
*     entry and returned.  

*  Arguments:
*     PNLON = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the longitude of the
*        vertice of the polylin.
*     PNLAT = CHARACTER*( * ) (Given)
*        The name of the parameter used to get the latitude of the
*        vertice of the polylin.
*     MODE = CHARACTER (Given)
*        A measure of the accuracy required for the section.
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     SCS = CHARACTER*( * ) (Given)
*        Name of sky coordinate system used.
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        The SGS zone bound of the image in pixels.
*     MXNSCT = INTEGER (Given)
*        The max. number of polylines can be drawn.
*     MXVTCE = INTEGER (Given)
*        The max. number of vertices a polyline can have.
*     NPOLY = INDETER (Given and Returned)
*        The number of polyline have been drawn.
*     NVTCE( MXNSCT ) = INTEGER (Given)
*        The number of vertices each polyline has
*     PLYLON( MXNSCT, MXVTCE ) = DOUBLE PRECISION
*        The longitudes of vertices of each polyline.
*     PLYLAT( MXNSCT, MXVTCE ) = DOUBLE PRECISION
*        The latitudes of vertices of each polyline.
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
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
                                                
*  Arguments Given:
      CHARACTER*( * ) PNLON
      CHARACTER*( * ) PNLAT
      CHARACTER*( * ) MODE
      INTEGER IRA
      CHARACTER*( * ) SCS
      REAL LBND( 2 ), UBND( 2 )
      INTEGER MXNSCT
      INTEGER MXVTCE
      
*  Arguments Given and Returned:
      INTEGER NPOLY
      INTEGER NVTCE( MXNSCT )
      DOUBLE PRECISION PLYLON( MXNSCT, MXVTCE )
      DOUBLE PRECISION PLYLAT( MXNSCT, MXVTCE )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      DOUBLE PRECISION SLA_DBEAR ! Bearing angle of a position see from
                                 ! other position 
                                
*  Local Variables:
      DOUBLE PRECISION ANG       ! Position of a great circle
      DOUBLE PRECISION DIST      ! Distance between two vertices
      DOUBLE PRECISION LSTLON, LSTLAT
                                 ! Position of last vertice
      DOUBLE PRECISION LON, LAT  ! Longitude & latitude of new vertice
      LOGICAL EXIT               ! Exit flag
      LOGICAL OUT                ! Outside image flag

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If on entry the number of polylines has been drawn exceed the upper
*  limit, report and exit.
      IF ( NPOLY .GT. MXNSCT ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'M', MXNSCT )
         CALL ERR_REP( 'SLINC3_ERR1',
     :                 'SLINC3: Maximum number of curves (^M) exceeded',
     :                 STATUS )
         GO TO 999
      END IF

*  Draw a new polyline.
      NPOLY = NPOLY + 1

*  Enter a do loop until a null response or an outside position is
*  obtained, or the number of vertices has been exceed the uplimit.
      EXIT = .FALSE.
      DO WHILE ( .NOT.EXIT .AND. NVTCE( NPOLY ) .LE. MXNSCT
     :           .AND. STATUS .EQ. SAI__OK )

*  Get position of next vertice.
         IF ( MODE( : 8 ) .EQ. 'KEYBOARD' ) THEN
            CALL IRA_GETCO( PNLON, PNLAT,
     :                      ' of the next poly-line vertex',
     :                      SCS, .FALSE., LON, LAT, STATUS )

*  If a null is obtained, set the exit flag.
            IF ( STATUS .EQ. PAR__NULL ) THEN
               EXIT = .TRUE.
               CALL ERR_ANNUL( STATUS )
            END IF      
         ELSE

            CALL MSG_BLANKIF( MSG__NORM, STATUS )
            CALL MSG_OUTIF( MSG__NORM, 'SLINC3_MSG1',
     : '  Position the cursor at a vertex of the poly-line and press '//
     : 'any button (position the cursor outside the image to exit).',
     :                            STATUS )
            CALL MSG_BLANKIF( MSG__NORM, STATUS )

            CALL IRM_SKCUR( IRA, SCS, LBND, UBND, LON, LAT, OUT,
     :                      STATUS )

*  If cursor position is outside the image, set exit flag.
            IF ( OUT ) EXIT = .TRUE.
         END IF

*  If not exit and no error happened and obtained sky coordinate is
*  valid, the obtained position is a new vertice of the polyline.
         IF ( .NOT.EXIT .AND. STATUS .EQ. SAI__OK .AND.
     :             LON .NE. VAL__BADD .AND. LAT .NE. VAL__BADD ) THEN
            NVTCE( NPOLY ) = NVTCE( NPOLY ) + 1

*  If the given position is the first vertice, do nothing except noting
*  it down.
            IF ( NVTCE( NPOLY ) .EQ. 1 ) THEN
               LSTLON = LON
               LSTLAT = LAT

*  Otherwise, draw the line connecting this vertice to the last one.
            ELSE

*  Find the position angle of the great circle connecting the new
*  vertice and the last vertice at the last vertice.
               ANG = SLA_DBEAR( LSTLON, LSTLAT, LON, LAT )

*  And the length of arc connecting these two vertices on the circle.
               CALL IRA_DIST( LSTLON, LSTLAT, LON, LAT, DIST, STATUS )

*  Draw the arc connecting these two vertices.
               CALL IRA_DRGTC( IRA, LSTLON, LSTLAT, ANG, DIST, SCS,
     :                         LBND, UBND, STATUS )

*  Denote this vertice as last, to continue drawing.
               LSTLON = LON
               LSTLAT = LAT
            END IF

*  If the great circle section is drawn successfully, flush out the
*  drawing and record the specification of this section of the polyline.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL SGS_FLUSH      
               PLYLON( NPOLY, NVTCE( NPOLY ) ) = LON
               PLYLAT( NPOLY, NVTCE( NPOLY ) ) = LAT
            END IF
         END IF

*  Canncel the parameters for to get next vertice position.
         IF ( MODE( : 8 ) .EQ. 'KEYBOARD' ) THEN
            CALL PAR_CANCL( PNLON, STATUS )
            CALL PAR_CANCL( PNLAT, STATUS )
         END IF
      END DO

 999  CONTINUE
      
      END
