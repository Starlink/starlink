      SUBROUTINE SPHOZ2( COLLO, COLHI, ROWLO, ROWHI, DATA, SCALE,
     :                   PIXSOL, BACK, NVERT, XVERT, YVERT, LOGING, FD,
     :                   IDA, SCS, MEAN, FLUXD, SIGMA, STATUS )
*+
*  Name:
*     SPHOZ2

*  Purpose:
*     Integrate flux density in a polygonal aperture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPHOZ2( COLLO, COLHI, ROWLO, ROWHI, DATA, SCALE, PIXSOL,
*                  BACK, NVERT, XVERT, YVERT, LOGING, FD, IDA, SCS,
*                  MEAN, FLUXD, SIGMA, STATUS )

*  Description:
*     The flux density (after subtraction of the supplied background
*     surface brightness) within the polygonal aperture specified by
*     the supplied argument values is integrated. The results are
*     displayed on the standard output device, logged (optionally) to a
*     log file and returrned in arguments MEAN, FLUXD and SIGMA. The
*     algorithm for finding the interior pixels of the polygon is
*     lifted from the KAPPA routine PLYSMP.

*  Arguments:
*     COLLO = INTEGER (Given)
*        Lower bound on column number in the image.
*     COLHI = INTEGER (Given)
*        Upper bound on column number in the image.
*     ROWLO = INTEGER (Given)
*        Lower bound on row number in the image.
*     ROWHI = INTEGER (Given)
*        Upper bound on row number in the image.
*     DATA( COLLO:COLHI, ROWLO:ROWHI ) = REAL (Given)
*        The input data array.
*     SCALE = REAL (Given)
*        The factor which converts input data values into units of
*        Jy/pixel.
*     PIXSOL = REAL (Given)
*        Solid angle of a pixel, in steradians.
*     BACK = REAL (Given)
*        Background surface brightness, in the same units as DATA.
*     NVERT = INTEGER (Given)
*        The number of vertices in the polygon.
*     XVERT( NVERT ) = REAL (Given)
*        X image coordinate of each vertex.
*     YVERT( NVERT ) = REAL (Given)
*        Y image coordinate of each vertex.
*     LOGING = LOGICAL (Given)
*        True if a log file is to be written to.
*     FD = INTEGER (Given)
*        FIO file descriptor for log file. Not used if LOGING is false.
*     IDA = INTEGER (Given)
*        Identifier for astrometry information.
*     SCS = CHARACTER * ( * ) (Given)
*        Sky coordinate system for displaying sky coordinate values.
*     MEAN = REAL (Returned)
*        The mean surface brightness, in MJy/sr.
*     FLUXD = REAL( Returned)
*        The total flux density, in Jy.
*     SIGMA = REAL (Returned)
*        The standard deviation of the surface brightness, in MJy/sr.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     16-JUL-1993 (DSB):
*        Original version.
*     22-SEP-1993 (DSB):
*        Arguments MEAN, FLUXD and SIGMA added.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER COLLO
      INTEGER COLHI
      INTEGER ROWLO
      INTEGER ROWHI
      REAL DATA( COLLO:COLHI, ROWLO:ROWHI )
      REAL SCALE
      REAL PIXSOL
      REAL BACK
      INTEGER NVERT
      REAL XVERT( NVERT )
      REAL YVERT( NVERT )
      LOGICAL LOGING
      INTEGER FD
      INTEGER IDA
      CHARACTER SCS*(*)

*  Arguments Returned:
      REAL MEAN
      REAL FLUXD
      REAL SIGMA

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants :
      INTEGER MXCRS              ! Max. allowable no. of line crossings
      PARAMETER ( MXCRS = 100 )

*  Local Variables:
      CHARACTER
     :     ATEXT*(IRA__SZFSC),   ! Formatted longitude value
     :     BTEXT*(IRA__SZFSC),   ! Formatted latitude value
     :     TEXT*80               ! Buffer for output text

      DOUBLE PRECISION
     :     A,                    ! Longitude value
     :     B,                    ! Latitude value
     :     XX,                   ! Central X image coordinate
     :     YY                    ! Central Y image coordinate

      INTEGER
     :     I,                    ! Column index
     :     IAT,                  ! Position of last non-blank character
     :     ISUM,                 ! Sum of I values.
     :     J,                    ! Row index
     :     JSUM,                 ! Sum of J values.
     :     LIMIT( 4 ),           ! Bounds of box enclosing polygon
     :     MAXX,                 ! Maximum X pixel index
     :     MINX,                 ! Minimum X pixel index
     :     N,                    ! Loop counter
     :     N1,                   ! Vertex number
     :     N2,                   ! Vertex number
     :     NCROSS,               ! Number of intersections
     :     NGOOD,                ! No. of good pixel values in aperture
     :     NTOP,                 ! Sorting index
     :     NTOT                  ! Total no. of pixels within polygon

      LOGICAL                
     :     EXIT                  ! Sorting complete?

      REAL
     :     DATVAL,               ! Input data value (minus background)
     :     DY,                   ! Difference in y between vertices
     :     PERT,                 ! Perturbation
     :     SUM1,                 ! Sum of data values 
     :     SUM2,                 ! Sum of squared data values 
     :     TEST,                 ! Line-polygon intersection test
     :     XCROSS( MXCRS ),      ! Line crossings
     :     XHI,                  ! Upper X limit for current row
     :     XLO,                  ! Lower X limit for current row
     :     XMAX,                 ! Maximum x in the polygon
     :     XMIN,                 ! Minimum x in the polygon
     :     XT,                   ! Dummy for sorting
     :     YL,                   ! Y pixel coordinate
     :     YMAX,                 ! Maximum y in the polygon
     :     YMIN                  ! Minimum y in the polygon

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check argument validity
      IF ( NVERT .LT. 3 ) THEN
         CALL MSG_OUTIF( MSG__QUIET, 'SPHOZ2_MSG1',
     :                   'WARNING: There must be at least three '//
     :                   'points to defined a polygon', STATUS )
         GO TO 999
      END IF

*  Initialise sums.
      NGOOD = 0
      SUM1 = 0.0
      SUM2 = 0.0
      ISUM = 0
      JSUM = 0
      NTOT = 0

*  Find the maximum and minimum x and y pixel coordinates from the x-y
*  co-ordinates.
      XMIN = VAL__MAXR
      XMAX = VAL__MINR
      YMIN = VAL__MAXR
      YMAX = VAL__MINR
 
      DO N = 1, NVERT
         XMIN = MIN( XVERT( N ), XMIN )
         XMAX = MAX( XVERT( N ), XMAX )
         YMIN = MIN( YVERT( N ), YMIN )
         YMAX = MAX( YVERT( N ), YMAX )
      END DO
 
*  Convert the ranges to pixel index limits restricted to the input
*  array size.
      LIMIT( 1 ) = MAX( COLLO, MIN( COLHI, NINT( XMIN + 0.5 ) ) )
      LIMIT( 2 ) = MAX( COLLO, MIN( COLHI, NINT( XMAX + 0.5 ) ) )
      LIMIT( 3 ) = MAX( ROWLO, MIN( ROWHI, NINT( YMIN + 0.5 ) ) )
      LIMIT( 4 ) = MAX( ROWLO, MIN( ROWHI, NINT( YMAX + 0.5 ) ) )

*  Scan the range of input rows affected.
      DO J = LIMIT( 3 ), LIMIT( 4 )

*  Store the Y pixel coordinate at the centre of this row
         YL = REAL( J ) - 0.5

*  Problems occur in counting the number of intersections if any array
*  line passes exactly through a polygon vertex. Therefore, the line
*  positions are shifted by a negligible amount PERT to ensure this
*  does not happen.
         PERT = 0.0001
 20      NCROSS = 0

*  Scan through the x-y positions, testing if each polygon side
*  intersects the array line.
         DO N1 = 1, NVERT
            N2 = N1 + 1

*  Polygon vertices cycle back to the start.
            IF ( N2 .GT. NVERT ) N2 = 1

*  Form the intersection test using the pixel indices.
            TEST = ( ( YVERT( N1 )  - YL ) - PERT ) *
     :             ( ( YL - YVERT( N2 ) ) + PERT )

*  If TEST is zero, the line passes through a vertex. Therefore, change
*  PERT and start again
            IF ( ABS( TEST ) .LT. VAL__SMLR ) THEN
               PERT = PERT + 0.0001
               GO TO 20

*  If TEST is positive, adjacent vertices lie on opposite sides of the
*  array line. Calculate the X pixel coordinate at the point of
*  intersection and store it.
            ELSE IF ( TEST .GT. 0.0 ) THEN
               NCROSS = NCROSS + 1
 
               IF ( NCROSS .LE. MXCRS ) THEN
                  DY = YVERT( N2 ) - YVERT( N1 )
 
                  IF ( ABS( DY ) .LT. VAL__SMLR )
     :              DY = SIGN( VAL__SMLR, DY )

                     XCROSS( NCROSS ) = XVERT( N1 ) +
     :                              ( (YL - YVERT( N1 ) ) *
     :                              ( XVERT( N2 ) - XVERT( N1 ) ) / DY )
 
               ELSE
 
*  If the storage for intersections is exceeded, report an error.
                  STATUS = SAI__ERROR
                  CALL ERR_REP ( 'SPHOZ2_ERR1',
     :              'SPHOZ2: There were too many intersections of '/
     :              /'the polygon with array lines.', STATUS )
                  GO TO 999

               END IF
 
*  End of the check for line-polygon intersections.
            END IF

*  End of the loop through the polygon vertices.
         END DO
 
*  If the line intersects the polygon, sort intersections into x order.
         IF( NCROSS .GT. 1 ) THEN
            EXIT = .FALSE.
            NTOP = NCROSS
 
*  Loop when an interchange was necessary.
            DO WHILE ( .NOT. EXIT )
               EXIT = .TRUE.
               NTOP = NTOP - 1
 
               DO N = 1, NTOP

*  Swap adjacent values if they are in the wrong order.
                  IF ( XCROSS( N ) .GT. XCROSS( N + 1 ) ) THEN
                     XT = XCROSS( N + 1 )
                     XCROSS( N + 1 ) = XCROSS( N )
                     XCROSS( N ) = XT
                     EXIT = .FALSE.
                  END IF
               END DO
            END DO
 
*  Scan through the ordered intersections in pairs.
            DO N = 2, NCROSS, 2

*  Get the pixel indices of the columns at which the current row
*  intersects the polygon.
               MINX = MAX( LIMIT( 1 ),
     :                MIN( LIMIT( 2 ), NINT( XCROSS( N - 1 ) + 0.5 ) ) )

               MAXX = MAX( LIMIT( 3 ),
     :                MIN( LIMIT( 4 ), NINT( XCROSS( N ) + 0.5 ) ) )

*  Increment statistics using pixels lying between each pair of
*  intersections.
               DO I = MINX, MAXX

*  Increment the sums used to calculate the centroid position within
*  the polygon.
                  ISUM = ISUM + I
                  JSUM = JSUM + J
                  NTOT = NTOT + 1

*  Only include this pixel value if it is not bad.
                  DATVAL = DATA( I, J )
                  IF( DATVAL .NE. VAL__BADR ) THEN

*  Subtract the supplied background from the input pixel value.
                     DATVAL = DATVAL - BACK

*  Increment the statistics.
                     SUM1 = SUM1 + DATVAL            
                     SUM2 = SUM2 + DATVAL*DATVAL
                     NGOOD = NGOOD + 1

                  END IF
 
               END DO
 
            END DO
         END IF
 
      END DO

*  If the aperture has zero size, tell the user.
      IF( NTOT .EQ. 0 ) THEN
         TEXT = '    Aperture has zero size'
         CALL MSG_OUTIF( MSG__NORM, ' ', TEXT, STATUS )
         IF ( LOGING ) CALL FIO_WRITE( FD, TEXT, STATUS )

*  Otherwise, calculate and display the sky coordinates corresponding
*  to the mean position within the polygon.
      ELSE
         XX = DBLE( ( ISUM/NTOT ) - 0.5 )
         YY = DBLE( ( JSUM/NTOT ) - 0.5 )
         
         CALL IRA_TRANS( 1, XX, YY, .TRUE., SCS, IDA, A, B, STATUS )
         CALL IRA_DTOC( A, B, SCS, 0, ATEXT, BTEXT, STATUS )

         TEXT = '    Mean position in polygon: '      
         IAT = 30
         CALL CHR_APPND( ATEXT, TEXT, IAT )
         CALL CHR_APPND( ', '//BTEXT, TEXT, IAT )
         CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
         IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

*  If no good values were found, tell the user.
         IF( NGOOD .EQ. 0 ) THEN
            TEXT = '    No good data found in aperture'
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT, STATUS )
            IF ( LOGING ) CALL FIO_WRITE( FD, TEXT, STATUS )

*  Otherwise, calculate the total flux density in the aperture, in Jy.
         ELSE
            FLUXD = SUM1*SCALE

*  Calculate the mean surface grightness in MJy/sr.
            MEAN = FLUXD/(NGOOD*PIXSOL*1.0E6)

*  Calculate the standard deviation of the surface brightness, in
*  MJy/sr.
            SIGMA = SQRT( MAX( 0.0, ( SUM2/NGOOD ) -
     :              ( SUM1/NGOOD )**2 ) )*SCALE/(PIXSOL*1.0E6)

*  Display the results.
            TEXT = '    Total flux density      : '
            IAT = 30
            CALL CHR_PUTR( FLUXD, TEXT, IAT )
            CALL CHR_APPND( ' Jy', TEXT, IAT )
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
            IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )
      
            TEXT = '    Mean surface brightness : '
            IAT = 30
            CALL CHR_PUTR( MEAN, TEXT, IAT )
            CALL CHR_APPND( ' MJy/sr', TEXT, IAT )
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
            IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

            TEXT = '    Standard deviation      : '
            IAT = 30
            CALL CHR_PUTR( SIGMA, TEXT, IAT )
            CALL CHR_APPND( ' MJy/sr', TEXT, IAT )
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
            IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

            TEXT = '    Used pixels in aperture : '
            IAT = 30
            CALL CHR_PUTI( NGOOD, TEXT, IAT )
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
            IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

            TEXT = '    Total pixels in aperture: '
            IAT = 30
            CALL CHR_PUTI( NTOT, TEXT, IAT )
            CALL MSG_OUTIF( MSG__NORM, ' ', TEXT( : IAT ), STATUS )
            IF( LOGING ) CALL FIO_WRITE( FD, TEXT( : IAT ), STATUS )

         END IF

      END IF

*  Seperate subseuquent results with a blank line
      CALL MSG_BLANKIF( MSG__NORM, STATUS )
      IF( LOGING ) CALL FIO_WRITE( FD, ' ', STATUS )

 999  CONTINUE

      END
