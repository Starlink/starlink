      SUBROUTINE KPS1_PLMSK( NPOLY, LBND1, UBND1, LBND2, UBND2, NVERT,
     :                       XVERT, YVERT, LOGPOS, FD, MASK, STATUS )
*+
*  Name:
*     KPS1_PLMSK

*  Purpose:
*     Insert a polygon into a a 2d logical mask identifying pixels which
*     are inside any polygon.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PLMSK( NPOLY, LBND1, UBND1, LBND2, UBND2, NVERT, XVERT,
*                      YVERT, LOGPOS, FD, MASK, STATUS )

*  Description:
*     A polygonal region of the mask array is defined by the
*     coordinates supplied in arguments XVERT and YVERT. Mask pixels
*     which have centres within the polygon are returned set to .TRUE.
*     and others are returned unchanged. If NPOLY is supplied equal to
*     zero, then all mask pixels are initially set to .FALSE.
*
*     The co-ordinates of the vertices are written out to a log file if
*     required.

*  Arguments:
*     NPOLY = INTEGER (Given)
*        The number of polygons previously added into the mask.
*     LBND1 = INTEGER (Given)
*        The lower bound of the first axis of the mask array.
*     UBND1 = INTEGER (Given)
*        The upper bound of the first axis of the mask array.
*     LBND2 = INTEGER (Given)
*        The lower bound of the second axis of the mask array.
*     UBND2 = INTEGER (Given)
*        The upper bound of the second axis of the mask array.
*     NVERT = INTEGER (Given)
*        The number of vertices in the polygon.
*     XVERT( NVERT ) = REAL (Given)
*        The pixel coordinates of the vertices on the first axis.
*     YVERT( NVERT ) = REAL (Given)
*        The pixel coordinates of the vertices on the second axis.
*     LOGPOS = LOGICAL (Given)
*        If .TRUE., then the co-ordinates of each vertex are written out
*        to a text file identified by FD.
*     FD = INTEGER (Given)
*        A FIO file descriptor for the log file. Ignored if LOGPOS is
*        .FALSE.
*     MASK( LBND1:UBND1, LBND2:UBND2 ) = _LOGICAL (Given and Returned)
*        The returned mask.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     CUR: Malcolm Currie (STARLINK)
*     RFWS: Rodney Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     20-OCT-1993 (DSB):
*        Original version, derived from routine PLYSMP written by CUR
*        and RFWS.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER NPOLY
      INTEGER LBND1
      INTEGER UBND1
      INTEGER LBND2
      INTEGER UBND2
      INTEGER NVERT
      REAL XVERT( NVERT )
      REAL YVERT( NVERT )
      LOGICAL LOGPOS
      INTEGER FD

*  Arguments Given and Returned:
      LOGICAL MASK(  LBND1:UBND1, LBND2:UBND2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MXCRS            ! Max. allowable no. of line crossings
      PARAMETER ( MXCRS = 100 )

*  Local Variables:
      CHARACTER
     :  BUF*80                 ! Buffer for output to text file

      REAL
     :  DY,                    ! Difference in y between adjacent
                               ! vertices
     :  PERT,                  ! Perturbation
     :  TEST,                  ! Line-polygon intersection test
     :  XCROSS( MXCRS ),       ! Line crossings
     :  XMIN,                  ! Minimum x in the polygon
     :  XMAX,                  ! Maximum x in the polygon
     :  XT,                    ! Dummy for sorting
     :  YL,                    ! Y pixel coordinate
     :  YMIN,                  ! Minimum y in the polygon
     :  YMAX                   ! Maximum y in the polygon

      INTEGER
     :  I, J, N,               ! Loop counters
     :  LBUF,                  ! Used length of log file buffer
     :  LIMIT( 4 ),            ! Bounds of enclosing rectangle
     :  MINX,                  ! Minimum x in the polygon to nearest
                               ! array element
     :  MAXX,                  ! Maximum x in the polygon to nearest
                               ! array element
     :  MAXY,                  ! Maximum y in the polygon to nearest
                               ! array element
     :  MINY,                  ! Minimum y in the polygon to nearest
                               ! array element
     :  N1, N2,                ! Vertex numbers
     :  NCROSS,                ! Number of intersections
     :  NTOP                   ! Sorting index

      LOGICAL                  ! True if:
     :  EXIT                   ! Sorting complete

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Check argument validity
      IF( NVERT .LT. 3 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_PLMSK_IVERT', 'KPS1_PLMSK: There '//
     :                 'must be at least three points to define a '//
     :                 'polygon', STATUS )
         GO TO 999
      END IF

*  If this is the first polygon to be put into the mask, initialise the
*  mask to hold .FALSE. at every pixel (i.e. all pixels are initially
*  considered to be outside the polygon).
      IF( NPOLY .EQ. 0 ) THEN

         DO J = LBND2, UBND2
            DO I = LBND1, UBND1
               MASK( I, J ) = .FALSE.
            END DO
         END DO

      END IF

*  Find the maximum and minimum x and y pixel coordinates at the
*  supplied polygon vertices, and write out the coordinates to the log
*  file if required.
      XMIN = VAL__MAXR
      XMAX = VAL__MINR
      YMIN = VAL__MAXR
      YMAX = VAL__MINR

      DO N = 1, NVERT
         XMIN = MIN( XVERT( N ), XMIN )
         XMAX = MAX( XVERT( N ), XMAX )
         YMIN = MIN( YVERT( N ), YMIN )
         YMAX = MAX( YVERT( N ), YMAX )

         IF( LOGPOS ) THEN
            CALL MSG_SETR( 'X', XVERT( N ) )
            CALL MSG_SETR( 'Y', YVERT( N ) )
            CALL MSG_LOAD( ' ', '^X ^Y', BUF, LBUF, STATUS )
            CALL FIO_WRITE( FD, BUF( : LBUF ), STATUS )
         END IF

      END DO

*  Convert the ranges to integer pixel index limits restricted to the
*  mask array size. Note, the value 1.0E8 is used to avoid integer
*  overflow where possible one could have used REAL( VAL__MAXI ). The
*  smaller value is used to avoid any possibility of floating point
*  rounding problems causing the value to creep above the maximum value
*  storable as an integer, and thus causing an overflow when the NINT
*  function is evaluated.
      MINX = NINT( MIN( MAX( -1.0E8, XMIN + 0.5 ), 1.0E8 ) )
      LIMIT( 1 ) = MIN( MAX( MINX, LBND1 ), UBND1 )

      MAXX = NINT( MIN( MAX( -1.0E8, XMAX + 0.5 ), 1.0E8 ) )
      LIMIT( 2 ) = MIN( MAX( MAXX, LBND1 ), UBND1 )

      MINY = NINT( MIN( MAX( -1.0E8, YMIN + 0.5 ), 1.0E8 ) )
      LIMIT( 3 ) = MIN( MAX( MINY, LBND2 ), UBND2 )

      MAXY = NINT( MIN( MAX( -1.0E8, YMAX + 0.5 ), 1.0E8 ) )
      LIMIT( 4 ) = MIN( MAX( MAXY, LBND2 ), UBND2 )

*  Scan the range of mask lines which cross the polygon.
      DO J = LIMIT( 3 ), LIMIT( 4 )

*  Store the Y pixel coordinate at the vertical centre of the mask
*  line.
         YL = REAL( J ) - 0.5

*  Problems occur in counting the number of intersections if any array
*  line passes exactly through a polygon vertex. Therefore, the line
*  positions are shifted by a negligible amount PERT to ensure this
*  does not happen.
         PERT = 1.0E-4

*  Loop back to here with a new value for PERT if the current value
*  causes a vertex to fall exactly on the current line. Initialise the
*  number of times the current line intersects the polygon.
  20     CONTINUE
         NCROSS = 0

*  Scan through the x-y positions, testing if each polygon side
*  intersects the array line.
         DO N1 = 1, NVERT
            N2 = N1 + 1

*  Polygon vertices cycle back to the start.
            IF( N2 .GT. NVERT ) N2 = 1

*  Form the intersection test.
            TEST = ( ( YVERT( N1 ) - YL ) - PERT ) *
     :             ( ( YL - YVERT( N2 ) ) + PERT )

*  If TEST is zero, the line passes through a vertex. Therefore, change
*  PERT and start again ( "the line" refers to a line through the
*  middle of the pixel. The act of increasing PERT effectively moves the
*  line a small amount in the +ve Y direction).
            IF( ABS( TEST ) .LT. VAL__SMLR ) THEN
               PERT = PERT + 1.0E-4
               GO TO 20

*  If TEST is positive, adjacent vertices lie on opposite sides of the
*  array line. Calculate the point of intersection (as a pixel
*  coordinate) and store it.
            ELSE IF( TEST .GT. 0.0 ) THEN
               NCROSS = NCROSS + 1

               IF( NCROSS .LE. MXCRS ) THEN
                  DY = YVERT( N2 ) - YVERT( N1 )

                  IF( ABS( DY ) .LT. VAL__SMLR ) DY = SIGN( VAL__SMLR,
     :                                                      DY )
                  XCROSS( NCROSS ) = XVERT( N1 ) +
     :                               ( YL - YVERT( N1 ) ) *
     :                               ( XVERT( N2 ) - XVERT( N1 ) ) / DY

*  If the storage for intersections is exceeded, return with a bad
*  status and report an error.
               ELSE
                  STATUS = SAI__ERROR
                  CALL ERR_REP ( 'KPS1_PLMSK_TMCRS',
     :                           'KPS1_PLMSK: There were too many '//
     :                           'intersections of the polygon with '//
     :                           'array lines.', STATUS )
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
                  IF( XCROSS( N ) .GT. XCROSS( N + 1 ) ) THEN
                     XT = XCROSS( N + 1 )
                     XCROSS( N + 1 ) = XCROSS( N )
                     XCROSS( N ) = XT
                     EXIT = .FALSE.
                  END IF
               END DO
            END DO

*  Scan through the ordered intersections in pairs.
            DO N = 2, NCROSS, 2

*  Find the pixel index bounds corresponding to the current section of
*  the current mask line. The bounds are limited to the bounds of the
*  smallest rectangle enclosing the polygon. The end pixels are included
*  if their centres fall within the intersection.
               MINX = NINT( MIN( MAX( -1.0E8, XCROSS( N - 1 )  ),
     :                           1.0E8 ) ) + 1
               MINX = MAX( MINX, LIMIT( 1 ) )

               MAXX = NINT( MIN( MAX( -1.0E8, XCROSS( N ) ), 1.0E8 ) )
               MAXX = MIN( MAXX, LIMIT( 2 ) )

*  Set mask pixels lying between each pair of intersections to .TRUE.
               DO I = MINX, MAXX
                  MASK( I, J ) = .TRUE.
               END DO

*  Do the next pair of interesections of the polygon with the current
*  mask line.
            END DO

         END IF

*  Do the next mask line.
      END DO

*  Arrive here if an error occurs.
 999  CONTINUE

      END
