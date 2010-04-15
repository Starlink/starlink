      SUBROUTINE SALIA2( LBND1, UBND1, IDA1, IDAR, SCS, LBND2, UBND2,
     :                   STATUS )
*+
*  Name:
*     SALIA2

*  Purpose:
*     Find default bounds for the output image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SALIA2( LBND1, UBND1, IDA1, IDAR, SCS, LBND2, UBND2, STATUS )

*  Description:
*     A grid of test points is transformed from the input image
*     coordinate system to the output image coordinate system and the
*     bounds of this grid in the output image are returned.
*     Complications arise because the grid of test points may not fully
*     sample the output image. Just transforming positions on the edge
*     of the input image is not good enough because some projections may
*     cause the inside of the input image to be mapped outside its
*     boundary (eg a gnomonic input image covering a polar region
*     mapped to an Aitoff output image). For this reason succesively
*     finer grids are used which cover the entire input image, until
*     the output bounds stabalise.
*
*     A grid of test points is defined in the input image. This grid
*     initially consists of just the four corners of the input image.
*     The grid is transformed into sky coordinates using the astrometry
*     information read from the input image, and then to output image
*     coordinates using the reference astrometry information. The range
*     of column and row numbers covered by the transformed points are
*     found. The process is then repeated using a finer grid. Each
*     section of the grid is divided into two (thus the 2x2 array of
*     test points becomes a 3x3 array), and the corresponding area of
*     the output image is found. If the second estimate is more than
*     10% larger than the first (or if there were insufficient good
*     positions in the grid to evaluate the output bounds), then the
*     process is repeated again with a finer grid. The shape of the
*     array of grid points goes through the values 2x2, 3x3, 5x5, 9x9,
*     etc, up to a maximum of 65x65. An error is reported if the output
*     bounds are still increasing when the maximum grid size is
*     reached.

*  Arguments:
*     LBND1( 2 ) = INTEGER (Given)
*        The lower bounds of the input NDF.
*     UBND1( 2 ) = INTEGER (Given)
*        The upper bounds of the input NDF.
*     IDA1 = INTEGER (Given)
*        An IRA identifier for the input astrometry information.
*     IDAR = INTEGER (Given)
*        An IRA identifier for the reference (output) astrometry
*        information.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system used by the reference astrometry
*        information.
*     LBND2( 2 ) = INTEGER (Returned)
*        The lower bounds of the output NDF.
*     UBND2( 2 ) = INTEGER (Returned)
*        The upper bounds of the output NDF.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-FEB-1993 (DSB):
*        Original version.
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
      INTEGER LBND1( 2 )
      INTEGER UBND1( 2 )
      INTEGER IDA1
      INTEGER IDAR
      CHARACTER SCS*(*)

*  Arguments Returned:
      INTEGER LBND2( 2 )
      INTEGER UBND2( 2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXSID             ! Max. no. of test points along each
      PARAMETER ( MAXSID = 65 )  ! edge

*  Local Variables:
      DOUBLE PRECISION
     :         AA( MAXSID*MAXSID ),! Logitudes at test points.
     :         BB( MAXSID*MAXSID ),! Latitudes at test points.
     :         DX,                ! X gap between test points.
     :         DY,                ! Y gap between test points.
     :         XMAX,              ! Max. X value in output grid.
     :         XMIN,              ! Min. X value in output grid.
     :         XX( MAXSID*MAXSID ),! X values for test points.
     :         Y,                 ! Y value for a test point.
     :         YMAX,              ! Max. Y value in output grid.
     :         YMIN,              ! Min. Y value in output grid.
     :         YY( MAXSID*MAXSID ) ! X values for test points.

      INTEGER
     :         I,                 ! Loop count
     :         J,                 ! Loop count
     :         LSIZEX,            ! Prevous size of X axis.
     :         LSIZEY,            ! Prevous size of Y axis.
     :         N,                 ! No. of test points to be transformed
     :         NGOOD,             ! No. of test points transfomed OK.
     :         NSIDE              ! No. of test points along each edge.

      LOGICAL
     :         OK                 ! True if a good estimate of output
                                  ! bounds has been obtained.

      REAL
     :         FRACX,             ! Fractional increase in X axis size
     :         FRACY              ! Fractional increase in Y axis size

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start off with 2 test points along each side, and initialise "last
*  times" image size to 1x1.
      NSIDE = 2
      LSIZEX = 1
      LSIZEY = 1

*  Loop back to here if the current no. of test points is insufficient
*  to get a good measure of the output image bounds.
 10   CONTINUE

*  Find the distance between adjacent test points in the X and Y
*  directions.
      DX = DBLE( UBND1( 1 ) - LBND1( 1 ) + 1 )/DBLE( NSIDE - 1 )
      DY = DBLE( UBND1( 2 ) - LBND1( 2 ) + 1 )/DBLE( NSIDE - 1 )

*  Initialise the number of points to be transformed.
      N = 0

*  Loop round each row of test points.
      DO J = 1, NSIDE

*  Calculate the Y image coordinate for this row of test points.
         Y = DBLE( LBND1( 2 ) - 1 ) + ( J - 1 )*DY

*  Loop round each test point in this row.
         DO I = 1, NSIDE

*  Store the X and Y image coordinate for this test point.
            N = N + 1
            YY( N ) = Y
            XX( N ) = DBLE( LBND1( 1 ) - 1 ) + ( I - 1 )*DX

         END DO

      END DO

*  Transform the test points into sky coordinates in the system
*  identified by IDA1.
      CALL IRA_TRANS( N, XX, YY, .TRUE., SCS, IDA1, AA, BB, STATUS )

*  Transform the test points into image coordinates in the output
*  grid.
      CALL IRA_TRANS( N, AA, BB, .FALSE., SCS, IDAR, XX, YY, STATUS )

*  Find the extreme output image coordinates amongst the transformed
*  test points. Count the number of good test points.
      XMAX = VAL__MIND
      XMIN = VAL__MAXD
      YMAX = VAL__MIND
      YMIN = VAL__MAXD
      NGOOD = 0
      N = 0

      DO J = 1, NSIDE
         DO I = 1, NSIDE

            N = N + 1
            IF( XX( N ) .NE. VAL__BADD ) THEN
               NGOOD = NGOOD + 1
               XMAX = MAX( XMAX, XX( N ) )
               XMIN = MIN( XMIN, XX( N ) )
               YMAX = MAX( YMAX, YY( N ) )
               YMIN = MIN( YMIN, YY( N ) )
            END IF

         END DO
      END DO

*  If sufficient test points were transformed succesfully, convert the
*  pixel coordinates to pixel index bounds.
      IF( NGOOD .GE. 2 ) THEN
         LBND2( 1 ) = NINT( XMIN ) + 1
         UBND2( 1 ) = NINT( XMAX )
         LBND2( 2 ) = NINT( YMIN ) + 1
         UBND2( 2 ) = NINT( YMAX )

*  Find the fractional change in output image size compared to that
*  estimated using the previous (smaller) number of test points.
         FRACX = REAL( UBND2( 1 ) - LBND2( 1 ) + 1 )/REAL( LSIZEX )
         FRACY = REAL( UBND2( 2 ) - LBND2( 2 ) + 1 )/REAL( LSIZEY )

*  If either dimension increased by more than 10%, we still havn't got
*  a good estimate of the output image size.
         IF( FRACX .GT. 1.1 .OR. FRACY .GT. 1.1 ) THEN
            OK = .FALSE.

*  Save last times dimension sizes for future use.
            LSIZEX = UBND2( 1 ) - LBND2( 1 ) + 1
            LSIZEY = UBND2( 2 ) - LBND2( 2 ) + 1

         ELSE
            OK = .TRUE.
         END IF

*  If less than 2 test points could be transformed, we still havn't got
*  a good estimate of the output image size.
      ELSE
         OK = .FALSE.
      END IF

*  If we still havn't got a good estimate of the output image size, try
*  again using more test points.
      IF( .NOT. OK ) THEN

*  Increase the number of test points on each side.
         NSIDE = 2*NSIDE - 1

*  If the maximum no. of test points has not yet been reached, jump
*  back to produce a new estimate of the output image size.
         IF( NSIDE .LE. MAXSID .AND. STATUS .EQ. SAI__OK ) GO TO 10

*  If the maximum no. of test points has been reached, report an error.
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SALIA2_ERR1',
     :     'SALIA2: Unable to find default bounds for the output image',
     :                    STATUS )
         END IF

      END IF

      END
