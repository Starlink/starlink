      SUBROUTINE SALIA6( IDAB, IDAA, SCS, LBNDX, UBNDX, LBNDY, UBNDY,
     :                   IB1, IB2, JB1, JB2, XAMAP, YAMAP, XA, YA, XB,
     :                   YB, NK, ICOL, JROW, NBAD, STATUS )
*+
*  Name:
*     SALIA6

*  Purpose:
*     Find the input and output image coordinates of a grid of nine
*     test points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SALIA6( IDAB, IDAA, SCS, LBNDX, UBNDX, LBNDY, UBNDY, IB1,
*                  IB2, JB1, JB2, XAMAP, YAMAP, XA, YA, XB, YB, NK,
*                  ICOL, JROW, NBAD, STATUS )

*  Description:
*     This routine sets up a grid of nine test points (3 rows of 3
*     points) evenly spaced in the section (IB1:IB2,JB1:JB2) of the
*     output image. It uses the full projection mappings to calculate
*     the corresponding input image coordinates (unless they have alrady
*     benn calculated and stored in XAMAP and YAMAP), and stores them in
*     XAMAP and YAMAP. It also returns the coordinates of the test
*     points in XA, YA, XB and YB. Depending on the projections
*     involved, some of the test points may not correspond to valid
*     positions in the input pixel grid. Such test points are returned
*     with bad coordinates. If the size of the section is too small
*     some of the test points may be coincident. Each distinct test
*     point is only included once in the output arrays. NK returns the
*     number of ddistinct test points calculated.

*  Arguments:
*     IDAB = INTEGER (Given)
*        IRA identifier for the astrometry information defining the
*        output (reference) pixel grid.
*     IDAA = INTEGER (Given)
*        IRA identifier for the astrometry information defining the
*        input pixel grid.
*     SCS = CHARACTER * ( * ) (Given)
*        The sky coordinate system used by the reference astrometry
*        information.
*     LBNDX = INTEGER (Given)
*        The lower X bound of the output image.
*     UBNDX = INTEGER (Given)
*        The upper X bound of the output image.
*     LBNDY = INTEGER (Given)
*        The lower Y bound of the output image.
*     UBNDY = INTEGER (Given)
*        The upper Y bound of the output image.
*     IB1 = INTEGER (Given)
*        The lower X bound of the section to be filled.
*     IB2 = INTEGER (Given)
*        The upper X bound of the section to be filled.
*     JB1 = INTEGER (Given)
*        The lower Y bound of the section to be filled.
*     JB2 = INTEGER (Given)
*        The upper Y bound of the section to be filled.
*     XAMAP( LBNDX:UBNDX, LBNDY:UBNDY ) = REAL (Returned)
*        Holds the input X image coordinate corresponding to the centre
*        of each output pixel.
*     YAMAP( LBNDX:UBNDX, LBNDY:UBNDY ) = REAL (Returned)
*        Holds the input Y image coordinate corresponding to the centre
*        of each output pixel.
*     XA( 9 ) = REAL( Returned)
*        The input X image coordinate of each test point.
*     YA( 9 ) = REAL( Returned)
*        The input Y image coordinate of each test point.
*     XB( 9 ) = REAL( Returned)
*        The output X image coordinate of each test point.
*     YB( 9 ) = REAL( Returned)
*        The output Y image coordinate of each test point.
*     NK = INTEGER (Returned)
*        The actual no. of test points returned in XA, YA, XB and YB.
*     ICOL( 3 ) = INTEGER (Returned)
*        The output column numbers at which the three columns of test
*        points are placed. ICOL( 1 ) and ICOL( 2 ) may be equal.
*     JROW( 3 ) = INTEGER (Returned)
*        The output row numbers at which the three rows of test
*        points are placed. JROW( 1 ) and JROW( 2 ) may be equal.
*     NBAD = INTEGER (Returned)
*        The number of test points with bad input image coordinates.
*        These are included in the NK values returned in XA, YA, etc.
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
      INTEGER IDAB
      INTEGER IDAA
      CHARACTER SCS*(*)
      INTEGER LBNDX
      INTEGER UBNDX
      INTEGER LBNDY
      INTEGER UBNDY
      INTEGER IB1
      INTEGER IB2
      INTEGER JB1
      INTEGER JB2

*  Arguments Given and Returned:
      REAL XAMAP( LBNDX:UBNDX, LBNDY:UBNDY )
      REAL YAMAP( LBNDX:UBNDX, LBNDY:UBNDY )

*  Arguments Returned:
      REAL XA( 9 )
      REAL YA( 9 )
      REAL XB( 9 )
      REAL YB( 9 )
      INTEGER NK
      INTEGER ICOL( 3 )
      INTEGER JROW( 3 )
      INTEGER NBAD

*  Status:
      INTEGER STATUS          ! Global status

*  Local Variables:
      DOUBLE PRECISION
     :        AA( 9 ),        ! Sky longitude of test points.
     :        BB( 9 ),        ! Sky latitude of test points.
     :        XX( 9 ),        ! X image coordinate of test points.
     :        YY( 9 )         ! Y image coordinate of test points.

      INTEGER
     :        COL,            ! Current column of test points.
     :        DX,             ! No. of columns between test points.
     :        DY,             ! No. of rows between test points.
     :        I(9),           ! Output X pixel index of each test point.
     :        II,             ! Output X pixel index.
     :        J(9),           ! Output Y pixel index of each test point.
     :        JJ,             ! Output Y pixel index.
     :        K,              ! Index of current test point.
     :        KK(9),          ! Index of test points to be transformed.
     :        L,              ! Index of current transformed test point.
     :        NL,             ! No. of test points not yet transformed.
     :        ROW             ! Current row of test points.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the number of columns of pixels between the left hand and
*  middle columns of test points.
      DX = ( IB2 - IB1 )/2

*  Set up the number of rows of pixels between the bottom and
*  middle rows of test points.
      DY = ( JB2 - JB1 )/2

*  Store the indices of the three columns of test points. Note, the left
*  hand and middle columns will be coincident if IB1 and IB2 are
*  adjacent.
      ICOL( 1 ) = IB1
      ICOL( 2 ) = IB1 + DX
      ICOL( 3 ) = IB2

*  Store the indices of the three rows of test points. Note, the bottom
*  and middle rows will be coincident if JB1 and JB2 are adjacent.
      JROW( 1 ) = JB1
      JROW( 2 ) = JB1 + DY
      JROW( 3 ) = JB2

*  Initialise the number of distinct test points to zero.
      NK = 0

*  Loop round each row of test points.
      DO ROW = 1, 3

*  Skip over the middle row if it coincident with the bottom row.
         IF( ROW .NE. 2 .OR. DY .NE. 0 ) THEN

*  Loop round each column of test points.
            DO COL = 1, 3

*  Skip over the middle column if it coincident with the left hand
*  column.
               IF( COL .NE. 2 .OR. DX .NE. 0 ) THEN

*  Increment the number of distinct test points, and store the X and Y
*  indices of the current test point.
                  NK = NK + 1
                  I( NK ) = ICOL( COL )
                  J( NK ) = JROW( ROW )

               END IF

            END DO

         END IF

      END DO

*  We now have lists of the pixel indices of the distinct test points.
*  See if any of these points have already been transformed. If they
*  have, return the coordinates previously stored in the X and Y maps.
*  Construct a list of test points for which no transformed coordinates
*  are yet available. First initialise the number of such test points to
*  zero.
      NL = 0

*  Loop round the distinct test points.
      DO K = 1, NK

*  Store the pixel indices of this test point.
         II = I( K )
         JJ = J( K )

*  Store the image coordinates of this test point. These refer to the
*  output pixel grid.
         XB( K ) = REAL( II ) - 0.5
         YB( K ) = REAL( JJ ) - 0.5

*  If the X and Y maps contain good values at this test point, store the
*  values as the input image coordinates of the test point.
         IF( XAMAP( II, JJ ) .NE. VAL__BADR ) THEN
            XA( K ) = XAMAP( II, JJ )
            YA( K ) = YAMAP( II, JJ )

*  If the input coordinates corresponding to the test point have not yet
*  been calculated, add the test point to the list of those to be
*  transformed.
         ELSE
            NL = NL + 1
            XX( NL ) = DBLE( XB( K ) )
            YY( NL ) = DBLE( YB( K ) )
            KK( NL ) = K
         END IF

      END DO

*  If the input image coordinates of any of the test points have not yet
*  been calculated, do it now using the full projection mappings.
      IF( NL .GT. 0 ) THEN

*  Convert output image coordinates to sky coordinates.
         CALL IRA_TRANS( NL, XX, YY, .TRUE., SCS, IDAB, AA, BB, STATUS )

*  Convert sky coordinates to input image coordinates.
         CALL IRA_TRANS( NL, AA, BB, .FALSE., SCS, IDAA, XX, YY,
     :                   STATUS )

*  Now store the input image coordinates in the returned arrays.
         NBAD = 0

         DO L = 1, NL
            K = KK( L )

            IF( XX( L ) .NE. VAL__BADD ) THEN
               XA( K ) = REAL( XX( L ) )
               YA( K ) = REAL( YY( L ) )
               XAMAP( I( K ), J( K ) ) = REAL( XX( L ) )
               YAMAP( I( K ), J( K ) ) = REAL( YY( L ) )

            ELSE
               NBAD = NBAD + 1
               XA( K ) = VAL__BADR
               YA( K ) = VAL__BADR
               XAMAP( I( K ), J( K ) ) = VAL__BADR
               YAMAP( I( K ), J( K ) ) = VAL__BADR

            END IF

         END DO

      END IF

      END
