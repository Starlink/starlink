      SUBROUTINE IRA1_SCAC( IDA, X0, Y0, XBOX, YBOX, MMAX, NMAX, M, N,
     :                      BOX, CACHE, STATUS )
*+
*  Name:
*     IRA1_SCAC

*  Purpose:
*     Set up the cache for the next bad box.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_SCAC( IDA, X0, Y0, XBOX, YBOX, MMAX, NMAX, M, N, BOX,
*                     CACHE, STATUS )

*  Description:
*     The bad/good status of each box adjacent to the current bad box is
*     stored in the returned cache. Some of these values have to be
*     obtained by application of the projection to the image coordinates
*     at the centre of the box, but some are already known and stored in
*     the input cache. The re-positioning of values within the cache
*     depends on the relative positioning of the current bad box and the
*     previous bad box, and information describing this re-positioning
*     for each of the eight possibilities is set up by data statements.
*     Boxes are also considered bad if they are on the edge of the
*     image.

*  Arguments:
*     IDA = INTEGER (Given)
*        An IRA identifier for the astrometry information.
*     X0 = REAL (Given)
*        The X image coordinate of the centre of the current bad box.
*     Y0 = REAL (Given)
*        The Y image coordinate of the centre of the current bad box.
*     XBOX = REAL (Given)
*        The size in X of a box.
*     YBOX = REAL (Given)
*        The size in Y of a box.
*     MMAX = INTEGER (Given)
*        The number of boxes across the image in the X direction.
*     NMAX = INTEGER (Given)
*        The number of boxes across the image in the Y direction.
*     M = INTEGER (Given)
*        First index of the current bad box.
*     N = INTEGER (Given)
*        Second index of the current bad box.
*     BOX = INTEGER (Given and Returned)
*        On entry, the box number of the current bad box relative to the
*        previous bad box. On exit, the box number of the previous bad
*        box relative to the current bad box.
*     CACHE( 8 ) = LOGICAL(Given and Returned)
*        On entry, the bad/good status of each box adjacent to the
*        previous bad box.  On exit, the bad/good status of each box
*        adjacent to the current bad box. A true value is stored in the
*        centre of the box corresponds to a valid sky position and is
*        well away from the edge of the image.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAR-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants

*  Arguments Given:
      INTEGER IDA
      REAL X0
      REAL Y0
      REAL XBOX
      REAL YBOX
      INTEGER MMAX
      INTEGER NMAX
      INTEGER M
      INTEGER N

*  Arguments Given and Returned:
      INTEGER BOX
      LOGICAL CACHE( 8 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL BUF( 8 )
      INTEGER C
      INTEGER CODE( 8, 8 )
      INTEGER DM( 8 )
      INTEGER DN( 8 )
      INTEGER I
      LOGICAL INSIDE( 8 )
      INTEGER K
      INTEGER KCALC( 8 )
      INTEGER NCALC
      INTEGER NEWBOX( 8 )
      LOGICAL OK( 8 )
      REAL XB
      DOUBLE PRECISION XX( 8 )
      REAL YB
      DOUBLE PRECISION YY( 8 )

*  Local Data:
      DATA CODE   / 0, 0, 2, 3, -1, 7, 8, 0,
     :              0, 0, 0, 0, 3, -1, 1, 0,
     :              2, 0, 0, 0, 4, 5, -1, 1,
     :              3, 0, 0, 0, 0, 0, 5, -1,
     :              -1, 3, 4, 0, 0, 0, 6, 7,
     :              7, -1, 5, 0, 0, 0, 0, 0,
     :              8, 1, -1, 5, 6, 0, 0, 0,
     :              0, 0, 1, -1, 7, 0, 0, 0/,
     :     DM     / 0, 1, 1, 1, 0, -1, -1, -1 /,
     :     DN     / 1, 1, 0, -1, -1, -1, 0, 1 /,
     :     NEWBOX / 5, 6, 7, 8, 1, 2, 3, 4 /

*  Set up statement functions giving the image coordinates of the centre
*  of a box specified by a relative box number.
      XB( K ) = X0 + REAL( DM( K ) )*XBOX
      YB( K ) = Y0 + REAL( DN( K ) )*YBOX
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise flags indicating that no boxes are at the edge of the
*  plotting space.
      DO K = 1, 8
         INSIDE( K ) = .TRUE.
      END DO

*  If the current centre box is on the edge of the plotting space,
*  modify the flags just initialised.
      IF( M .EQ. 1 ) THEN
         INSIDE( 5 ) = .FALSE.
         INSIDE( 6 ) = .FALSE.
         INSIDE( 7 ) = .FALSE.
         INSIDE( 8 ) = .FALSE.
         INSIDE( 1 ) = .FALSE.

      ELSE IF( M .EQ. 2 ) THEN
         INSIDE( 6 ) = .FALSE.
         INSIDE( 7 ) = .FALSE.
         INSIDE( 8 ) = .FALSE.

      ELSE IF( M .EQ. MMAX - 1 ) THEN
         INSIDE( 2 ) = .FALSE.
         INSIDE( 3 ) = .FALSE.
         INSIDE( 4 ) = .FALSE.

      ELSE IF( M .EQ. MMAX ) THEN
         INSIDE( 1 ) = .FALSE.
         INSIDE( 2 ) = .FALSE.
         INSIDE( 3 ) = .FALSE.
         INSIDE( 4 ) = .FALSE.
         INSIDE( 5 ) = .FALSE.

      END IF

      IF( N .EQ. 1 ) THEN
         INSIDE( 3 ) = .FALSE.
         INSIDE( 4 ) = .FALSE.
         INSIDE( 5 ) = .FALSE.
         INSIDE( 6 ) = .FALSE.
         INSIDE( 7 ) = .FALSE.

      ELSE IF( N .EQ. 2 ) THEN
         INSIDE( 4 ) = .FALSE.
         INSIDE( 5 ) = .FALSE.
         INSIDE( 6 ) = .FALSE.

      ELSE IF( N .EQ. NMAX - 1 ) THEN
         INSIDE( 8 ) = .FALSE.
         INSIDE( 1 ) = .FALSE.
         INSIDE( 2 ) = .FALSE.

      ELSE IF( N .EQ. NMAX ) THEN
         INSIDE( 7 ) = .FALSE.
         INSIDE( 8 ) = .FALSE.
         INSIDE( 1 ) = .FALSE.
         INSIDE( 2 ) = .FALSE.
         INSIDE( 3 ) = .FALSE.

      END IF

*  If no previous cache is defined, explicitly check each box to see if
*  it bad.
      IF( BOX .EQ. 0) THEN
         DO K = 1, 8
            XX( K ) = DBLE( XB( K ) )
            YY( K ) = DBLE( YB( K ) )
         END DO
         CALL IRA_VALID( 8, .TRUE., ' ', IDA, XX, YY, OK, STATUS )

*  If a previous cache is defined, the members of the old cache which
*  form part of the new cache must be found and moved to their new
*  positions within the cache. Consider each box in turn.
      ELSE

         NCALC = 0
         DO K = 1, 8

*   A set of codes determining where the value to be stored in the
*   output cache should come from is used.
            C = CODE( K, BOX )

*  If this box is the previous centre box, it is known to be bad.
            IF( C .EQ. -1 ) THEN
               OK( K ) = .FALSE.

*  If this box was not present in the old cache, add it to the list of
*  boxes to be explicitly checked.
            ELSE IF( C .EQ. 0 ) THEN
               NCALC = NCALC + 1
               KCALC( NCALC ) = K
               XX( NCALC ) = DBLE( XB( K ) )
               YY( NCALC ) = DBLE( YB( K ) )

*  Otherwise, if the required value was already present in the old
*  cache, get it from the old cache.
            ELSE
               OK( K ) = CACHE( C )

            END IF

         END DO

*  Perform the explicit checks, and store the results.
         CALL IRA_VALID( NCALC, .TRUE., ' ', IDA, XX, YY, BUF, STATUS )

         DO I = 1, NCALC
            K = KCALC( I )
            OK( K ) = BUF( I )
         END DO

*  Store the box number of the previous centre with respect to the
*  current centre.
         BOX =  NEWBOX( BOX )

      END IF

*  Set up the returned cache, by combining bad flags and edge flags.
      DO K = 1, 8
         CACHE( K ) = OK( K ) .AND. INSIDE( K )
      END DO

      END
