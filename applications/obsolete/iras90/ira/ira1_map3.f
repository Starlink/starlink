      SUBROUTINE IRA1_MAP3( N, DIST, X, Y, STATUS )
*+
*  Name:
*     IRA1_MAP3

*  Purpose:
*     Find image coordinates at a given offset along a section of a
*     parallel.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRA1_MAP3( N, DIST, X, Y, STATUS )

*  Description:
*     The supplied offsets (normalised to the range [0,1]) are converted
*     into sky longitude and latitude values, and then these sky
*     coordinates are mapped into image coordinates. This routine uses
*     information set up previously and stored in common.

*  Arguments:
*     N = INTEGER (Given)
*        The number of offsets to process.
*     DIST( N ) = REAL (Given)
*        The normalised offsets at each required point along the
*        parallel. Zero corresponds to the start of the parallel
*        section, and one corresponds to the end.
*     X( N ) = REAL (Returned)
*        The X image coordinate (in single precision) at each point
*        along the curve.
*     Y( N ) = REAL (Returned)
*        The Y image coordinate (in single precision) at each point
*        along the curve.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-MAR-1992 (DSB):
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
      INCLUDE 'IRA_PAR'          ! IRA constants

*  Global Variables:
      INCLUDE 'IRA_COM'          ! IRA common blocks.
*        ACM_M3A0 = DOUBLE PRECISION (Read)
*           The sky longitude at the start of the parallel.
*        ACM_M3B0 = DOUBLE PRECISION (Read)
*           The sky latitude at the start of the parallel.
*        ACM_M3ID = INTEGER (Read)
*           The IRA identifier for the astrometry information.
*        ACM_M3IN = DOUBLE PRECISION (Read)
*           Increment in latitude along the curve.
*        ACM_M3SC = CHARACTER (Read)
*           The Sky Coordinate System to use.

*  Arguments Given:
      INTEGER N
      REAL DIST( N )

*  Arguments Returned:
      REAL X( N )
      REAL Y( N )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      REAL VAL_DTOR              ! Double precision to real conversion.

*  Local Constants:
      INTEGER SIZE               ! Max. value of N which can be coped
                                 ! with.
      PARAMETER ( SIZE = 2 )

*  Local Variables:
      DOUBLE PRECISION A( SIZE ) ! Sky longitude at each point along the
                                 ! curve.
      DOUBLE PRECISION B( SIZE ) ! Sky latitude at each point along the
                                 ! curve.
      DOUBLE PRECISION D         ! Longitude offset corresponding to
                                 ! the supplied normalised offset.
      INTEGER          I         ! Loop count.
      DOUBLE PRECISION XX( SIZE )! X image coordinate at each point
                                 ! along the curve.
      DOUBLE PRECISION YY( SIZE )! Y image coordinate at each point
                                 ! along the curve.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  For efficiency, all points are transformed from sky to image
*  coordinates in a single call to IRA_TRANS. This makes it necessary
*  to store the sky coordinates of each required point along the curve
*  in local arrays. Check that these local arrays are big enough.
      IF( N .GT. SIZE ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'N', N )
         CALL ERR_REP( 'IRA1_MAP3_ERR1',
     :'IRA1_MAP3: Insufficient local storage to map ^N points.',
     :                 STATUS )
         GO TO 999
      END IF

*  Loop round each offset along the curve.
      DO I = 1, N

*  Convert the normalised offset in the range [0,1] to an longitude
*  increment in radians.
         D = DBLE( DIST( I ) ) * ACM_M3IN

*  Find the sky coordinates at this offset from the start of the
*  curve.
         A( I ) = ACM_M3A0 + D
         B( I ) = ACM_M3B0

      END DO

*  Map all the positions into image coordinates.
      CALL IRA_TRANS( 2, A, B, .FALSE., ACM_M3SC, ACM_M3ID, XX, YY,
     :                STATUS )

*  Convert the double precision image coordinates to single precision
*  world coordinates, checking for bad values.
      DO I = 1, N
         X( I ) = VAL_DTOR( .TRUE., XX( I ), STATUS )
         Y( I ) = VAL_DTOR( .TRUE., YY( I ), STATUS )
      END DO

 999  CONTINUE

      END
