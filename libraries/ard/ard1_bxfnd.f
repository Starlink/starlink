      SUBROUTINE ARD1_BXFND( NDIM, LBND, UBND, NPAR, PAR, LBINTB,
     :                       UBINTB, STATUS )
*+
*  Name:
*     ARD1_BXFND

*  Purpose:
*     Find the pixel index bounds of a n-D box with sides parallel to
*     the user axes.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_BXFND( NDIM, LBND, UBND, NPAR, PAR, LBINTB, UBINTB,
*                      STATUS )

*  Description:
*     The user co-ordinates of each corner of the box are found. These
*     are transformed into pixel indices and the supplied bounds of the
*     internal bounding box are updated to include all the corners.

*  Arguments:
*     NDIM = INTEGER (Given)
*        No. of dimensions.
*     LBND( NDIM ) = INTEGER (Given)
*        Lower bounds of mask.
*     UBND( NDIM ) = INTEGER (Given)
*        Upper bounds of mask.
*     NPAR = INTEGER (Given)
*        No. of values in PAR.
*     PAR( NPAR ) = REAL (Given)
*        Parameters; Coeffs of user coord.s to pixel coord.s
*        transformation, followed by user coords of box centre,
*        followed by length of each side of the box (in user coords).
*     LBINTB( NDIM ) = INTEGER (Returned)
*        The lower bounds of the internal bounding box.
*     UBINTB( NDIM ) = INTEGER (Returned)
*        The upper bounds of the internal bounding box.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-MAR-1994 (DSB):
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
      INCLUDE 'ARD_CONST'        ! ARD private constants

*  Arguments Given:
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER NPAR
      REAL PAR( NPAR )

*  Arguments Returned:
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :     CEN0,                 ! Offset to centre values
     :     I,                    ! Dimension counter
     :     IC,                   ! Corner counter
     :     PINDEX,               ! Pixel index value
     :     SIDE0                 ! Offset to side length values

      LOGICAL
     :     UPPER( ARD__MXDIM ),  ! At upper bound?
     :     CARRY                 ! Carry forward?

      REAL
     :     PIXCO( ARD__MXDIM ),  ! Pixel coordinates for current corner
     :     USERCO( ARD__MXDIM )  ! User coordinates for current corner

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Store the offsets (within PAR) to the centre co-ordinates, and
*  to the side lengths.
      CEN0 = NDIM*( 1 + NDIM )
      SIDE0 = NDIM*( 2 + NDIM )

*  Initialise the interior bounding box.
      DO I = 1, NDIM
         LBINTB( I ) = VAL__MAXI
         UBINTB( I ) = VAL__MINI
      END DO

*  Initialise a set of flags (one for each axis) indicating if the 
*  corresponding co-ordinate is at its upper or lower bound. Set all
*  axes to their lower bounds.
      DO I = 1, NDIM      
         UPPER( I ) = .FALSE.
         USERCO( I ) = PAR( CEN0 + I ) - 0.5*PAR( SIDE0 + I )
      END DO

*  Loop round each of the corners.
      DO IC = 1, 2**NDIM

*  Transform the current user position to pixel coordinates.
         CALL ARD1_TRANS( NDIM, PAR, USERCO, PIXCO )

*  Convert the pixel co-ordinates to pixel indices and update the upper
*  and lower bounds of the internal bounding box.
         DO I = 1, NDIM
            PINDEX = REAL( INT( PIXCO( I ) ) )
            IF( PINDEX .LT. PIXCO( I ) ) PINDEX = PINDEX + 1

            LBINTB( I ) = MIN( LBINTB( I ), PINDEX )
            UBINTB( I ) = MAX( UBINTB( I ), PINDEX )

         END DO

*  Change the flags to move on to another corner. Each flag is treated
*  as if it was one bit in a binary integer value (.TRUE. = 1,
*  .FALSE. = 0). This integer value is incremented by one on each pass
*  through the following code. This results in all possible combinations
*  of upper and lower bounds being used by the time IC=2**NDIM is
*  reached.
         I = 1
         CARRY = .TRUE.
         DO WHILE( CARRY .AND. I .LE. NDIM )

*  If this "bit" is set, clear it, and store the lower bound. Go on to
*  add the carry onto the next bit.
            IF( UPPER( I ) ) THEN
               UPPER( I ) = .FALSE.
               USERCO( I ) = PAR( CEN0 + I ) - 0.5*PAR( SIDE0 + I )
               I = I + 1

*  If this "bit" is clear, set it, and store the upper bound. Indicate
*  that no carry is required.
            ELSE
               UPPER( I ) = .TRUE.            
               USERCO( I ) = PAR( CEN0 + I ) + 0.5*PAR( SIDE0 + I )
               CARRY = .FALSE.

            END IF

         END DO

      END DO

*  Ensure that the returned bounding box does not exceed the bounds of
*  the mask.
      DO I = 1, NDIM
         LBINTB( I ) = MAX( LBINTB( I ), LBND( I ) )
         UBINTB( I ) = MIN( UBINTB( I ), UBND( I ) )

*  If the lower bound is higher than the upper bound, return with a 
*  null box
         IF( LBINTB( I ) .GT. UBINTB( I ) ) THEN
            LBINTB( 1 ) = VAL__MINI
            GO TO 999
         END IF

      END DO

 999  CONTINUE

      END
