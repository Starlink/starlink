      SUBROUTINE PREPC1( NX, NY, IN, XFLIP, YFLIP, SCALE, ZERO, OUT,
     :                   STATUS )
*+
*  Name:
*     PREPC1

*  Purpose:
*     Copy a 2-d data array from input to output.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPC1( NX, NY, IN, XFLIP, YFLIP, SCALE, ZERO, OUT, STATUS )

*  Description:
*     This subroutine copies the data from an input 2-d array to a
*     output 2-d array. The axes may be flipped as required and the
*     data value will be scaled and added a constant.

*  Arguments:
*     NX = INTEGER (Given)
*        The size of the first dimension.
*     NY = INTEGER (Given)
*        The size of the second dimension.
*     IN( NX, NY ) = REAL (Given)
*        The input data array.
*     XFLIP = LOGICAL (Given)
*        Flipping X axis flag.
*     YFLIP = LOGICAL (Given)
*        Flipping Y axis flag.
*     SCALE = REAL (Given)
*        Scale applied to the data array.
*     ZERO = REAL (Given)
*        Constant added to the data array.
*     OUT( NX, NY ) = REAL (Returned)
*        The output data array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JUL-1992 (WG):
*        Original version.
*     4-DEC-1992 (DSB):
*        Name changed from CPYDAT to PREPC1, etc.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Primitive constants

*  Arguments Given:
      INTEGER NX
      INTEGER NY
      REAL IN( NX, NY )
      LOGICAL XFLIP
      LOGICAL YFLIP
      REAL SCALE
      REAL ZERO

*  Arguments Returned:
      REAL OUT( NX, NY )

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER I                  ! Do loop index.
      INTEGER J                  ! Do loop index.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If no flipping is required, copy data to the output array directly.
      IF( .NOT.XFLIP .AND. .NOT.YFLIP ) THEN
         DO J = 1, NY
            DO I = 1, NX

*  Only apply scale and zero to valid pixels and keep bad pixels as bad.
               IF( IN( I, J ) .NE. VAL__BADR ) THEN
                  OUT( I, J ) = SCALE*IN( I, J ) + ZERO
               ELSE
                  OUT( I, J ) = VAL__BADR
               END IF

            END DO
         END DO

*  If X axis need to be flipped but not Y axis, ...
      ELSE IF( XFLIP .AND. .NOT.YFLIP ) THEN
         DO J = 1, NY
            DO I = 1, NX

*  Only apply scale and zero to valid pixels and keep bad pixels as bad.
               IF( IN( NX - I + 1 , J ) .NE. VAL__BADR ) THEN
                  OUT( I, J ) = SCALE*IN( NX - I + 1, J )
     :                             + ZERO
               ELSE
                  OUT( I, J ) = VAL__BADR
               END IF

            END DO
         END DO

*  If Y axis need to be flipped but not X axis, ...
      ELSE IF( .NOT.XFLIP .AND. YFLIP ) THEN
         DO J = 1, NY
            DO I = 1, NX

*  Only apply scale and zero to valid pixels and keep bad pixels as bad.
               IF( IN( I, NY - J + 1 ) .NE. VAL__BADR ) THEN
                  OUT( I, J ) = SCALE*IN( I, NY - J + 1 )
     :                             + ZERO
               ELSE
                  OUT( I, J ) = VAL__BADR
               END IF

            END DO
         END DO

*  If both axis need to be flipped, ...
      ELSE IF( XFLIP .AND. YFLIP ) THEN
         DO J = 1, NY
            DO I = 1, NX

*  Only apply scale and zero to valid pixels and keep bad pixels as bad.
               IF( IN( NX - I + 1, NY - J + 1 ) .NE. VAL__BADR ) THEN
                  OUT( I, J ) = SCALE*IN( NX - I + 1, NY - J + 1 )
     :                          + ZERO
               ELSE
                  OUT( I, J ) = VAL__BADR
               END IF

            END DO
         END DO

      END IF

      END
