      SUBROUTINE ARY_SHIFT( NSHIFT, SHIFT, IARY, STATUS )
*+
*  Name:
*     ARY_SHIFT

*  Purpose:
*     Apply pixel-index shifts to an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_SHIFT( NSHIFT, SHIFT, IARY, STATUS )

*  Description:
*     The routine applies pixel-index shifts to an array. An integer
*     shift is applied to each dimension so that its pixel-index
*     bounds, and the indices of each pixel, change by the amount of
*     shift applied to the corresponding dimension. The array's pixels
*     retain their values and none are lost.

*  Arguments:
*     NSHIFT = INTEGER (Given)
*        Number of dimensions to which shifts are to be applied. This
*        must not exceed the number of array dimensions. If fewer
*        shifts are supplied than there are dimensions in the array,
*        then the extra dimensions will not be shifted.
*     SHIFT( NSHIFT ) = INTEGER (Given)
*        The pixel-index shifts to be applied to each dimension.
*     IARY = INTEGER (Given)
*        Array identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Pixel-index shifts applied to a base array will affect the
*     appearance of that array as seen by all base-array identifiers
*     associated with it. However, array sections derived from that
*     base array will remain unchanged (as regards both pixel-indices
*     and data content).
*     -  Pixel-index shifts cannot be applied to a base array while any
*     part of it is mapped for access (i.e. even through another
*     identifier).
*     -  Pixel-index shifts applied to an array section only affect
*     that section itself, and have no effect on other array
*     identifiers.
*     -  Pixel-index shifts cannot be applied to an array section while
*     it is mapped for access through the identifier supplied to this
*     routine.

*  Algorithm:
*     -  Check that the number of pixel shifts specified is positive and
*     report an error if it is not.
*     -  Import the array identifier.
*     -  Check that the number of pixel shifts specified does not exceed
*     the number of array dimensions and report an error if it does.
*     -  Apply the shifts to the array.
*     -  If an error occurred, then report context information.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-AUG-1989 (RFWS):
*        Original version.
*     15-SEP-1989 (RFWS):
*        Added check that SHIFT access to the array is available.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants
      INCLUDE 'ARY_ERR'          ! ARY_ error codes

*  Global Variables:
      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of array dimensions.

*  Arguments Given:
      INTEGER NSHIFT
      INTEGER SHIFT( * )
      INTEGER IARY

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IACB               ! Index to array entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the number of shifts specified is not positive, then report an
*  error.
      IF ( NSHIFT .LE. 0 ) THEN
         STATUS = ARY__SFTIN
         CALL MSG_SETI( 'BADNSFT', NSHIFT )
         CALL ERR_REP( 'ARY_SHIFT_NSLO',
     :   'Invalid number of shifts (^BADNSFT) specified (possible ' //
     :   'programming error).', STATUS )

*  Import the array identifier.
      ELSE
         CALL ARY1_IMPID( IARY, IACB, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Check that the number of shifts does not exceed the number of array
*  dimensions and report an error if it does.
            IF ( NSHIFT .GT. ACB_NDIM( IACB ) ) THEN
               STATUS = ARY__SFTIN
               CALL MSG_SETI( 'BADNSFT', NSHIFT )
               CALL MSG_SETI( 'NDIM', ACB_NDIM( IACB ) )
               CALL ERR_REP( 'ARY_SHIFT_NSHI',
     :         'Number of shifts specified (^BADNSFT) exceeds the ' //
     :         'number of array dimensions (^NDIM) (possible ' //
     :         'programming error).', STATUS )

*  Check that SHIFT access to the array is available.
            ELSE
               CALL ARY1_CHACC( IACB, 'SHIFT', STATUS )

*  Apply the shifts to the array's ACB entry.
               CALL ARY1_SFT( NSHIFT, SHIFT, IACB, STATUS )
            END IF
         END IF
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_SHIFT_ERR',
     :   'ARY_SHIFT: Error applying pixel-index shifts to an array.',
     :   STATUS )
         CALL ARY1_TRACE( 'ARY_SHIFT', STATUS )
      END IF

      END
