      SUBROUTINE ARY_BOUND( IARY, NDIMX, LBND, UBND, NDIM, STATUS )
*+
*  Name:
*     ARY_BOUND

*  Purpose:
*     Enquire the pixel-index bounds of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY_BOUND( IARY, NDIMX, LBND, UBND, NDIM, STATUS )

*  Description:
*     The routine returns the lower and upper pixel-index bounds of
*     each dimension of an array, together with the total number of
*     dimensions.

*  Arguments:
*     IARY = INTEGER (Given)
*        Array identifier.
*     NDIMX = INTEGER (Given)
*        Maximum number of pixel-index bounds to return (i.e. the
*        declared size of the LBND and UBND arguments).
*     LBND( NDIMX ) = INTEGER (Returned)
*        Lower pixel-index bounds for each dimension.
*     UBND( NDIMX ) = INTEGER (Returned)
*        Upper pixel-index bounds for each dimension.
*     NDIM = INTEGER (Returned)
*        Total number of array dimensions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  If the array has fewer than NDIMX dimensions, then any
*     remaining elements of the LBND and UBND arguments will be filled
*     with 1's.
*     -  If the array has more than NDIMX dimensions, then the NDIM
*     argument will return the actual number of dimensions. In this
*     case only the first NDIMX sets of bounds will be returned, and an
*     error will result if the size of any of the remaining dimensions
*     exceeds 1.
*     -  The symbolic constant ARY__MXDIM may be used to declare the
*     size of the LBND and UBND arguments so that they will be able to
*     hold the maximum number of array bounds that this routine can
*     return. This constant is defined in the include file ARY_PAR.

*  Algorithm:
*     -  Import the array identifier.
*     -  Return the number of array dimensions from the Access Control
*     Block.
*     -  Return as many array bounds values as possible.
*     -  If any elements of the LBND and UBND arguments remain unused,
*     then fill them with 1's.
*     -  If not all the bounds could be returned, then check that the
*     size of any excluded dimensions does not exceed 1. Report an
*     error if any dimension size does.
*     -  If an error occurred, then report context information.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JUL-1989 (RFWS):
*        Original version.
*     5-MAR-1990 (RFWS):
*        Added code to pad returned bounds with 1's and to check for
*        significant excluded dimensions.
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
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block
*        ACB_LOC( ARY__MXDCB ) = CHARACTER * ( DAT__SZLOC ) (Read)
*           Data object locator.

      INCLUDE 'ARY_ACB'          ! ARY_ Access Control Block
*        ACB_IDCB( ARY__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.
*        ACB_LBND( ARY__MXACB, ARY__MXACB ) = INTEGER (Read)
*           Lower bounds of array.
*        ACB_NDIM( ARY__MXACB ) = INTEGER (Read)
*           Number of array dimensions.
*        ACB_UBND( ARY__MXDIM, ARY__MXACB ) = INTEGER (Read)
*           Upper bounds of array.

*  Arguments Given:
      INTEGER IARY
      INTEGER NDIMX

*  Arguments Returned:
      INTEGER LBND( NDIMX )
      INTEGER UBND( NDIMX )
      INTEGER NDIM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter for dimensions
      INTEGER IACB               ! Index to array entry in the ACB
      INTEGER IDCB               ! Index to data object entry in the DCB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the array identifier.
      CALL ARY1_IMPID( IARY, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Return the number of array dimensions.
         NDIM = ACB_NDIM( IACB )

*  Return as many array bounds values as possible.
         DO 1 I = 1, MIN( NDIMX, NDIM )
            LBND( I ) = ACB_LBND( I, IACB )
            UBND( I ) = ACB_UBND( I, IACB )
1        CONTINUE

*  Pad any remaining elements in the LBND and UBND arguments with 1's.
         DO 2 I = MIN( NDIMX, NDIM ) + 1, NDIMX
            LBND( I ) = 1
            UBND( I ) = 1
2        CONTINUE

*  If the array has more dimensions than there are elements in the LBND
*  and UBND arguments, then check that the size of all remaining
*  dimensions is 1.
         DO 3 I = MIN( NDIMX, NDIM ) + 1, NDIM

*  Report an error if any significant dimensions have been excluded.
            IF ( ACB_UBND( I, IACB ) .NE. ACB_LBND( I, IACB ) ) THEN
               STATUS = ARY__XSDIM
               IDCB = ACB_IDCB( IACB )
               CALL DAT_MSG( 'ARRAY', DCB_LOC( IDCB ) )
               CALL MSG_SETI( 'NDIMX', NDIMX )
               CALL ERR_REP( 'ARY_BOUND_NDIM',
     :         'The array structure ^ARRAY has more than ^NDIMX ' //
     :         'significant dimension(s).', STATUS )
               GO TO 4
            END IF
3        CONTINUE
4        CONTINUE
      END IF
       
*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARY_BOUND_ERR',
     :   'ARY_BOUND: Error obtaining the pixel-index bounds of an ' //
     :   'array.', STATUS )
         CALL ARY1_TRACE( 'ARY_BOUND', STATUS )
      END IF

      END
