      SUBROUTINE PREPB9( INDF1, INDF2, SCALE, ZERO, XFLIP, YFLIP,
     :                   STATUS )
*+
*  Name:
*     PREPB9

*  Purpose:
*     Scale and flip the input image to create the output image.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPB9( INDF1, INDF2, SCALE, ZERO, XFLIP, YFLIP, STATUS )

*  Description:
*     This routine maps the DATA arrays of the input and output NDFs and
*     then calls a routine to flip them in either axis so that rotation
*     from north to east appears anti-clockwise when the outout image is
*     displayed normally. It also applies the scale factor and zero
*     offset to create the output data values.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        An identifier for the input NDF.
*     INDF2 = INTEGER (Given)
*        An identifier for the output NDF.
*     SCALE = REAL (Given)
*        The scale factor for converting value from the input NDF to
*        output values.
*     ZERO = REAL (Given)
*        The zero offfset for converting value from the input NDF to
*        output values.
*     XFLIP = LOGICAL (Given)
*        True if the image is to be flipped in the X direction.
*     YFLIP = LOGICAL (Given)
*        True if the image is to be flipped in the Y direction.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2
      REAL SCALE
      REAL ZERO
      LOGICAL XFLIP
      LOGICAL YFLIP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER DIM( 2 )           ! NDF dimensions.
      INTEGER EL                 ! No. of elements in the NDFs.
      INTEGER NDIM               ! No. of dimensions in the NDFs.
      INTEGER PNT1               ! Pointer to mapped i/p data array.
      INTEGER PNT2               ! Pointer to mapped o/p data array.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the size of the data array dimensions.
      CALL NDF_DIM( INDF1, 2, DIM, NDIM, STATUS )

*  Map the data arrays.
      CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', PNT1, EL, STATUS )
      CALL NDF_MAP( INDF2, 'DATA', '_REAL', 'WRITE', PNT2, EL, STATUS )

*  Do the flipping,  scaling and adding constant as required.
      CALL PREPC1( DIM( 1 ), DIM( 2 ), %VAL( PNT1 ), XFLIP, YFLIP,
     :             SCALE, ZERO, %VAL( PNT2 ), STATUS )

*  Unmap the data arrays.
      CALL NDF_UNMAP( INDF1, 'DATA', STATUS )
      CALL NDF_UNMAP( INDF2, 'DATA', STATUS )

      END
