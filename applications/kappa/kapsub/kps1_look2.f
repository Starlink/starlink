      SUBROUTINE KPS1_LOOK2( XLO, XHI, YLO, YHI, MASK, NOVAL, IN, 
     :                       OUT, STATUS )
*+
*  Name:
*     KPS1_LOOK2

*  Purpose:
*     Copies a 2D masked region of an NDF into an output array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_LOOK2( XLO, XHI, YLO, YHI, MASK, NOVAL, IN, OUT, STATUS )

*  Description:
*     This routine copies the input array to the output array,
*     substituting the value NOVAL for each pixel which has a zero 
*     value in the supplied mask array.

*  Arguments:
*     XLO = INTEGER (Given)
*        The lower X pixel index bound of the arrays.
*     XHI = INTEGER (Given)
*        The upper X pixel index bound of the arrays.
*     YLO = INTEGER (Given)
*        The lower Y pixel index bound of the arrays.
*     YHI = INTEGER (Given)
*        The upper Y pixel index bound of the arrays.
*     MASK( XLO:XHI, YLO:YHI ) = INTEGER (Given)
*        A mask.
*     NOVAL = DOUBLE PRECISION (Given)
*        The value to place in OUT for each pixel which has a zero value
*        in MASK.
*     IN( XLO:XHI, YLO:YHI ) = DOUBLE PRECISION (Given)
*        The input array.
*     OUT( XLO:XHI, YLO:YHI ) = DOUBLE PRECISION (Returned)
*        The output array.
*     STATUS = INTEGER (Given)
*        Global status value.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     22-OCT-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE            

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER XLO
      INTEGER XHI
      INTEGER YLO
      INTEGER YHI
      INTEGER MASK( XLO:XHI, YLO:YHI )
      DOUBLE PRECISION NOVAL
      DOUBLE PRECISION IN( XLO:XHI, YLO:YHI )

*  Arguments Returned:
      DOUBLE PRECISION OUT( XLO:XHI, YLO:YHI )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count
      INTEGER J                  ! Loop count
*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just do it.
      DO J = YLO, YHI
         DO I = XLO, XHI
            IF( MASK( I, J ) .GT. 0 ) THEN
               OUT( I, J ) = IN( I, J )
            ELSE
               OUT( I, J ) = NOVAL
            END IF
         END DO
      END DO

      END
