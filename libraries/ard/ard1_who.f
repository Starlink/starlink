      SUBROUTINE ARD1_WHO( RINDEX, NDIM, MSKSIZ, B, LBEXTB, UBEXTB,
     :                     LBINTB, UBINTB, STATUS )
*+
*  Name:
*     ARD1_WHO

*  Purpose:
*     Initialise an array to hold a WHOLE region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_WHO( RINDEX, NDIM, MSKSIZ, B, LBEXTB, UBEXTB, LBINTB,
*                    UBINTB, STATUS )

*  Description:
*     The array B is filled with interior values. The interior bounding
*     box is returned "infinite" and the exterior bounding box is
*     returned "null".

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the mask.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     B( MSKSIZ ) = INTEGER (Given and Returned)
*        The array.
*     LBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        exterior points in B. A value of VAL__MAXI for element 1 is
*        returned to indicate an "infinite" box. Other elements should
*        be ignored.
*     UBEXTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        exterior points in B. The returned values should be ignored
*        since the box is "infinite".
*     LBINTB( NDIM ) = INTEGER (Given and Returned)
*        The lower pixel bounds of the smallest box which contains all
*        interior points in B. A value of VAL__MAXI for element 1 is
*        used to indicate an infinite box, and a value of VAL__MINI for
*        element 1 is used to indicate a zero sized box.
*     UBINTB( NDIM ) = INTEGER (Given and Returned)
*        The upper pixel bounds of the smallest box which contains all
*        interior points in B. 
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-APR-1994 (DSB):
*        Original version.
*     26-JUN-2001 (DSB):
*        Modified for ARD version 2.0.
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
      INTEGER RINDEX
      INTEGER NDIM
      INTEGER MSKSIZ

*  Arguments Given and Returned:
      INTEGER B( MSKSIZ )
      INTEGER LBEXTB( NDIM )
      INTEGER UBEXTB( NDIM )
      INTEGER LBINTB( NDIM )
      INTEGER UBINTB( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        I                  ! Loop count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill the array with interior values.
      DO I = 1, MSKSIZ
         B( I ) = RINDEX
      END DO

*  Return VAL__MAXI for LBINTB( 1 ) to indicate that the interior
*  bounding box is infinite.
      LBINTB( 1 ) = VAL__MAXI

*  Return VAL__MINI for LBEXTB( 1 ) to indicate that the exterior
*  bounding box is null.
      LBEXTB( 1 ) = VAL__MINI

      END
