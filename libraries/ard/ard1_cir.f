      SUBROUTINE ARD1_CIR( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, PAR,
     :                     B, LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )
*+
*  Name:
*     ARD1_CIR

*  Purpose:
*     Initialise an array to hold a CIRCLE region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_CIR( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, PAR, B,
*                    LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The array B is initialised by setting all values within the
*     supplied interior bounding box to the exterior value 0.
*     All points outside this box already hold exterior values.
*     Interior values are then assigned to the points specified by the
*     supplied parameters. The supplied parameters are the
*     co-efficients of the current transformation from user
*     co-ordinates to pixel co-ordinates, followed by the supplied user
*     co-ordinates of the circle centre, followed by the radius in user
*     co-ordinates.

*  Arguments:
*     RINDEX = INTEGER (Given)
*        The value to use to represent interior points.
*     NDIM = INTEGER (Given)
*        The number of dimensions in the B array.
*     LBND( NDIM ) = INTEGER (Given)
*        The lower pixel index bounds of the B array.
*     UBND( NDIM ) = INTEGER (Given)
*        The upper pixel index bounds of the B array.
*     MSKSIZ = INTEGER (Given)
*        The total number of elements in the B array.
*     NPAR = INTEGER (Given)
*        The size of the PAR array.
*     PAR( NPAR ) = REAL (Given)
*        A list of pixel co-ordinates, in groups of NDIM. 
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
*     11-APR-1994 (DSB):
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
      INCLUDE 'ARD_CONST'        ! ARD_ private constants
      INCLUDE 'ARD_ERR'          ! ARD_ error constants

*  Arguments Given:
      INTEGER RINDEX
      INTEGER NDIM
      INTEGER LBND( NDIM )
      INTEGER UBND( NDIM )
      INTEGER MSKSIZ
      INTEGER NPAR
      REAL PAR( NPAR )

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

      REAL 
     :        LPAR( ARD__MXDIM*( ARD__MXDIM + 3 ) ) ! Box parameters

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the number of supplied parameters is wrong.
      IF( NPAR .NE. ( 1 + NDIM )**2 ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL MSG_SETI( 'ND', NDIM )
         CALL ERR_REP( 'ARD1_CIR_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for ^ND D region in ARD1_CIR '//
     :                 '(programming error).', STATUS )
         GO TO 999
      END IF

*  Reset all pixels within the interior bounding box so that they
*  hold exterior values. The pixels outside the interior bounding box
*  already hold exterior values.
      CALL ARD1_BXSET( NDIM, LBND, UBND, MSKSIZ, 0, LBINTB,
     :                 UBINTB, B, STATUS )

*  Create a local copy of the supplied region parameters, excluding 
*  the last parameter which is the circle radius in user coords.
      DO I = 1, NPAR - 1
         LPAR( I ) = PAR( I )
      END DO

*  Append extra parameters (equal to the diameter) to the local copy so
*  that the entire set of local parameters describes a box which just
*  encompasses the circle.
      DO I = NPAR, NPAR + NDIM - 1 
         LPAR( I ) = 2*PAR( NPAR )
      END DO

*  Use these local parameters to find the bounds of the new interior
*  bounding box.
      CALL ARD1_BXFND( NDIM, LBND, UBND, NDIM*( 3 + NDIM ), LPAR,
     :                 LBINTB, UBINTB, STATUS )

*  Call a routine to assign an interior value to every pixel inside
*  the interior bounding box which is also inside the specified user
*  circle.
      CALL ARD1_BXCIR( NDIM, LBND, UBND, MSKSIZ, RINDEX, LBINTB,
     :                 UBINTB, NPAR, PAR, B, STATUS )

*  If the interior bounding box is null, return the usual value
*  (VAL__MINI for LBINTB( 1 ) ).
      DO I = 1, NDIM
         IF( LBINTB( I ) .GT. UBINTB( I ) ) LBINTB( 1 ) = VAL__MINI
      END DO

*  Ensure the the exterior bounding box is returned "infinite".
      LBEXTB( 1 ) = VAL__MAXI

*  Jump here if an error occurs.
 999  CONTINUE

      END
