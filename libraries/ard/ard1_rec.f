      SUBROUTINE ARD1_REC( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, PAR,
     :                     B, LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )
*+
*  Name:
*     ARD1_REC

*  Purpose:
*     Initialise an array to hold a RECT region.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_REC( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, PAR, B,
*                    LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Description:
*     The supplied parameters are modified so that they look the same
*     as those for a BOX region. ARD1_BOX is then called to load the
*     region.

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
     :        I,                 ! Loop count
     :        I0                 ! No. of transformation co-efficients

      REAL
     :        LPAR( ARD__MXDIM*( 3 + ARD__MXDIM ) )! Local parameters

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the number of supplied parameters is wrong.
      IF( NPAR .NE. NDIM*( 3 + NDIM ) ) THEN
         STATUS = ARD__INTER
         CALL MSG_SETI( 'NP', NPAR )
         CALL MSG_SETI( 'ND', NDIM )
         CALL ERR_REP( 'ARD1_REC_ERR1', 'Wrong no. of parameters '//
     :                 '(^NP) supplied for ^ND D region in ARD1_REC '//
     :                 '(programming error).', STATUS )
         GO TO 999
      END IF

*  Store the number of transformation co-efficient.
      I0 = NDIM*( 1 + NDIM )

*  Copy the transformation co-efficients from the supplied parameter
*  array to a local parameter array.
      DO I = 1, I0
         LPAR( I ) = PAR ( I )
      END DO

*  Loop round each dimension.
      DO I = I0 + 1, I0 + NDIM

*  Find the center of the box on this axis, and store in the local
*  parameter array.
         LPAR( I ) = 0.5*( PAR( I ) + PAR( I + NDIM ) )

*  Find the length of the box on this axis, and store in the local
*  parameter array.
         LPAR( I + NDIM ) = ABS( PAR( I ) - PAR( I + NDIM ) )

      END DO

*  The parameters are now in the same format as those for a BOX region.
*  Call the subroutine used to load a BOX region.
      CALL ARD1_BOX( RINDEX, NDIM, LBND, UBND, MSKSIZ, NPAR, LPAR,
     :               B, LBEXTB, UBEXTB, LBINTB, UBINTB, STATUS )

*  Jump here if an error occurs.
 999  CONTINUE

      END
