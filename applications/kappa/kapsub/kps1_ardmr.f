      SUBROUTINE KPS1_ARDMR( EL, MASK, ARRAY, STATUS )
 
*+
*  Name:
*     KPS1_ARDMx
 
*  Purpose:
*     Sets to bad all elements of an array that have a positive mask
*     value.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPS1_ARDMx( EL, MASK, ARRAY, STATUS )
 
*  Description:
*     This routine uses an integer mask (probably from ARD) to flag
*     elements of an array.  Whenever a positive mask value is found
*     the associated pixel in the output image is set to VAL__BADR.
 
*  Arguments:
*     EL = INTEGER (Given)
*        The dimension of the input and output arrays.
*     MASK( EL ) = INTEGER (Given)
*        The mask array.  Positive values willgenerate bad values in the
*        array.
*     ARRAY( EL ) = ? (Given & Returned)
*        The array to be masked.
*     STATUS  =  INTEGER (Given and Returned)
*        Global status value.
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate. The
*     ARRAY argument supplied to the routine must have the data type
*     specified.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1995 June 30 (MJC):
*        Original version.
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE parameters
      INCLUDE 'PRM_PAR'          ! VAL_ constants
 
*  Status:
      INTEGER STATUS             ! Global status value
 
*  Arguments Given:
      INTEGER EL
      INTEGER MASK( EL )
 
*  Arguments Given and Returned:
      REAL ARRAY( EL )
 
*  Local Variables:
       INTEGER I                 ! Loop counter
 
*.
 
*  Check the global inhertied status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Loop for each array element.  Replace the element with the bad value
*  whenever the corresponding mask value is positive.
      DO I = 1, EL
         IF ( MASK( I ) .GT. 0 ) ARRAY( I ) = VAL__BADR
      END DO
 
      END
