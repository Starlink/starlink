      SUBROUTINE KPG1_ELNMB( LBND, UBND, EL, ARRAY, STATUS )
*+
*  Name:
*     KPG1_ELNMx
 
*  Purpose:
*     Writes a range of element numbers into an array.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_ELNMBx( LBND, UBND, EL, ARRAY, STATUS )
 
*  Description:
*     This routine writes numbers sequentially to a 1-dimensional
*     array, where the numbers are between defined integer limits and
*     are stepped by 1 (or -1) from one pixel to the next.  In other
*     words the array value takes the element number plus an offset.
*     Only the first EL elements can be accommodated in the output
*     array, should the section be larger than this.
 
*  Arguments:
*     LBND = INTEGER (Given)
*        The first value to be written to the output array.  If this is
*        larger than argument UBND, the values will decrease by 1 for
*        each element.
*     UBND = INTEGER (Given)
*        The last value to be written to the output array, provided
*        there are sufficient elements to accommodate it.  If not
*        only EL values will be stored.
*     EL = INTEGER (Given)
*        The number of elements in the output array.
*     ARRAY( EL ) = ? (Returned)
*        The array into which the sub-array is copied.
*     STATUS = INTEGER (Given)
*        The global status.
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B or UB as appropriate.  The
*     ARRAY argument must have the data type specified.
 
*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1988 October 25 (MJC):
*        Original version.
*     1990 July 29 (MJC):
*        Allowed the element value to decrease with increasing element
*        number.
*     1995 April 26 (MJC):
*        Renamed from ELNMB.  Added the Notes; and used modern-style
*        variable names and declarations, and commenting.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing allowed
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
 
*  Arguments Given:
      INTEGER LBND
      INTEGER UBND
      INTEGER EL
 
*  Arguments Returned:
      BYTE ARRAY( EL )
 
*  Status:
      INTEGER STATUS
 
*  Local Variables:
      INTEGER I                  ! Counter
      INTEGER J                  ! Counter
      INTEGER LOWER              ! Lower bound of the array values
      INTEGER UPPER              ! Upper bound of the array values
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM declarations for conversions
      INCLUDE 'NUM_DEF_CVT'      ! NUM definitions for conversions
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Decide if the increment is positive or negative.
      IF ( UBND. GE. LBND ) THEN
 
*  Forward
*  =======
*
*  Only store as many values as can be accommodated.
         UPPER = MIN( UBND, EL + LBND - 1 )
 
*  Copy the elements of the array.
         J = 0
         DO  I = LBND, UPPER, 1
            J = J + 1
            ARRAY( J ) = NUM_ITOB( I )
         END DO
 
      ELSE
 
*  Reverse
*  =======
 
*  Only store as many values as can be accommodated.
         LOWER = MAX( UBND, LBND - EL + 1 )
 
*  Copy the elements of the array.
         J = 0
         DO  I = LBND, LOWER, -1
            J = J + 1
            ARRAY( J ) = NUM_ITOB( I )
         END DO
      END IF
 
      END
