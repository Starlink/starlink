      SUBROUTINE KPG1_FILLUW( VALUE, EL, ARRAY, STATUS )
*+
*  Name:
*     KPG1_FILLx
 
*  Purpose:
*     Sets all elements in a vectorised array to a specified value.
 
*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*     CALL KPG1_FILLUW( VALUE, EL, ARRAY, STATUS )
 
*  Description:
*     This routine sets all the pixels in a 1-dimensional array to a
*     specified value.
 
*  Arguments:
*     VALUE = ? (Given)
*        Value to be substituted in every pixel.
*     EL = INTEGER (Given)
*        The dimension of the array to be filled with a constant.
*     ARRAY( EL ) = ? (Returned)
*        The output array containing a single value.
*     STATUS = INTEGER (Given & Returned)
*        Global status value
 
*  Notes:
*     -  There is a routine for each numeric data type: replace "x" in
*     the routine name by D, R, I, W, UW, B, or UB as appropriate.  The
*     VALUE and ARRAY arguments must have the data type specified.
 
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     1989 October 31 (MJC):
*        Original version.
*     1995 April 25 (MJC):
*        Renamed from SETAV, and used a modern prologue and commenting
*        style.
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-
 
*  Type Definitions:
      IMPLICIT  NONE             ! No default typing allowed
 
*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions
 
*  Arguments Given:
      INTEGER*2 VALUE
      INTEGER EL
 
*  Arguments Returned:
      INTEGER*2 ARRAY( EL )
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop counter
 
*.
 
*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Fill the array with the constant.
      DO I = 1, EL
         ARRAY( I ) = VALUE
      END DO
 
      END
