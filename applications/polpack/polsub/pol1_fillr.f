      SUBROUTINE POL1_FILLR( VALUE, EL, ARRAY, STATUS )
*+
*  Name:
*     POL1_FILLR

*  Purpose:
*     Sets all elements in a vectorised array to a specified value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_FILLR( VALUE, EL, ARRAY, STATUS )

*  Description:
*     This routine sets all the pixels in a 1-dimensional array to a
*     specified value.

*  Arguments:
*     VALUE = REAL (Given)
*        Value to be substituted in every pixel.
*     EL = INTEGER (Given)
*        The dimension of the array to be filled with a constant.
*     ARRAY( EL ) = REAL (Returned)
*        The output array containing a single value.
*     STATUS = INTEGER (Given & Returned)
*        Global status value

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-FEB-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! No default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Global SSE definitions

*  Arguments Given:
      REAL VALUE
      INTEGER EL

*  Arguments Returned:
      REAL ARRAY( EL )

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
