      SUBROUTINE CON_FILL<T>( EL, START, INCREM, ARRAY, STATUS )
*+
*  Name:
*     CON_FILLx

*  Purpose:
*     Fills array with linearly spaced values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_FILLx( EL, START, INCREM, ARRAY, STATUS )

*  Description:
*    It fills an array with linearly spaced values, beginning with START
*    and with an increment of INCREM.

*  Arguments:
*     EL = INTEGER (Given)
*        Number of array elements.
*     START = ? (Given)
*        Start value for the array.
*     INCREM = ? (Given)
*        Increment value for the array.
*     ARRAY( EL ) = ? (Returned)
*        Array to contain the linearly spaced values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  There is a routine for double precision and real data types:
*     replace "x" in the routine name by D or R as appropriate.  The
*     START, INCREM, and ARRAY arguments supplied to the routine must
*     have the data type specified.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 February 11 (MJC):
*        Original version, based loosely on CON_FILL.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER EL
      <TYPE> START
      <TYPE> INCREM

*  Arguments Returned:
      <TYPE> ARRAY( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      <LTYPE> VALUE              ! Incremental variable

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Generate the array in which the first element has value START and
*  subsequent values increase by the value of INCREM.  This simple
*  algorithm should not be used when high accuracy is required.
      VALUE = START
      DO I = 1, EL
         ARRAY( I ) = VALUE
         VALUE = VALUE + INCREM
      END DO
 
      END
