      SUBROUTINE CON_FILL (NELM, START, INCREM, ARRAY, STATUS)
*+
*  Name:
*     CON_FILL

*  Purpose:
*     Fills array with linearly space values, beginning with START
*     and with an increment of INCREM.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_FILL (NELM, START, INCREM, ARRAY, STATUS)

*  Description:
*    The array is generated as described above. 

*  Arguments:

*     NELM        = INTEGER (Given)
*        Array size
*     START       = REAL (Given)
*        Start value for array
*     INCREM      = REAL (Given)
*        Increment value for array
*     ARRAY(NELM) = REAL (Returned)
*        Destination array for data.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*  Authors:
*     JM: Jo Murray (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 Feb 21 (JM):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT    NONE           ! No implicit typing

*  Global Constants:
      INCLUDE    'SAE_PAR'       ! Standard SAE constants

*  Arguments Given:
      INTEGER    NELM            ! Array size
      REAL       START           ! Start value for array
      REAL       INCREM          ! Incremental velue for array

*  Arguments Reurned:
      REAL       ARRAY(NELM)     ! Destination array.

*  Status:
      INTEGER    STATUS          ! Global status

*  Local variables:
      INTEGER    I               ! Loop variable
      REAL       VALUE           ! Incremental variable.
*.

*    Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  .

*   Generate an array where the first element has value START
*   and subsequent values increase by the value of INCREM.      
      VALUE = START
      DO I=1, NELM
         ARRAY(I) = VALUE
         VALUE = VALUE + INCREM
      END DO
 
      END
