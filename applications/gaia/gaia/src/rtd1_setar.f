      SUBROUTINE RTD1_SETAR( START, STEP, SIZE, ARRAY, STATUS )
*+
*  Name:
*     RTD1_SETAR

*  Purpose:
*     Sets the values of an array to an incremented range of values.

*  Language:
*     Starlink Fortran-77.

*  Invocation:
*     CALL RTD1_SETAR( START, STEP, SIZE, ARRAY, STATUS )

*  Description:
*     This routine fills an array with values starting at START and
*     incremented between each element by STEP. It's really just 
*     a convience for setting dynamically allocated arrays.
*
*     The result is:
*        ARRAY( I ) = START + ( I - 1 ) * STEP

*  Arguments:
*     START = REAL (Given)
*        Value of first element in ARRAY.
*     STEP = REAL (Given)
*        Increment for each pixel.
*     SIZE = INTEGER (Given)
*        Size of the array ARRAY.
*     ARRAY( SIZE ) = REAL (Returned)
*        Array with all elements set up with incremented values.
*     STATUS = INTEGER ({status_access_mode})
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     14-MAR-1996 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      
*  Arguments Given:
      REAL START
      REAL STEP
      INTEGER SIZE
      
*  Arguments Returned:
      REAL ARRAY( SIZE )
      
*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER I                 ! Loop variable
      REAL CURVAL               ! Current array value
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      CURVAL = START
      DO 1 I = 1, SIZE
         ARRAY( I ) = CURVAL
         CURVAL = CURVAL + STEP
 1    CONTINUE
      END
