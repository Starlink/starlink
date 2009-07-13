      SUBROUTINE POL1_SETR( N, VAL, DATA, STATUS )
*+
*  Name:
*     POL1_SETR

*  Purpose:
*     Fill the supplied array with a constant value.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SETR( N, VAL, DATA, STATUS )

*  Description:
*     This routine stores a given constant value in every element of an
*     array.

*  Arguments:
*     N = INTEGER (Given)
*        The number of points in DATA.
*     VAL = REAL (Given)
*        The value to store.
*     DATA( N ) = REAL (Returned)
*        The array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-MAR-1998 (DSB):
*        Original version.
*     13-JUL-2009 (DSB):
*        Renamed as POL1_SETR and made single precision.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER N
      REAL VAL

*  Arguments Given and Returned:
      REAL DATA( N )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! loop index
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DO I = 1, N
         DATA( I ) = VAL
      END DO

      END
