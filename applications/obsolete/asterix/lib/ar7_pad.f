      SUBROUTINE AR7_PAD( NDIM, DIMS, STATUS )
*+
*  Name:
*     AR7_PAD

*  Purpose:
*     Pad a dimensions array up to 7D with ones

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL AR7_PAD( NDIM, DIMS, STATUS )

*  Description:
*     Before arrays can be handled by the AR7 package their dimensions
*     must be padded with ones to enable them to be declared properly.
*     This routine performs this padding.

*  Arguments:
*     NDIM = INTEGER (given)
*        Number of real dimensions
*     DIMS[7] = INTEGER (given and returned)
*        Dimensions
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  References:
*     AR7 Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/ar7.html

*  Keywords:
*     package:ar7, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     8 Aug 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER			NDIM

*  Arguments Given and Returned:
      INTEGER			DIMS(7)

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			IDIM			! Loop over dimensions
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Pad to 7D
      DO IDIM = NDIM + 1, 7
        DIMS(IDIM) = 1
      END DO

      END
