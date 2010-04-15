      SUBROUTINE ARR_COP1C( N, IN, OUT, TRUNC, STATUS )
*+
*  Name:
*     ARR_COP1C

*  Purpose:
*     Copies 1D CHARACTER array to another

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_COP1C( N, IN, OUT, TRUNC, STATUS )

*  Description:
*     Copies 1D CHARACTER array to another
*     Silently truncating if necessary

*  Arguments:
*     N = INTEGER (given)
*        Number of elements to copy
*     IN[] = CHARACTER*(*) (given)
*        Input array to be copied
*     OUT[] = CHARACTER*(*) (returned)
*        Copy of input array
*     TRUNC = LOGICAL (returned)
*        Whether truncation has occurred
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public, array, copying

*  Copyright:
*     Copyright (C) CCLRC 2001

*  Authors:
*     AJC: Alan J Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*      7-NOV-2001 (AJC):
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
      INTEGER N			! Number of elements
      CHARACTER*(*) IN(*)	! Input array

*  Arguments Returned:
      LOGICAL TRUNC             ! Whether truncation occurred
      CHARACTER*(*) OUT(*)	! Output array

*  Status:
      INTEGER STATUS           	! Global status

*  Local Variables:
      INTEGER I                 ! Loop variable
*.

*  Check inherited global status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Initialise TRUNC
         TRUNC = .FALSE.

*      Copy the array
         DO I = 1, N
            OUT(I) = IN(I)
*         Check for truncation of non-blank characters
            IF( OUT(I) .NE. IN(I) ) TRUNC = .TRUE.
         END DO

      END IF

      END
