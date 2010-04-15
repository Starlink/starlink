      SUBROUTINE ARR_INIT1C( N, ARRAY, VALUE, TRUNC, STATUS )
*+
*  Name:
*     ARR_INIT1C

*  Purpose:
*     Initialise elements of a CHARACTER*(*) array

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_INIT1C( N, ARRAY, VALUE, TRUNC, STATUS )

*  Description:
*     Initialise the elements of a 1-dimensional array

*  Arguments:
*     N = INTEGER (given)
*        Number of values to initialise. ARRAY must be declared to be at
*        least this size.
*     ARRAY[] = CHARACTER*(*) (returned)
*        The initialised array
*     VALUE = CHARACTER*(*) (given)
*        Value to use to initialise array
*     TRUNC - LOGICAL (returned)
*        .TRUE. if truncation occurred
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Notes:
*     {routine_notes}...

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public, array, initialisation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     JCMP: Jim Peden (University of Birmingham)
*     AJC:  Alan Chipperfield (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     27-NOV-1985 (JCMP):
*        Original version.
*      7-NOV-2001 (AJC):
*        Reorder arguments so length of mapped characters can be passed
*        Add the TRUNC argument
*     {enter_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*) VALUE       ! Initialisation value
      INTEGER N                 ! Number of values

*  Arguments Returned:
      CHARACTER*(*)ARRAY(*)     ! Array to initialise
      LOGICAL TRUNC             ! Whether truncation occurred

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER I                 ! Loop variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise TRUNC
      TRUNC = .FALSE.
      DO I = 1, N
         ARRAY(I) = VALUE
      ENDDO

*  Check for truncation
      IF( ARRAY(1) .NE. VALUE ) TRUNC = .TRUE.

      END
