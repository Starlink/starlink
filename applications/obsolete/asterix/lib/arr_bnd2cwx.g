      SUBROUTINE ARR_BND2CW<T>( NBND, BNDS, CEN, WID, STATUS )
*+
*  Name:
*     ARR_BND2CW<T>

*  Purpose:
*     Calculate <COMM> centres and widths from bounds

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_BND2CW<T>( NBND, BNDS, CEN, WID, STATUS )

*  Description:
*     The data are written to the axis width component, overwriting any
*     existing data.

*  Arguments:
*     NBND = INTEGER (given)
*        The number of axis bounds pairs
*     BNDS[2,N] = <TYPE> (given)
*        The axis bounds
*     CEN[] = <TYPE> (returned)
*        The axis centres
*     WID[] = <TYPE> (returned)
*        The axis centres
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
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
      INTEGER                   NBND
      <TYPE>			BNDS(2,*)

*  Arguments Returned:
      <TYPE>			CEN(*), WID(*)

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over arrays
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  For each bound pair
      DO I = 1, NBND

*    Bin centre
        CEN(I) = (BNDS(1,I) + BNDS(2,I))/2

*    Bin width
        WID(I) = BNDS(2,I) - BNDS(1,I)

      END DO

      END
