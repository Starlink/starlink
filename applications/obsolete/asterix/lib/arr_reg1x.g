      SUBROUTINE ARR_REG1<T>( BASE, INCR, NDAT, ARR, STATUS )
*+
*  Name:
*     ARR_REG1<T>

*  Purpose:
*     Fills a <TYPE> array with regularly spaced values

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARR_REG1<T>( BASE, INCR, NDAT, ARR, STATUS )

*  Description:
*     Fills <TYPE> array ARR with regularly spaced values BASE+(i-1)*INCR.

*  Arguments:
*     BASE = <TYPE> (given)
*        The value for the first array value
*     INCR = <TYPE> (given)
*        The value to be added to each subsequent array value
*     NDAT = INTEGER (given)
*        Number of values to write
*     ARR[] = <TYPE> (returned)
*        The output array of regular values
*     STATUS = INTEGER (given)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  References:
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public, arrays, initialisation

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     TJP: Trevor Ponman (University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     31 Mar 87 (TJP):
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
      <TYPE>			BASE			! First array value
      <TYPE>			INCR			! Array increment
      INTEGER			NDAT			! Number of values

*  Arguments Returned:
      <TYPE>			ARR(*)			! Output regular array

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Fill array
      DO I = 1, NDAT
	ARR(I) = BASE + (I-1)*INCR
      END DO

      END
