      SUBROUTINE USI0_LOCRTN( CODE, RPTR, STATUS )
*+
*  Name:
*     USI0_LOCRTN

*  Purpose:
*     Returns address of parameter system routine given its code

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL USI0_LOCRTN( CODE, RPTR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     CODE = INTEGER (given)
*        The code of the parameter system method
*     RPTR = INTEGER (returned)
*        The address of the routine implementing the function specified by CODE
*     STATUS = INTEGER (given and returned)
*        The global status.

*  Examples:
*     {routine_example_text}
*        {routine_example_description}

*  Pitfalls:
*     {pitfall_description}...

*  Notes:
*     {routine_notes}...

*  Prior Requirements:
*     {routine_prior_requirements}...

*  Side Effects:
*     {routine_side_effects}...

*  Algorithm:
*     {algorithm_description}...

*  Accuracy:
*     {routine_accuracy}

*  Timing:
*     {routine_timing}

*  External Routines Used:
*     {name_of_facility_or_package}:
*        {routine_used}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     USI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/usi.html

*  Keywords:
*     package:usi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     6 Mar 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'USI_CMN'                                 ! USI common block
*       USI_INIT = LOGICAL (given)
*         USI class definitions loaded?

*  Arguments Given:
      INTEGER			CODE

*  Arguments Returned:
      INTEGER			RPTR

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			USI0_BLK		! Ensures inclusion
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check USI initialised
      IF ( .NOT. USI_SYINIT ) CALL USI_INIT( STATUS )

*  Get routine pointer
      RPTR = PS_RTN(CODE,CTX_TYPE(USI_ICTX))

*  Report error if zero
      IF ( RPTR .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'CODE', CODE )
        CALL ERR_REP( ' ', 'Null parameter system method, code'/
     :                /' = ^CODE', STATUS )

      END IF

      END
