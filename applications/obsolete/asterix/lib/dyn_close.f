      SUBROUTINE DYN_CLOSE(  )
*+
*  Name:
*     DYN_CLOSE

*  Purpose:
*     Shutdown DYN and free and remaining memory allocations

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN_CLOSE( )

*  Description:
*     Shutdown DYN and free and remaining memory allocations. It is not
*     an error to call this routine if the DYN system is uninitialised.

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
*     DYN Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/dyn.html

*  Keywords:
*     package:dyn, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Mar 1995 (DJA):
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
      INCLUDE 'DYN_CMN'                                 ! DYN common block
*       DYN_ISINIT = LOGICAL (given)
*         DYN class definitions loaded?
*       DYS_PTR[] = INTEGER (given)
*         Dynamic memory addresses

*  External References:
      EXTERNAL			DYN0_BLK		! Ensures inclusion

*  Local Variables:
      INTEGER			I			! Loop over slots
      INTEGER 			STATUS             	! Local status
*.

*  Check initialised
      IF ( DYN_ISINIT ) THEN

*    Start new error context
        CALL ERR_BEGIN( STATUS )

*    Loop over sections
        DO I = 1, DYN__NMAX

*      Active section?
          IF ( DYS_PTR(I) .NE. 0 ) THEN

*        Unmap the section
            CALL DYN0_UNMAP( I, STATUS )

          END IF

        END DO

*      Restore error context
        CALL ERR_END( STATUS )

      END IF

      END
