      SUBROUTINE DYN0_FIND( PTR, SLOT, STATUS )
*+
*  Name:
*     DYN0_FIND

*  Purpose:
*     Look up internal table to find mapped memory referenced by its address

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DYN0_FIND( PTR, SLOT, STATUS )

*  Description:
*     Looks up the internal table to find the slot number of a particular
*     area of mapped memory. Reports an error if not found.

*  Arguments:
*     PTR = INTEGER (given)
*        The address of the mapped memory section
*     SLOT = INTEGER (returned)
*        The DYN internal slot of this mapped memory
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
*     DYN Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/dyn.html

*  Keywords:
*     package:dyn, usage:private

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
      INCLUDE 'SAE_PAR'          		! Standard SAE constants

*  Global Variables:
      INCLUDE 'DYN_CMN'                                 ! DYN common block
*       DYS_PTR[] = INTEGER (given)
*         DYN memory addresses

*  Arguments Given:
      INTEGER			PTR			! See above

*  Arguments Returned:
      INTEGER			SLOT			!

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			I			! Loop over slots

      LOGICAL			FOUND			! Found slot yet?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      I = 1
      FOUND = .FALSE.

*  Scan for section
      DO WHILE ( (I.LE.DYN__NMAX) .AND. .NOT. FOUND )
        IF ( DYS_PTR(I) .EQ. PTR ) THEN
          FOUND = .TRUE.
        ELSE
          I = I + 1
        END IF
      END DO

*  Report error if not found, otherwise return slot
      IF ( FOUND ) THEN
        SLOT = I
      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid dynamic memory address - '/
     :                        /'not allocated by DYN', STATUS )
      END IF

*  Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'DYN0_FIND', STATUS )
      END IF

      END
