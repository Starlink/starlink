      SUBROUTINE WCI_SAME( PIX1, PRJ1, SYS1, PIX2, PRJ2, SYS2, SAME,
     :                     STATUS )
*+
*  Name:
*     WCI_SAME

*  Purpose:
*     Test whether two coordinate systems are the same

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL WCI_SAME( PIX1, PRJ1, SYS1, PIX2, PRJ2, SYS2, SAME, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PIX1 = INTEGER (given)
*        Pixellation object of 1st input
*     PRJ1 = INTEGER (given)
*        Projection object of 1st input
*     SYS1 = INTEGER (given)
*        CoordSystem object of 1st input
*     PIX2 = INTEGER (given)
*        Pixellation object of 2nd input
*     PRJ2 = INTEGER (given)
*        Projection object of 2nd input
*     SYS2 = INTEGER (given)
*        CoordSystem object of 2nd input
*     SAME = LOGICAL (returned)
*        Are coordinate systems the same?
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
*     WCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/wci.html

*  Keywords:
*     package:wci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     17 Oct 1995 (DJA):
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
      INTEGER			PIX1, PRJ1, SYS1, PIX2, PRJ2, SYS2

*  Arguments Returned:
      LOGICAL			SAME

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      DOUBLE PRECISION		PA1, PA2		! Position angles
      DOUBLE PRECISION		P1(2), P2(2)		! Pointing directions

      INTEGER			IDUM			! Dummy argument
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      SAME = .FALSE.

*  Compare pointing directions
      CALL ADI_CGET1D( PRJ1, 'SPOINT', 2, P1, IDUM, STATUS )
      CALL ADI_CGET1D( PRJ2, 'SPOINT', 2, P2, IDUM, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        IF ( (P1(1).EQ.P2(1)) .AND. (P1(2).EQ.P2(2)) ) THEN

*    Compare position angle
          CALL ADI_CGET0D( PIX1, 'ROTATION', PA1, STATUS )
          CALL ADI_CGET0D( PIX2, 'ROTATION', PA2, STATUS )
          IF ( STATUS .EQ. SAI__OK ) THEN

*        Same?
            IF ( PA1 .EQ. PA2 ) THEN
              SAME = .TRUE.
            END IF
          ELSE
            CALL ERR_ANNUL( STATUS )
          END IF

        END IF
      ELSE
        CALL ERR_ANNUL( STATUS )

      END IF

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'WCI_SAME', STATUS )

      END
