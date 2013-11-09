      LOGICAL FUNCTION MPC_QSLAVE( ID, STATUS )
*+
*  Name:
*     MPC_QSLAVE

*  Purpose:
*     Is the current process a multiple-process slave?

*  Language:
*     Starlink Fortran

*  Invocation:
*     RESULT = MPC_QSLAVE( ID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of multi-process control. May be equal to
*        ADI__NULLID in which case the process is assumed to be a master
*     STATUS = INTEGER (given)
*        The global status.

*  Returned Value:
*     MPC_QSLAVE = LOGICAL
*        True if the current process is a slave process

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
*     {facility_or_package}...

*  Implementation Deficiencies:
*     {routine_deficiencies}...

*  References:
*     MPC Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/mpc.html

*  Keywords:
*     package:mpc, usage:public

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Apr 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			ID

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER			MYCPU			! Process cpu #
      LOGICAL			RVAL			! Return value
*.

*  Initialise
      RVAL = .FALSE.
      IF ( ID .NE. ADI__NULLID  )THEN
        CALL ADI_CGET0I( ID, 'MYCPU', MYCPU, STATUS )
        RVAL = (MYCPU.GT.0)
      END IF

*  Set return value
      MPC_QSLAVE = RVAL

      END
