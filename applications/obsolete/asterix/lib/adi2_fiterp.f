      SUBROUTINE ADI2_FITERP( FITERR, STATUS )
*+
*  Name:
*     ADI2_FITERP

*  Purpose:
*     Report a FITSIO error to the EMS/ERR error system

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADI2_FITERP( FITERR, STATUS )

*  Description:
*     Report a FITSIO error whose code is specified by FITERR to the
*     EMS/ERR reporting system.

*  Arguments:
*     FITERR = INTEGER (given)
*        The FITSIO error code
*     STATUS = INTEGER (returned)
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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

*  Keywords:
*     package:adi, usage:private, FITSIO, error reporting

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     1 Feb 1995 (DJA):
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
      INTEGER			FITERR			! FITSIO error code

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*80		ERRTXT			! Error text
*.

*  Set return status
      STATUS = SAI__ERROR

*  Retrieve error text
      CALL FTGERR( FITERR, ERRTXT )

*  Construct EMS error
      CALL MSG_SETC( 'REASON', ERRTXT )
      CALL ERR_REP( ' ', '^REASON', STATUS )

      END
