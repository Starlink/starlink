      SUBROUTINE FCI1_SHOW( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     FCI1_SHOW

*  Purpose:
*     Display status of CURFIT minimisation

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FCI1_SHOW( NARG, ARGS, OARG, STATUS )

*  Description:
*     Reports status of a fit given minimisation control object

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     FCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fci.html

*  Keywords:
*     package:fci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     7 Mar 1996 (DJA):
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
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			NIT			! Iterations performed
      INTEGER			OCI			! Output channel

      LOGICAL			FINISHED		! Got to minimum?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract stream argument
      CALL ADI_GET0I( ARGS(1), OCI, STATUS )

*  Report number of iterations performed
      CALL AIO_BLNK( OCI, STATUS )
      CALL ADI_CGET0I( ARGS(2), 'Niter', NIT, STATUS )
      CALL MSG_SETI( 'NIT', NIT )
      CALL AIO_WRITE( OCI, 'Terminated after ^NIT iterations', STATUS )

*  Achieved minimum?
      CALL ADI_CGET0L( ARGS(2), 'Finished', FINISHED, STATUS )
      IF ( FINISHED ) THEN
        CALL AIO_WRITE( OCI, 'Minimum found', STATUS )
      ELSE
        CALL AIO_WRITE( OCI, 'Minimum not found', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FCI1_SHOW', STATUS )

      END
