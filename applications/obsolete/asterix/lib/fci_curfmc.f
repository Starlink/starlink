      SUBROUTINE FCI_CURFMC( NITMAX, NUP, MINSLO, CTRLID, STATUS )
*+
*  Name:
*     FCI_CURFMC

*  Purpose:
*     Create Curfit minimisation control object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FCI_CURFMC( NITMAX, NUP, MINSLO, CTRLID, STATUS )

*  Description:
*     Creates minimisation control for CURFIT algorithm using supplied data

*  Arguments:
*     NITMAX = INTEGER (given)
*        Max number of iterations
*     NUP = INTEGER (given)
*        Number of iterations between progress reports
*     MINSLO = REAL (given)
*        Minimum slope regarded as indicating fit is over
*     CTRLID = INTEGER (returned)
*        ADI identifier to instance of class derived from MinimisationControl
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
*     package:fci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1996

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     24 Apr 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PKG'

*  Arguments Given:
      INTEGER			NITMAX, NUP
      REAL			MINSLO

*  Arguments Returned:
      INTEGER			CTRLID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( FCI__PKG ) ) CALL FCI0_INIT( STATUS )

*  Create control for CURFIT algorithm
      CALL ADI_NEW0( 'CurfitControl', CTRLID, STATUS )

*  Write control data
      CALL ADI_CPUT0I( CTRLID, 'MaxIt', NITMAX, STATUS )
      CALL ADI_CPUT0I( CTRLID, 'UpdateInterval', NUP, STATUS )
      CALL ADI_CPUT0R( CTRLID, 'MinSlope', MINSLO, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FCI_CURFMC', STATUS )

      END
