      SUBROUTINE FCI_GETMC( CTRLID, STATUS )
*+
*  Name:
*     FCI_GETMC

*  Purpose:
*     Load minimisation control data

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FCI_GETMC( CTRLID, STATUS )

*  Description:
*     Loads minimisation control either from a file, or if that is not
*     present, from the environment.

*  Arguments:
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
      INCLUDE 'AST_PKG'
      INCLUDE 'PAR_ERR'
      INCLUDE 'SUBPAR_ERR'

*  Arguments Returned:
      INTEGER			CTRLID

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      LOGICAL			AST_QPKGI
        EXTERNAL		AST_QPKGI

*  Local Variables:
      REAL			MINSLO			! Minimum slope

      INTEGER			NITMAX			! Max # iterations
      INTEGER			NUP			! Update this often
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( FCI__PKG ) ) CALL FCI0_INIT( STATUS )

*  User is supplying a minimisation control file?
      CALL USI_ASSOC( 'MCTRL', 'MinimisationControl', 'UPDATE',
     :                CTRLID, STATUS )

*  Trap user null response
      IF ( STATUS .EQ. PAR__NULL ) THEN

*    Clear status
        CALL ERR_ANNUL( STATUS )

*    Get parameters describing CURFIT control algorithm
        CALL USI_GET0I( 'MAX', NITMAX, STATUS )
        CALL USI_GET0R( 'MINS', MINSLO, STATUS )

*    Update control is optional - not needed by error and gridding software
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL USI_GET0I( 'NUP', NUP, STATUS )
          IF ( STATUS .EQ. SUBPAR__NOPAR ) THEN
            CALL ERR_ANNUL( STATUS )
            NUP = 1
          END IF
        END IF

*    Create control for CURFIT algorithm
        CALL FCI_CURFMC( NITMAX, NUP, MINSLO, CTRLID, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FCI_GETMC', STATUS )

      END
