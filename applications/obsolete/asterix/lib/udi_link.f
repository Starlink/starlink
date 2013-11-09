      SUBROUTINE UDI_LINK( LHS, RHS, STATUS )
*+
*  Name:
*     UDI_LINK

*  Purpose:
*     Link two objects together, and update USI data if present

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UDI_LINK( LHS, RHS, STATUS )

*  Description:
*     This routine is used to create new BinDS derived data objects and
*     link them to low level file objects. The supplied file identifier
*     is updated and should be used to erase the object chain after this
*     routine has been invoked.

*  Arguments:
*     LHS = INTEGER (given)
*        The new object
*     RHS = INTEGER (given)
*        The object to link to (ie. the one nearer the file end of the chain)
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
*     UDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/udi.html

*  Keywords:
*     package:udi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Dec 1995 (DJA):
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
      INTEGER			LHS, RHS

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*15		PAR			! USI parameter
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Link these identifiers
      CALL ADI_SETLNK( LHS, RHS, STATUS )

*  Was the input object obtained from USI?
      CALL USI0_IDPAR( RHS, PAR, STATUS )
      IF ( PAR .GT. ' ' ) THEN

*    If so, update USI storage
        CALL USI0_UPDID( PAR, LHS, STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'UDI_LINK', STATUS )

      END
