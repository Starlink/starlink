      SUBROUTINE BDI_GETDST( ID, DTYPE, STATUS )
*+
*  Name:
*     BDI_GETDST

*  Purpose:
*     Get the DatasetType component of a object handled by BDI

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL BDI_GETDST( ID, DTYPE, STATUS )

*  Description:
*     Get the DatasetType component of an object being handled by BDI. For
*     objects derived from BinDS the value of the DatasetType class member
*     is returned if present, otherwise DTYPE is set to 'PRIMITIVE'.

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of BinDS derived object
*     DTYPE = CHARACTER*(*) (returned)
*        The dataset type
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
*     BDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/bdi.html

*  Keywords:
*     package:bdi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Sep 1995 (DJA):
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
      INTEGER			ID

*  Arguments Returned:
      CHARACTER*(*)		DTYPE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      LOGICAL			OK			! Derived from BinDS?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Derived from BinDS?
      CALL ADI_DERVD( ID, 'BinDS', OK, STATUS )
      IF ( OK ) THEN
        CALL ADI_CGET0C( ID, 'DatasetType', DTYPE, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          DTYPE = 'BINDS'
        END IF
      ELSE
        DTYPE = 'PRIMITIVE'
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'BDI_GETDST', STATUS )

      END
