      SUBROUTINE EDI_LINK( CLASS, NEVENT, TITLE, FID, STATUS )
*+
*  Name:
*     EDI_LINK

*  Purpose:
*     Create a new event dataset derived object and link to file identifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_LINK( CLASS, NEVENT, TITLE, FID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     CLASS = CHARACTER*(*) (given)
*        The class of the new object
*     NEVENT = INTEGER (given)
*         Number of events in the new dataset
*     TITLE = CHARACTER*(*) (given)
*        The title of the event dataset
*     FID  = INTEGER (given and returned)
*       On entry the file object on the right hand side of the link, on
*       exit the object of class CLASS.
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
*     EDI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/edi.html

*  Keywords:
*     package:edi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     19 Dec 1995 (DJA):
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
      INTEGER			NEVENT
      CHARACTER*(*)		CLASS,TITLE

*  Arguments Given and Returned:
      INTEGER			FID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			TID			! New object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create the new object
      CALL ADI_NEW0( CLASS, TID, STATUS )

*  Write components
      CALL ADI_CPUT0I( TID, 'NEVENT', NEVENT, STATUS )
      CALL ADI_CPUT0C( TID, 'Title', TITLE, STATUS )

*  Link objects
      CALL UDI_LINK( TID, FID, STATUS )

*  Update file object if successful
      IF ( STATUS .EQ. SAI__OK ) FID = TID

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_LINK', STATUS )

      END
