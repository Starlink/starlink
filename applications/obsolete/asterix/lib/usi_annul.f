      SUBROUTINE USI_ANNUL( PAR, STATUS )
*+
*  Name:
*     USI_ANNUL

*  Purpose:
*     Close the any file associated with the parameter PAR

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL USI_ANNUL( PAR, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PAR = CHARACTER*(*) (given)
*        The name of the environment parameter
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
*     USI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/usi.html

*  Keywords:
*     package:usi, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Sep 1995 (DJA):
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
      CHARACTER*(*)		PAR

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER 			ID			! Associated object
      INTEGER 			PSID			! Parameter storage

      LOGICAL 			THERE			! Object exists?
*.

*  New error context
      CALL ERR_BEGIN( STATUS )

*  Look for object associated with the parameter
      CALL USI0_FNDPSL( PAR, .FALSE., PSID, STATUS )

*  Parameter was found?
      IF ( STATUS .EQ. SAI__OK ) THEN

*    Association?
        CALL ADI_THERE( PSID, 'ID', THERE, STATUS )
        IF ( THERE ) THEN

*      Close the file
          CALL ADI_CGET0I( PSID, 'ID', ID, STATUS )
          CALL ADI_FCLOSE( ID, STATUS )

*      Destroy association objects
          CALL ADI_CERASE( PSID, 'IO', STATUS )
          CALL ADI_CERASE( PSID, 'ID', STATUS )

        END IF

      END IF

*  Restore error context
      CALL ERR_END( STATUS )

      END
