      SUBROUTINE EDI_MTYPE( LID, MTYPE, STATUS )
*+
*  Name:
*     EDI_MTYPE

*  Purpose:
*     Decide on a mapping type given a list identifier

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_MTYPE( LID, MTYPE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     LID = INTEGER (given)
*        ADI identifier of EventList object
*     MTYPE = CHARACTER*(*) (returned)
*        The ADI type to map the list with
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
*     21 Aug 1995 (DJA):
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
      INTEGER			LID

*  Arguments Returned:
      CHARACTER*(*)		MTYPE

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Variables:
      CHARACTER*20		TYPE			! Basic type
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the type name
      CALL ADI_CGET0C( LID, 'TYPE', TYPE, STATUS )

*  Choose mapping type
      IF ( CHR_INSET( 'REAL,DOUBLE', TYPE ) ) THEN
        MTYPE = 'DOUBLE'

      ELSE IF ( CHR_INSET( 'BYTE,UBYTE,WORD,UWORD,INTEGER',
     :                       TYPE ) ) THEN
        MTYPE = 'INTEGER'

      ELSE IF ( TYPE .EQ. 'LOGICAL' ) THEN
        MTYPE = 'LOGICAL'

      ELSE
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Character lists cannot be handled '/
     :                         /' by this application', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_MTYPE', STATUS )

      END
