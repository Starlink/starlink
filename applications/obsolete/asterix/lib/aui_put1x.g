      SUBROUTINE AUI_PUT1<T>( ID, NAME, NVAL, VALUES, STATUS )
*+
*  Name:
*     AUI_PUT1<T>

*  Purpose:
*     Write vector auxilliary parameter

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL AUI_PUT1<T>( ID, NAME, NVAL, VALUES, STATUS )

*  Description:
*     Writes a vector <COMM> auxilliary value to the specified dataset. Auxilliary
*     data is just odd bits of information which don't fit into the dataset
*     model which applications need to put somewhere. Generally knowledge of
*     the format of this information is very restricted, often to one program.

*  Arguments:
*     ID = INTEGER (given)
*        Dataset to which data is to be written
*     NAME = CHARACTER*(*) (given)
*        The character name of the attribute being written
*     NVAL = INTEGER (given)
*        Number of values to write
*     VALUES[] = <TYPE> (given)
*        The values of the attribute
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
*     AUI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/aui.html

*  Keywords:
*     package:aui, usage:public, auxilliary data, write

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     4 Apr 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants

*  Arguments Given:
      INTEGER			ID			! Dataset identifier
      CHARACTER*(*)		NAME			! Attribute name
      INTEGER			NVAL			! Attribute dimension
      <TYPE>			VALUES(*)		! Attribute values

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			VARG			! Temp object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create ADI object describing value
      CALL ADI_NEWV1<T>( NVAL, VALUES, VARG, STATUS )

*  Write the value
      CALL AUI_PUTID( ID, NAME, VARG, STATUS )

*  Destroy temporary object
      CALL ADI_ERASE( VARG, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'AUI_PUT1<T>', STATUS )

      END
