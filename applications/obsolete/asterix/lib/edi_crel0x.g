      SUBROUTINE EDI_CREL0<T>( ID, LIST, DECR, FMIN, FMAX,
     :                         QUANTUM, UNITS, LID, STATUS )
*+
*  Name:
*     EDI_CREL0<T>

*  Purpose:
*     Create a scalar list of type <COMM>

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL EDI_CREL0<T>( ID, LIST, DECR, FMIN, FMAX,
*                    QUANTUM, UNITS, LID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of EventDS or derived object
*     LIST = CHARACTER*(*) (given)
*        Name of the new list
*     DECR = LOGICAL (given)
*        Is the list naturally decreasing in value
*     FMIN = <TYPE> (given)
*        The minimum allowable data value
*     FMAX = <TYPE> (given)
*        The maximum allowable data value
*     QUANTUM = <TYPE> (given)
*        The minimum realistic change in list value
*     UNITS = CHARACTER*(*) (given)
*        Units of the list data
*     LID = INTEGER (returned)
*        The identifier of the new list
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
*     17 Aug 1995 (DJA):
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
      CHARACTER*(*)		LIST, UNITS
      LOGICAL	                DECR
      <TYPE>			FMIN, FMAX, QUANTUM

*  Arguments Returned:
      INTEGER			LID

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke master routine
      CALL EDI_CREL( ID, LIST, '<HTYPE>', 0, 0, DECR, FMIN, FMAX,
     :               QUANTUM, UNITS, LID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'EDI_CREL0<T>', STATUS )

      END
