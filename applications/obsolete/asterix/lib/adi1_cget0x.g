      SUBROUTINE ADI1_CGET0<T>( LOC, NAME, OK, VALUE, STATUS )
*+
*  Name:
*     ADI1_CGET0<T>

*  Purpose:
*     Get a <TYPE> HDS component value, but check existance first

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI1_CGET0<T>( LOC, NAME, OK, VALUE, STATUS )

*  Description:
*     Read the value of an object's component. If the component does
*     not exist, or if an error occurred during the read then OK is
*     set false, otherwise TRUE.

*  Arguments:
*     LOC = CHARACTER*(DAT__SZLOC) (given)
*        Locator of the object whose component is to be read
*     NAME = CHARACTER*(*) (given)
*        The name of the component
*     OK = LOGICAL (returned)
*        Was the component value read succesfully?
*     VALUE = <TYPE> (returned)
*        The component value
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'DAT_PAR'					! HDS constants

*  Arguments Given:
      CHARACTER*(DAT__SZLOC)	LOC			! Object locator
      CHARACTER*(*)		NAME			! Component name

*  Arguments Returned:
      LOGICAL			OK			! Value ok?
      <TYPE>			VALUE			! Object value

*  Status:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Object exists?
      CALL DAT_THERE( LOC, NAME, OK, STATUS )

*  Read it
      IF ( OK ) THEN
        CALL CMP_GET0<T>( LOC, NAME, VALUE, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          OK = .FALSE.
          CALL ERR_ANNUL( STATUS )
        END IF
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI1_CGET0<T>', STATUS )

      END
