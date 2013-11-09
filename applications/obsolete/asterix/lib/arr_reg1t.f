      SUBROUTINE ARR_REG1T( TYPE, BASE, INCR, NDAT, ARR, STATUS )
*+
*  Name:
*     ARR_REG1T

*  Purpose:
*     Create regular series of arbitrary numeric type

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ARR_REG1T( TYPE, BASE, INCR, NDAT, ARR, STATUS )

*  Description:
*     Create regular series of arbitrary numeric type. The base and
*     increment will usually have been extracted into a workspace
*     data area.

*  Arguments:
*     TYPE = CHARACTER*(*) (given)
*        The HDS style real type name of the BASE, INCR and ARR arguments
*     BASE = BYTE (given)
*        The value for the first array value, really of type TYPE
*     INCR = BYTE (given)
*        The value to be added to each subsequent array value
*     NDAT = INTEGER (given)
*        Number of values to write
*     ARR[] = BYTE (returned)
*        The output array of regular values
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
*     ARR Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/arr.html

*  Keywords:
*     package:arr, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     14 Aug 1995 (DJA):
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
      CHARACTER*(*)		TYPE			!
      BYTE			BASE, INCR
      INTEGER			NDAT

*  Arguments Returned:
      BYTE			ARR(*)

*  Status:
      INTEGER			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Switch on type
      IF ( TYPE .EQ. '_REAL' ) THEN
        CALL ARR_REG1R( BASE, INCR, NDAT, ARR, STATUS )
      ELSE IF ( TYPE .EQ. '_DOUBLE' ) THEN
        CALL ARR_REG1D( BASE, INCR, NDAT, ARR, STATUS )
      ELSE IF ( TYPE .EQ. '_INTEGER' ) THEN
        CALL ARR_REG1I( BASE, INCR, NDAT, ARR, STATUS )
      ELSE IF ( TYPE .EQ. '_WORD' ) THEN
        CALL ARR_REG1W( BASE, INCR, NDAT, ARR, STATUS )
      ELSE IF ( TYPE .EQ. '_BYTE' ) THEN
        CALL ARR_REG1B( BASE, INCR, NDAT, ARR, STATUS )
      ELSE
        STATUS = SAI__ERROR
        CALL MSG_SETC( 'T', TYPE )
        CALL ERR_REP( 'ARR_REG1T_T', 'Illegal type specified ^T',
     :                STATUS )

      END IF

*  Trap status
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ARR_REG1T', STATUS )

      END
