      SUBROUTINE UDI0_CHKTYP( TYPE, LTYPE, STATUS )
*+
*  Name:
*     UDI0_CHKTYP

*  Purpose:
*     Check type name supplied is a valid type

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL UDI0_CHKTYP( TYPE, LTYPE, STATUS )

*  Description:
*     Check type name supplied is a valid numeric type. If the leading
*     character is an underscore then the type name is assumed to be an
*     HDS style one which we'll forgive for the moment.

*  Arguments:
*     TYPE = CHARACTER*(*) (given)
*        The type name to validate
*     LTYPE = CHARACTER*(*) (returned)
*        The validate upper case ADI type name
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
*     package:udi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     9 Aug 1995 (DJA):
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
      CHARACTER*(*)		TYPE

*  Arguments Returned:
      CHARACTER*(*)		LTYPE

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_INSET
        LOGICAL			CHR_INSET

*  Local Constants:
      CHARACTER*70		TYPES
        PARAMETER		( TYPES =
     :      'UBYTE,BYTE,UWORD,WORD,INTEGER,REAL,DOUBLE,LOGICAL,CHAR' )

*  Local Variables:
      INTEGER			FC			! First good character
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Trap leading underscore
      FC = 1
      IF ( TYPE(1:1) .EQ. '_' ) FC = 2

*  Make copy
      LTYPE = TYPE(FC:)

*  Coerce to upper case
      CALL CHR_UCASE( LTYPE )

*  Check its allowed
      IF ( .NOT. CHR_INSET( TYPES, LTYPE ) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( 'UDI0_CHKTYP', 'Object type supplied is not '/
     :     /' valid, must be one of '//TYPES, STATUS )
      END IF

      END
