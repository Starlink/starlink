      SUBROUTINE FRI1_TRNSNM( SID, NAME, STATUS )
*+
*  Name:
*     FRI1_TRNSNM

*  Purpose:
*     Translate logical file link name into HDS component name

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL FRI1_TRNSNM( SID, NAME, STATUS )

*  Description:
*     Translates a logical file link name into HDS component name. This
*     is generally done by appending "REF" to the logical name, except
*     in the case of BGND where the name BGREF has been in use long
*     before this library was written.

*  Arguments:
*     SID = INTEGER (given)
*        ADI string object
*     NAME = CHARACTER*(*) (returned)
*        HDS component name
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
*     FRI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/fri.html

*  Keywords:
*     package:fri, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     27 Jul 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   SID

*  Arguments Returned:
      CHARACTER*(*)		NAME

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*(DAT__SZNAM)	ROBJ			! Reference obj name

      INTEGER			RLEN			! Used length of ROBJ
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct the name of the reference object
      CALL ADI_GET0C( SID, ROBJ, STATUS )

*  Ok so far?
      RLEN = CHR_LEN(ROBJ)
      IF ( (RLEN.GT.0) .AND. (STATUS .EQ. SAI__OK) ) THEN

*    Swap logical name for HDS name if required
        IF ( ROBJ(:RLEN) .EQ. 'BGND' ) THEN
          RLEN = 2
        END IF

*    Concatenate REF to logical name
        NAME = ROBJ(:RLEN)//'REF'

*    Convert to upper case
        CA;; CHR_UCASE( NAME )

*  Duff string?
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid reference logical name string',
     :                STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'FRI1_TRNSNM', STATUS )

      END
