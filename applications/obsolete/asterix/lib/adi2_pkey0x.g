      SUBROUTINE ADI2_PKEY0<T>( FID, HDU, KEY, VALUE, COMNT, STATUS )
*+
*  Name:
*     ADI2_PKEY0<T>

*  Purpose:
*     Write a keyword value into the buffer structures

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ADI2_PKEY0<T>( FID, HDU, KEY, VALUE, COMNT, STATUS )

*  Description:
*     Write a keyword to the ADI object representing a FITS file. This
*     routine will write both the value and the comment, but if the comment
*     has value '*' then the keyword is a standard keyword and a standard
*     comment will be invented (and not stored in the buffer).

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITSfile object
*     HDU = CHARSCTER*(*) (given)
*        Logical HDU whose keyword this is. Blank for primary
*     KEY = CHARACTER*(*) (given)
*        Name of the keyword. Same as the FITS keyword name, so should be
*        less than 9 characters in length
*     VALUE = <TYPE> (given)
*        The value for the keyword
*     COMNT = CHARACTER*(*) (given)
*        The keyword comment
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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

*  References:
*     {routine_references}...

*  Keywords:
*     {routine_keywords}...

*  Copyright:
*     {routine_copyright}

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     2 Feb 1995 (DJA):
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
      INTEGER			FID			! File identifier
      CHARACTER*(*)		HDU			! HDU name
      CHARACTER*(*)		KEY			! Keyword name
      <TYPE>			VALUE			! Value of the keyword
      CHARACTER*(*)		COMNT			! Keyword comment

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Constants:
      {data_type} {constant_name} ! [constant_description]
      PARAMETER ( {constant_name} = {cons} )

*  Local Variables:
      {data_type} {name}[dimensions] ! [local_variable_description]

*  Internal References:
      {data_type} {internal_name} ! [internal_description]
      [internal_definition_statement]...

*  Local Data:
      DATA {data_elm} / {data_values}... /
      [data_stmt]...

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate the approriate place depending on the HDU value. Blank means
*  primary HDU
      IF ( HDU(1:1) .EQ. ' ' ) THEN
        CALL ADI_THERE( FID, 'PRIMARY', THERE, STATUS )
        IF ( .NOT. THERE ) THEN
          CALL ADI_CNEW0( FID, 'PRIMARY', 'STRUC', STATUS )
        END IF
        CALL ADI_FIND( FID, 'PRIMARY', HID, STATUS )
      ELSE
        CALL ADI_THERE( FID, 'EXTENSIONS', THERE, STATUS )
        IF ( .NOT. THERE ) THEN
          CALL ADI_CNEW0( FID, 'EXTENSIONS', 'STRUC', STATUS )
        END IF
        CALL ADI_FIND( FID, 'EXTENSIONS', EID, STATUS )
        CALL ADI_THERE( EID, HDU, THERE, STATUS )
        IF ( .NOT. THERE ) THEN

        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK )
     :        CALL AST_REXIT( 'ADI2_PKEY0<T>', STATUS )

      END
