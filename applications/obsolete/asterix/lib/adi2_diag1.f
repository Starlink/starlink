      SUBROUTINE ADI2_DIAG1( ID, STATUS )
*+
*  Name:
*     ADI2_DIAG1

*  Purpose:
*     Diagnostic dump of state of ADI FitsFile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_DIAG1( ID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of FitsFile object
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
*     14 Jun 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
c      INCLUDE '{global_constants_file}' ! [global_constants_description]

*  Global Variables:
c      INCLUDE '{global_variables_file}' ! [global_variables_description]
*        {global_name}[dimensions] = {data_type} ({global_access_mode})
*           [global_variable_purpose]

*  Arguments Given:
      INTEGER			ID			!

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		HNAME			! HDU name
      CHARACTER*2		STR			!

      INTEGER			HID			! HDU identifier
      INTEGER			IHDU			! Loop over HDUs
      INTEGER			NDIG			! Digits used in STR
      INTEGER			NHDU			! # HDU's in definition
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get number of HDU's
      CALL ADI_CGET0I( ID, 'Nhdu', NHDU, STATUS )
	call adi_print( id, status )

*  Loop over HDUs
      DO IHDU = 1, NHDU

*    Get property defining name of the IHDU'th HDU
        IF ( IHDU .EQ. 1 ) THEN
          CALL CHR_ITOC( IHDU, STR, NDIG )
          CALL ADI_CGET0C( ID, '.HDU_'//STR(:NDIG), HNAME, STATUS )
        ELSE
          HNAME = 'PRIMARY'
        END IF

*    Locate the HDU
        CALL ADI_FIND( ID, HNAME, HID, STATUS )

*    Free HDU handle
        CALL ADI_ERASE( HID, STATUS )

      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_DIAG1', STATUS )

      END
