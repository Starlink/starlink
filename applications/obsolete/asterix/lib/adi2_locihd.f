      SUBROUTINE ADI2_LOCIHD( FID, IHDU, HID, STATUS )
*+
*  Name:
*     ADI2_LOCIHD

*  Purpose:
*     Locate an HDU description by number

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_LOCIHD( ID, IHDU, HID, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the FITSfile object
*     IHDU = INTEGER (given)
*        HDU number in the file (1 = primary HDU)
*     HID = INTEGER (returned)
*        ADI identifier to FITShdu object describing the HDU
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

*  Arguments Given:
      INTEGER			FID
      INTEGER			IHDU

*  Arguments Returned:
      INTEGER			HID

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		HDUNAME
      CHARACTER*4		STR

      INTEGER			HCID			! HDU container
      INTEGER			NDIG			! Digits used in STR
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate file's HDU container
      CALL ADI_FIND( FID, 'Hdus', HCID, STATUS )

*  Write property name so that ADI can do number -> HDU name mapping
      CALL CHR_ITOC( IHDU, STR, NDIG )
      CALL ADI_CGET0C( FID, '.HDU_'//STR(:NDIG), HDUNAME, STATUS )

*  Locate the HDU
      CALL ADI_FIND( HCID, HDUNAME, HID, STATUS )

*  Release container
      CALL ADI_ERASE( HCID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_LOCIHD', STATUS )

      END
