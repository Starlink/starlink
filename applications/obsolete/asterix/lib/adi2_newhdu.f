      SUBROUTINE ADI2_NEWHDU( FID, HDUNAME, STATUS )
*+
*  Name:
*     ADI2_NEWHDU

*  Purpose:
*     Create new HDU description block in FITSfile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_NEWHDU( ID, HDUNAME, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the FITSfile object
*     HDUNAME = CHARACTER*(*) (given)
*        HDU name
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
      CHARACTER*(*)		HDUNAME

*  Status:
      INTEGER 			STATUS             	! Global status

*  External references:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*20		LHDU
      CHARACTER*4		STR

      INTEGER			HCID			! HDU container
      INTEGER			HID			! New HDU
      INTEGER			HLEN
      INTEGER			I			! Loop over LHDU
      INTEGER			NDIG			! Digits used in STR
      INTEGER			NHDU			! # HDUs in file
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Make local copy of name, and translate embedded spaces to underscores
      LHDU = HDUNAME
      HLEN = CHR_LEN(LHDU)
      DO I = 1, HLEN
        IF ( LHDU(I:I) .EQ. ' ' ) LHDU(I:I) = '_'
      END DO

*  Locate file's HDU container
      CALL ADI_FIND( FID, 'Hdus', HCID, STATUS )

*  Create named component & locate it
      CALL ADI_CNEW0( HCID, LHDU(:HLEN), 'FITShdu', STATUS )
      CALL ADI_FIND( HCID, LHDU(:HLEN), HID, STATUS )

*  Set the HDU counter
      CALL ADI_CGET0I( FID, 'Nhdu', NHDU, STATUS )
      NHDU = NHDU + 1
      CALL ADI_CPUT0I( FID, 'Nhdu', NHDU, STATUS )

*  Store the HDUs self reference number
      CALL ADI_CPUT0I( HID, 'Ihdu', NHDU, STATUS )

*  Write property name so that ADI can do number -> HDU name mapping
      CALL CHR_ITOC( NHDU, STR, NDIG )
      CALL ADI_CPUT0C( FID, '.HDU_'//STR(:NDIG), LHDU(:HLEN), STATUS )

*  Write true name
      CALL ADI_CPUT0C( HID, 'Name', HDUNAME, STATUS )

*  Relase new HDU
      CALL ADI_ERASE( HID, STATUS )

*  Release container
      CALL ADI_ERASE( HCID, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_NEWHDU', STATUS )

      END
