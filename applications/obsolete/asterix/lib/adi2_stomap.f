      SUBROUTINE ADI2_STOMAP( PSID, HDUID, FORM, PTR, TYPE, MODE,
     :                        STATUS )
*+
*  Name:
*     ADI2_STOMAP

*  Purpose:
*     Store various things to do with a mapped FITS item

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_STOMAP( PSID, HDUID, FORM, PTR, TYPE, MODE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     PSID = INTEGER (given)
*        Item's private storage
*     HDUID = INTEGER (given)
*        The HDU in which the object belongs
*     FORM = CHARACTER*(*) (given)
*        The form of the mapping. I (image), BC (binary column), K
*        (keyword), KC (keyword comment)
*     PTR = INTEGER (given)
*        Address of mapped memory
*     TYPE = CHARACTER*(*) (given)
*        Data access type
*     MODE = CHARACTER*(*) (given)
*        Memory access mode
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     30 Aug 1995 (DJA):
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
      INTEGER			PSID, HDUID, PTR
      CHARACTER*(*)		FORM, TYPE, MODE

*  Status:
      INTEGER 			STATUS             	! Global status

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write reference to the HDU
      CALL ADI_CPUTREF( PSID, 'Hdu', HDUID, STATUS )

*  Flag HDU as changed if the mode is other than READ
      IF ( MODE .NE. 'READ' ) THEN
        CALL ADI_CPUT0L( HDUID, 'Changed', .TRUE., STATUS )
        IF ( (FORM(1:1) .NE. 'K') .AND. (FORM .NE. 'X') ) THEN
          CALL ADI_CPUT0L( HDUID, 'DataChanged', .TRUE., STATUS )
        END IF
      END IF

*  The mapping form
      CALL ADI_CPUT0C( PSID, 'Form', FORM, STATUS )

*  Store the various items in the storage block
      CALL ADI_CNEWV0I( PSID, 'Ptr', PTR, STATUS )
      CALL ADI_CNEWV0I( PSID, 'MapCount', 1, STATUS )
      CALL ADI_CPUT0C( PSID, 'Type', TYPE(:CHR_LEN(TYPE)), STATUS )
      CALL ADI_CPUT0C( PSID, 'Mode', MODE(:CHR_LEN(MODE)), STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_STOMAP', STATUS )

      END
