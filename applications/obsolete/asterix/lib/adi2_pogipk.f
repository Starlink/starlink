      SUBROUTINE ADI2_POGIPK( FID, HDU, HC1, HV1, HC2, HV2, HC3, HV3,
     :                        STATUS )
*+
*  Name:
*     ADI2_POGIPK

*  Purpose:
*     Write OGIP standard keywords

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_POGIPK( FID, HDU, HC1, HV1, HC2, HV2, HC3, HV3, STATUS )

*  Description:
*     Write OGIP HDU classification keywords.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of FITS file object
*     HDU = CHARACTER*(*)
*        HDU name to write keywords
*     HC1 = CHARACTER*(*) (given)
*        Value of the HDUCLAS1 keyword
*     HV1 = CHARACTER*(*) (given)
*        Value (if any) of the HDUVERS1 keyword
*     HC2 = CHARACTER*(*) (given)
*        Value of the HDUCLAS2 keyword
*     HV2 = CHARACTER*(*) (given)
*        Value (if any) of the HDUVERS2 keyword
*     HC3 = CHARACTER*(*) (given)
*        Value of the HDUCLAS3 keyword
*     HV3 = CHARACTER*(*) (given)
*        Value (if any) of the HDUVERS3 keyword
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
*     28 Feb 1995 (DJA):
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
      INTEGER			FID			! See above
      CHARACTER*(*)		HDU			! HDU name
      CHARACTER*(*)		HC1, HV1		! Level 1
      CHARACTER*(*)		HC2, HV2		! Level 2
      CHARACTER*(*)		HC3, HV3		! Level 3

*  Arguments Returned:
      INTEGER 			STATUS             	! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write the keyword that shows this extension is OGIP defined
      CALL ADI2_PKEY0C( FID, HDU, 'HDUCLASS', 'OGIP',
     :                  'Format conforms to OGIP standard', STATUS )

*  Set values if non-blank
      IF ( HC1 .NE. ' ' ) THEN
        IF ( HC1 .EQ. 'RESPONSE' ) THEN
          CMT = 'dataset relates to spectral response'
        ELSE
          CMT = '*'
        END IF
        CALL ADI2_PKEY0C( FID, HDU, 'HDUCLAS1', HC1, CMT, STATUS )
        IF ( HV1 .NE. ' ' ) THEN
          CALL ADI2_PKEY0C( FID, HDU, 'HDUVERS1', HV1,
     :         'Version of family of formats', STATUS )
        END IF
      END IF
      IF ( HC2 .NE. ' ' ) THEN
        CALL ADI2_PKEY0C( FID, HDU, 'HDUCLAS2', HC2,
     :         'Version of format', STATUS )
        IF ( HV2 .NE. ' ' ) THEN
          CALL ADI2_PKEY0C( FID, HDU, 'HDUVERS2', HV2,
     :         'Version of format', STATUS )
        END IF
      END IF
      IF ( HC3 .NE. ' ' ) THEN
        CALL ADI2_PKEY0C( FID, HDU, 'HDUCLAS3', HC3, '*', STATUS )
        IF ( HV3 .NE. ' ' ) THEN
          CALL ADI2_PKEY0C( FID, HDU, 'HDUVERS3', HV3, '*', STATUS )
        END IF
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_POGIPK', STATUS )

      END
