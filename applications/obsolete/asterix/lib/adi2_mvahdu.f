      SUBROUTINE ADI2_MVAHDU( ID, LUN, IHDU, HDUTYP, STATUS )
*+
*  Name
*     ADI2_MVAHDU

*  Purpose:
*     Move to absolute HDU number

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_MVAHDU( ID, LUN, IHDU, HDUTYP, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of FITSfile object
*     LUN = INTEGER (given)
*        Logical unit for FITSIO
*     IHDU = INTEGER (given)
*        Absolute HDU number required
*     HDUTYP = INTEGER (returned)
*        HDU type of HDU
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
*     11 Sep 1995 (DJA):
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
      INTEGER			ID, LUN, IHDU

*  Arguments Returned:
      INTEGER			HDUTYP

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			CHDU			! Current HDU number
      INTEGER			FSTAT			! FITSIO status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get current HDU number
      CALL ADI_CGET0I( ID, '.CurHdu', CHDU, STATUS )

*  Different?
      IF ( IHDU .EQ. CHDU ) THEN

        HDUTYP = -1

      ELSE

*    Clear FITSIO status
        FSTAT = 0

*    Try to move to HDU
        CALL FTMAHD( LUN, IHDU, HDUTYP, FSTAT )
        IF ( FSTAT .NE. 0 ) THEN
          CALL ADI2_FITERP( FSTAT, STATUS )

*    Update current HDU number
        ELSE
          CALL ADI_CPUT0I( ID, '.CurHdu', IHDU, STATUS )

        END IF

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_MVAHDU', STATUS )

      END
