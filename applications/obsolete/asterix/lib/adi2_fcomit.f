      SUBROUTINE ADI2_FCOMIT( FID, STATUS )
*+
*  Name:
*     ADI2_FCOMIT

*  Purpose:
*     Commit buffer changes to a FITSfile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FCOMIT( FID, STATUS )

*  Description:
*     Commit any changes to keywords or data to the FITS file on disk. The
*     file is not closed.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the FITSfile object
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
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			FID			! FITSfile identifier

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			NHDU			! HDU count
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract logical unit
      CALL ADI_CGET0I( FID, '.NHDU', NHDU, STATUS )

*  Commit units and keywords
      CALL ADI2_CHKPRV( FID, NHDU, .TRUE., STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FCOMIT', STATUS )

      END


      SUBROUTINE ADI2_FCOMIT_HDU( FID, HID, STATUS )
*+
*  Name:
*     ADI2_FCOMIT_HDU

*  Purpose:
*     Commit buffer changes to a FITSfile object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FCOMIT_HDU( FID, HID, STATUS )

*  Description:
*     Commit any changes to keywords or data to the FITS file on disk. The
*     file is not closed.

*  Arguments:
*     FID = INTEGER (given)
*        ADI identifier of the FITSfile object
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
      INCLUDE 'ADI_PAR'

*  Arguments Given:
      INTEGER			FID			! FITSfile identifier
      INTEGER			HID			! HDU identifier

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*132		CMT			! Keyword comment
      CHARACTER*20		CLASS			! Keyword value class
      CHARACTER*132		CVALUE			! Keyword value
      CHARACTER*8		KEY			! Keyword name

      DOUBLE PRECISION		DVALUE			! Keyword value

      INTEGER			FSTAT			! FITSIO status
      INTEGER			HID			! HDU object
      INTEGER			IKEY			! Loop over keys
      INTEGER			KID			! Keyword object
      INTEGER			LUN			! Logical unit number
      INTEGER			NAXES(ADI__MXDIM)	! Dimensions
      INTEGER			NAXIS			! Dimensionality
      INTEGER			NKEY			! # of keywords

      LOGICAL			COMIT			! Value committed?
      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract logical unit
      CALL ADI_CGET0I( FID, '.LUN', LUN, STATUS )

*  Get number of keywords
      CALL ADI_NCMP( HID, NKEY, STATUS )
      DO IKEY = 1, NKEY
        CALL ADI_INDCMP( HID, IKEY, KID, STATUS )
        CALL ADI_THERE( KID, '.COMMITTED', COMIT, STATUS )
        IF ( .NOT. COMIT ) THEN
          CALL ADI_NAME( KID, KEY, STATUS )
          CALL ADI_THERE( KID, '.COMMENT', THERE, STATUS )
          IF ( THERE ) THEN
            CALL ADI_CGET0C( KID, '.COMMENT', CMT, STATUS )
          ELSE
            CALL ADI2_STDCMT( KEY, CMT, STATUS )
          END IF
          CALL ADI_CLASS( KID, CLASS, STATUS )
          IF ( CLASS(1:1) .EQ. 'D' ) THEN
            CALL ADI_GET0D( KID, DVALUE, STATUS )
            CALL FTPKYG( LUN, KEY, DVALUE, 8, CMT, FSTAT )
          ELSE
            CALL ADI_GET0C( KID, CVALUE, STATUS )
            CALL FTPKYS( LUN, KEY, CVALUE, CMT, FSTAT )
          END IF
          CALL ADI_CPUT0L( KID, '.COMMITTED', .TRUE., STATUS )
        END IF
        CALL ADI_ERASE( KID, STATUS )
      END DO

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'ADI2_FCOMIT_HDU', STATUS )
      END IF

      END
