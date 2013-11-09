      SUBROUTINE HSI2_GETCTR( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     HSI2_GETCTR

*  Purpose:
*     Construct history control data from dataset

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL HSI2_GETCTR( NARG, ARGS, OARG, STATUS )

*  Description:
*     Constructs the format independent HistoryControl object for the
*     specified dataset.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     OARG = INTEGER (returned)
*        Output data
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
*     HSI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/hsi.html

*  Keywords:
*     package:hsi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     RB: Richard Beard (ROSAT, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     04 Feb 1997 (RB):
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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER                   NARG                    ! # arguments
      INTEGER                   ARGS(*)                 ! Method arguments

*  Arguments Returned:
      INTEGER                   OARG                    ! Returned data

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*8		DATE, TIME

      INTEGER			NREC			! Number of records
      INTEGER			HDUID, HDUTYPE
      INTEGER			LUN, CACHEID

      LOGICAL			DIDCRE
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract locator of dataset
      CALL ADI2_FNDHDU( ARGS(1), ' ', .FALSE., HDUID, STATUS )

*  Create control object
      CALL ADI_NEW0( 'HistoryControl', OARG, STATUS )

*  How many HDUs can we step through?
      CALL ADI2_GETLUN( ARGS(1), LUN, STATUS )
      NREC = 0
      DO WHILE ( STATUS .NE. SAI__ERROR )
        NREC = NREC + 1
        CALL ADI2_MVAHDU( ARGS(1), LUN, NREC, HDUTYPE, STATUS )
      END DO
      NREC = NREC - 1
      CALL ERR_ANNUL( STATUS )

*  Fill in member data
      CALL ADI_CPUT0I( OARG, 'NRECORD', NREC, STATUS )
      CALL ADI2_CFIND( ARGS(1), ' ', '.DATE', ' ', .FALSE., .FALSE.,
     :                 'CHAR', 0, 0, DIDCRE, CACHEID, STATUS )
      IF ( CACHEID .EQ. ADI__NULLID ) THEN
        DATE = 'no_date'
      ELSE
        CALL ADI_CGET0C( CACHEID, 'Value', DATE, STATUS )
        CALL ADI_ERASE( CACHEID, STATUS )
      END IF
      CALL ADI2_CFIND( ARGS(1), ' ', '.TIME', ' ', .FALSE., .FALSE.,
     :                 'CHAR', 0, 0, DIDCRE, CACHEID, STATUS )
      IF ( CACHEID .EQ. ADI__NULLID ) THEN
        TIME = 'no_time'
      ELSE
        CALL ADI_CGET0C( CACHEID, 'Value', TIME, STATUS )
        CALL ADI_ERASE( CACHEID, STATUS )
      END IF
      CALL ADI_CPUT0C( OARG, 'Date', DATE//' '//TIME, STATUS )

*  Verbosity is not defined for FITS files
      CALL ADI_CPUT0C( OARG, 'Verbosity', 'undefined', STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'HSI2_GETCTR', STATUS )

      END
