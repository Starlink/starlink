      SUBROUTINE DCI2_WRITE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     DCI2_WRITE

*  Purpose:
*     Write hardware description to a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DCI2_WRITE( NARG, ARGS, OARG, STATUS )

*  Description:
*     Load the data from an HDS file describing the mission, instrument,
*     detector and filter the data was produced by.

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
*     DCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/dci.html

*  Keywords:
*     package:dci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     6 Mar 1995 (DJA):
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
      INTEGER			NARG			! # of input arguments
      INTEGER			ARGS(NARG)		! Input arguments

*  Arguments Returned:
      INTEGER			OARG			! Output structure

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*8		CONTNT			! Content of file
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID

*  Write to SPECTRUM too
      CALL ADI2_GKEY0C( ARGS(1), ' ', 'CONTENT', .FALSE., .FALSE.,
     :                  CONTNT, ' ', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    OGIP spectrum?
        IF ( CONTNT .EQ. 'SPECTRUM' ) THEN

*      Write data to primary HDU and SPECTRUM extension
          CALL DCI2_WRITE1( NARG, ARGS, ' ', STATUS )
          CALL DCI2_WRITE1( NARG, ARGS, 'SPECTRUM', STATUS )

*    OGIP response matrix?
        ELSE IF ( CONTNT .EQ. 'MATRIX' ) THEN

          CALL DCI2_WRITE1( NARG, ARGS, 'MATRIX', STATUS )
          CALL DCI2_WRITE1( NARG, ARGS, 'EBOUNDS', STATUS )

        END IF

*  Write data to primary HDU if CONTENT not defined
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI2_WRITE', STATUS )

      END



      SUBROUTINE DCI2_WRITE1( NARG, ARGS, HDUNAM, STATUS )
*+
*  Name:
*     DCI2_WRITE1

*  Purpose:
*     Write hardware description to a specific FITS file HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DCI2_WRITE1( NARG, ARGS, HDUNAM, STATUS )

*  Description:
*     Write the data from an HDS file describing the mission, instrument,
*     detector and filter the data was produced by.

*  Arguments:
*     NARG = INTEGER (given)
*        Number of method arguments
*     ARGS(*) = INTEGER (given)
*        ADI identifier of method arguments
*     HDUNAM = CHARACTER*(*) (given)
*        HDU to write to
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
*     DCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/dci.html

*  Keywords:
*     package:dci, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     6 Mar 1995 (DJA):
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
      INTEGER			NARG			! # of input arguments
      INTEGER			ARGS(NARG)		! Input arguments
      CHARACTER*(*)		HDUNAM			! HDU name

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*20		DET			! Detector name
      CHARACTER*20		FILT			! Filter name
      CHARACTER*20		INSTRUM			!
      CHARACTER*20		MISSION			!

      INTEGER			FID, DETID		! Extracted arguments

      LOGICAL			DOK, FOK, IOK, MOK	! Things present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FID = ARGS(1)
      DETID = ARGS(2)

*  Extract each member and write appropriate keyword
      CALL ADI_THERE( DETID, 'Mission', MOK, STATUS )
      IF ( MOK ) THEN
        CALL ADI_CGET0C( DETID, 'Mission', MISSION, STATUS )
        CALL ADI2_PKEY0C( FID, HDUNAM, 'TELESCOP', MISSION,
     :                    'Telescope (mission) name', STATUS )
      END IF
      CALL ADI_THERE( DETID, 'Instrument', IOK, STATUS )
      IF ( IOK ) THEN
        CALL ADI_CGET0C( DETID, 'Instrument', INSTRUM, STATUS )
        CALL ADI2_PKEY0C( FID, HDUNAM, 'INSTRUME', INSTRUM,
     :                    'Instrument name', STATUS )
      END IF
      CALL ADI_THERE( DETID, 'Detector', DOK, STATUS )
      IF ( DOK ) THEN
        CALL ADI_CGET0C( DETID, 'Detector', DET, STATUS )
        CALL ADI2_PKEY0C( FID, HDUNAM, 'DETNAM', DET,
     :                    'Detector name', STATUS )
      END IF
      CALL ADI_THERE( DETID, 'Filter', FOK, STATUS )
      IF ( FOK ) THEN
        CALL ADI_CGET0C( DETID, 'Filter', FILT, STATUS )
        CALL ADI2_PKEY0C( FID, HDUNAM, 'FILTER', FILT,
     :                    'Instrument filter name', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI2_WRITE1', STATUS )

      END
