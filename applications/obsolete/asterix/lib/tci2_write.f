      SUBROUTINE TCI2_WRITE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     TCI2_WRITE

*  Purpose:
*     Write hardware description to a FITS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI2_WRITE( NARG, ARGS, OARG, STATUS )

*  Description:
*     Write data to a FITS describing timing information

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
*     TCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/tci.html

*  Keywords:
*     package:tci, usage:private

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
          CALL TCI2_WRITE1( NARG, ARGS, ' ', STATUS )
          CALL TCI2_WRITE1( NARG, ARGS, 'SPECTRUM', STATUS )

*    OGIP response matrix?
        ELSE IF ( CONTNT .EQ. 'MATRIX' ) THEN

          CALL TCI2_WRITE1( NARG, ARGS, 'MATRIX', STATUS )
          CALL TCI2_WRITE1( NARG, ARGS, 'EBOUNDS', STATUS )

        END IF

*  Write data to primary HDU if CONTENT not defined
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI2_WRITE', STATUS )

      END



      SUBROUTINE TCI2_WRITE1( NARG, ARGS, HDUNAM, STATUS )
*+
*  Name:
*     TCI2_WRITE1

*  Purpose:
*     Write timing description to a specific FITS file HDU

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI2_WRITE1( NARG, ARGS, HDUNAM, STATUS )

*  Description:
*     Write the data to a FITS file describing timing

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
*     TCI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/tci.html

*  Keywords:
*     package:tci, usage:private

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
      CHARACTER*8		DSTR, TSTR		! Date and time strings

      INTEGER			FID, TIMID		! Extracted arguments

      LOGICAL			MOK			! Things present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FID = ARGS(1)
      TIMID = ARGS(2)

*  Start date/time of observation
      CALL ADI_THERE( TIMID, 'MJDObs', MOK, STATUS )
      IF ( MOK ) THEN
        CALL ADI_CGET0D( DETID, 'MJDObs', MJD, STATUS )
        CALL TCI_MJD2DT( MJD, DSTR, TSTR, STATUS )
        CALL ADI2_PKEY0D( FID, HDUNAM, 'MJD-OBS', MJD,
     :                    'MJD of the data start', STATUS )
        CALL ADI2_PKEY0C( FID, HDUNAM, 'DATE-OBS', DSTR,
     :                    'Date of data start', STATUS )
        CALL ADI2_PKEY0C( FID, HDUNAM, 'TIME-OBS', TSTR,
     :                    'Time of data start', STATUS )
      END IF

*  Clock reference date/time
      CALL ADI_THERE( TIMID, 'MJDRef', MOK, STATUS )
      IF ( MOK ) THEN
        CALL ADI_CGET0D( DETID, 'MJDRef', MJD, STATUS )
        CALL TCI_MJD2DT( MJD, DSTR, TSTR, STATUS )
        CALL ADI2_PKEY0D( FID, HDUNAM, 'MJDREF', MJD,
     :                    'MJD SC clock start', STATUS )
        CALL ADI2_PKEY0C( FID, HDUNAM, 'ZERODATE', DSTR,
     :                    'Date of data start', STATUS )
        CALL ADI2_PKEY0C( FID, HDUNAM, 'ZEROTIME', TSTR,
     :                    'Time of data start', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI2_WRITE1', STATUS )

      END
