      SUBROUTINE TCI1_READ( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     TCI1_READ

*  Purpose:
*     Read timing info from HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI1_READ( NARG, ARGS, OARG, STATUS )

*  Description:
*     Load the data from an HDS file describing the timing of an
*     observation.

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
*     TCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/tci.html

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
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER			NARG			! # of input arguments
      INTEGER			ARGS(NARG)		! Input arguments

*  Arguments Returned:
      INTEGER			OARG			! Output structure

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	HLOC			! HEADER object

      DOUBLE PRECISION		UTC			! BASE_UTC value

      INTEGER			IMJD			! BASE_MJD value

      LOGICAL			UTCOK, MJDOK		! Things present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate HEADER structure
      CALL ADI1_LOCHEAD( ARGS(1), .FALSE., HLOC, STATUS )

*  The new object
      CALL ADI_NEW0( 'TimingInfo', OARG, STATUS )

*  Simple conditional copies
      CALL ADI1_CCH2AR( HLOC, 'EXPOSURE', OARG, 'Exposure', STATUS )
      CALL ADI1_CCH2AR( HLOC, 'EFF_EXPOSURE', OARG, 'EffExposure',
     :                  STATUS )
      CALL ADI1_CCH2AD( HLOC, 'BASE_TAI', OARG, 'TAIObs', STATUS )
      CALL ADI1_CCH2AD( HLOC, 'OBS_LENGTH', OARG, 'ObsLength', STATUS )

*  Look for the various header components
      CALL ADI1_CGET0I( HLOC, 'BASE_MJD', MJDOK, IMJD, STATUS )
      CALL ADI1_CGET0D( HLOC, 'BASE_UTC', UTCOK, UTC, STATUS )

*  Default for BASE_UTC
      IF ( .NOT. UTCOK ) UTC = 0D0

*  Write its member values
      IF ( MJDOK ) CALL ADI_CPUT0D( OARG, 'MJDObs', DBLE(IMJD) +
     :                              UTC/86400D0, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI1_READ', STATUS )

      END
