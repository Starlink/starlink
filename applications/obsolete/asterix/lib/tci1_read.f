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

*  External References:
      EXTERNAL			CHR_LEN
        INTEGER			CHR_LEN

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	HLOC			! HEADER object

      DOUBLE PRECISION		TAI			! BASE_TAI value
      DOUBLE PRECISION		UTC			! BASE_UTC value

      REAL			EXPO, EFEXP		! Exposure times
      REAL			OBSLEN			! Observation length

      INTEGER			IMJD			! BASE_MJD value

      LOGICAL			EFFOK, EXPOK		! Things present?
      LOGICAL			UTCOK, MJDOK, TAIOK	!
      LOGICAL			OBSOK			!
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate HEADER structure
      CALL ADI1_LOCHEAD( ARGS(1), .FALSE., HLOC, STATUS )

*  The new object
      CALL ADI_NEW0( 'TimingInfo', OARG, STATUS )

*  Look for the various header components
      CALL ADI1_CGET0R( HLOC, 'EXPOSURE', EXPOK, EXPO, STATUS )
      CALL ADI1_CGET0R( HLOC, 'EFF_EXPOSURE', EFFOK, EFEXP, STATUS )
      CALL ADI1_CGET0I( HLOC, 'BASE_MJD', MJDOK, IMJD, STATUS )
      CALL ADI1_CGET0D( HLOC, 'BASE_UTC', UTCOK, UTC, STATUS )
      CALL ADI1_CGET0D( HLOC, 'BASE_TAI', TAIOK, TAI, STATUS )
      CALL ADI1_CGET0D( HLOC, 'OBS_LENGTH', OBSOK, OBSLEN, STATUS )

*  Default for BASE_UTC
      IF ( .NOT. UTCOK ) UTC = 0D0

*  Write its member values
      IF ( EXPOK ) CALL ADI_CPUT0R( OARG, 'Exposure', EXPO, STATUS )
      IF ( EFFOK ) CALL ADI_CPUT0R( OARG, 'EffExposure', EFEXP, STATUS )
      IF ( MJDOK ) CALL ADI_CPUT0D( OARG, 'TAIObs', DBLE(IMJD) +
     :                              UTC/86400D0, STATUS )
      IF ( TAIOK ) CALL ADI_CPUT0D( OARG, 'MJDObs', TAI, STATUS )
      IF ( OBSOK ) CALL ADI_CPUT0R( OARG, 'ObsLength', OBSLEN, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI1_READ', STATUS )

      END
