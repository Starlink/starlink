      SUBROUTINE TCI1_WRITE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     TCI1_WRITE

*  Purpose:
*     Write timing info to HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TCI1_WRITE( NARG, ARGS, OARG, STATUS )

*  Description:
*     Writes details of timing to an HDS dataset

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

      DOUBLE PRECISION		DMJD			! D.P. MJD from TIMID

      INTEGER			IMJD			! BASE_MJD value
      INTEGER			TIMID			! Timing object

      LOGICAL			THERE			! Object exists?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate HEADER structure, create if necessary
      CALL ADI1_LOCHEAD( ARGS(1), .TRUE., HLOC, STATUS )

*  The timing data
      TIMID = ARGS(2)

*  Extract the various members of TimingInfo writing the HDS header components
      CALL ADI_THERE( TIMID, 'MJDObs', THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CGET0D( TIMID, 'MJDObs', DMJD, STATUS )
        IMJD = INT(DMJD)
        CALL HDX_PUTI( HLOC, 'BASE_MJD', 1, IMJD, STATUS )
        CALL HDX_PUTD( HLOC, 'BASE_UTC', 1,
     :                 DBLE(DMJD-DBLE(IMJD))*86400D0, STATUS )
      END IF

*  Conditional copying for the rest
      CALL ADI1_CCA2HR( TIMID, 'Exposure', HLOC, 'EXPOSURE', STATUS )
      CALL ADI1_CCA2HR( TIMID, 'EffExposure', HLOC, 'EFF_EXPOSURE',
     :                 STATUS )
      CALL ADI1_CCA2HD( TIMID, 'TAIObs', HLOC, 'BASE_TAI', STATUS )
      CALL ADI1_CCA2HR( TIMID, 'ObsLength', HLOC, 'OBS_LENGTH', STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI1_WRITE', STATUS )

      END
