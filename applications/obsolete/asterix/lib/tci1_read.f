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

      REAL			EXPO, EFEXPO		! Exposure times

      LOGICAL			EFFOK, EXPOK		! Things present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate HEADER structure
      CALL ADI1_LOCHEAD( ARGS(1), .FALSE., HLOC, STATUS )

*  The new object
      CALL ADI_NEW0( 'TimingInfo', OARG, STATUS )

*  Look for the various header components
      CALL ADI1_CGET0C( HLOC, 'EXPOSURE', EXPOK, EXPO, STATUS )
      CALL ADI1_CGET0C( HLOC, 'EFF_EXPOSURE', EFFOK, EFEXPO, STATUS )

*  Write its member values
      IF ( EXPOK ) THEN
        CALL ADI_CPUT0R( OARG, 'Exposure', EXPO, STATUS )
      END IF
      IF ( EFFOK ) THEN
        CALL ADI_CPUT0R( OARG, 'EffExposure', EFEXPO, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'TCI1_READ', STATUS )

      END
