      SUBROUTINE DCI1_READ( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     DCI1_READ

*  Purpose:
*     Read hardware description from HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DCI1_READ( NARG, ARGS, OARG, STATUS )

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
*     DCI Subroutine Guide : http://www.sr.bham.ac.uk:8080/asterix-docs/Programmer/Guides/dci.html

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
      CHARACTER*(DAT__SZLOC)	INLOC			! INSTRUMENT object
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID

*  Locate HEADER structure
      CALL ADI1_LOCHEAD( ARGS(1), .FALSE., HLOC, STATUS )

*  The new object
      CALL ADI_NEW0( 'MissionStrings', OARG, STATUS )

*  Transfer HDS info to ADI object
      CALL ADI1_CCH2AC( HLOC, 'INSTRUMENT', OARG, 'Instrument', STATUS )
      CALL ADI1_CCH2AC( HLOC, 'OBSERVATORY', OARG, 'Mission', STATUS )
      CALL ADI1_CCH2AC( HLOC, 'TARGET', OARG, 'Target', STATUS )
      CALL ADI1_CCH2AC( HLOC, 'OBSERVER', OARG, 'Observer', STATUS )

*  Locate INSTRUMENT structure
      CALL ADI1_LOCINSTR( ARGS(1), .FALSE., INLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        CALL ADI1_CCH2AC( INLOC, 'DETECTOR', OARG, 'Detector', STATUS )
        CALL ADI1_CCH2AC( INLOC, 'FILTER', OARG, 'Filter', STATUS )
      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI1_READ', STATUS )

      END
