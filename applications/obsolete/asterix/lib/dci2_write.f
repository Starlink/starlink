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

*  {machine}-specific features used:
*     {routine_machine_specifics}...

*  {DIY_prologue_heading}:
*     {DIY_prologue_text}

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

*  Local Variables:
      CHARACTER*20		DET			! Detector name
      CHARACTER*20		FILT			! Filter name
      CHARACTER*(DAT__SZLOC)	HLOC			! HEADER object
      CHARACTER*(DAT__SZLOC)	INLOC			! INSTRUMENT object
      CHARACTER*20		INSTRUM			!
      CHARACTER*20		MISSION			!

      LOGICAL			DOK, FOK, IOK, MOK	! Things present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      FID = ARGS(1)
      DETID = ARGS(2)
      OARG = ADI__NULLID

*  Extract each member and write appropriate keyword
      CALL ADI_THERE( DETID, 'Mission', MOK, STATUS )
      IF ( MOK ) THEN
        CALL ADI_CGET0C( DETID, 'Mission', MISSION, STATUS )
        CALL ADI2_PKEY0C( FID, 'TELESCOP', MISSION,
     :                    'Telescope (mission) name', STATUS )
      END IF
      CALL ADI_THERE( DETID, 'Instrument', IOK, STATUS )
      IF ( IOK ) THEN
        CALL ADI_CGET0C( DETID, 'Instrument', INSTRUM, STATUS )
        CALL ADI2_PKEY0C( FID, 'INSTRUME', INSTRUM,
     :                    'Instrument name', STATUS )
      END IF
      CALL ADI_THERE( DETID, 'Detector', DOK, STATUS )
      IF ( DOK ) THEN
        CALL ADI_CGET0C( DETID, 'Detector', DET, STATUS )
        CALL ADI2_PKEY0C( FID, 'DETNAM', DET,
     :                    'Detector name', STATUS )
      END IF
      CALL ADI_THERE( DETID, 'Filter', FOK, STATUS )
      IF ( FOK ) THEN
        CALL ADI_CGET0C( DETID, 'Filter', FILT, STATUS )
        CALL ADI2_PKEY0C( FID, 'FILTER', FILT,
     :                    'Instrument filter name', STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI2_WRITE', STATUS )

      END
