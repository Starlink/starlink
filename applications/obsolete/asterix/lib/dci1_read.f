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
      CHARACTER*20		DET			! Detector name
      CHARACTER*20		FILT			! Filter name
      CHARACTER*(DAT__SZLOC)	HLOC			! HEADER object
      CHARACTER*(DAT__SZLOC)	INLOC			! INSTRUMENT object
      CHARACTER*20		INSTRUM			!
      CHARACTER*20		MISSION			!
      CHARACTER*50		OBSERVER		!
      CHARACTER*50		TARGET			!

      LOGICAL			DOK, FOK, IOK, MOK	! Things present?
      LOGICAL			TOK, OOK
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID

*  Locate HEADER structure
      CALL ADI1_LOCHEAD( ARGS(1), .FALSE., HLOC, STATUS )

*  Look for the various header components
      CALL ADI1_CGET0C( HLOC, 'OBSERVATORY', MOK, MISSION, STATUS )
      CALL ADI1_CGET0C( HLOC, 'INSTRUMENT', IOK, INSTRUM, STATUS )
      CALL ADI1_CGET0C( HLOC, 'TARGET', TOK, TARGET, STATUS )
      CALL ADI1_CGET0C( HLOC, 'OBSERVER', OOK, OBSERVER, STATUS )

*  If instrument was specified, look for detector and filter names
      IF ( IOK ) THEN
        CALL ADI1_LOCINSTR( ARGS(1), .FALSE., INLOC, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          CALL ADI1_CGET0C( INLOC, 'DETECTOR', DOK, DET, STATUS )
          CALL ADI1_CGET0C( INLOC, 'FILTER', FOK, FILT, STATUS )
        ELSE
          CALL ERR_ANNUL( STATUS )
        END IF
      END IF

*    The new object
      CALL ADI_NEW0( 'MissionStrings', OARG, STATUS )

*    Write its member values
      IF ( MOK ) THEN
        CALL ADI_CPUT0C( OARG, 'Mission',
     :                   MISSION(:CHR_LEN(MISSION)), STATUS )
      END IF
      IF ( IOK ) THEN
        CALL ADI_CPUT0C( OARG, 'Instrument',
     :                   INSTRUM(:CHR_LEN(INSTRUM)), STATUS )
      END IF
      IF ( DOK ) THEN
        CALL ADI_CPUT0C( OARG, 'Detector', DET(:CHR_LEN(DET)),
     :                   STATUS )
      END IF
      IF ( FOK ) THEN
        CALL ADI_CPUT0C( OARG, 'Filter', FILT(:CHR_LEN(FILT)),
     :                   STATUS )
      END IF
      IF ( TOK ) THEN
        CALL ADI_CPUT0C( OARG, 'Target', TARGET(:CHR_LEN(TARGET)),
     :                   STATUS )
      END IF
      IF ( OOK ) THEN
        CALL ADI_CPUT0C( OARG, 'Observer', OBSERVER(:CHR_LEN(OBSERVER)),
     :                   STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI1_READ', STATUS )

      END
