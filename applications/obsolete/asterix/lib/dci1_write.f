      SUBROUTINE DCI1_WRITE( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     DCI1_WRITE

*  Purpose:
*     Write hardware description from HDS file

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DCI1_WRITE( NARG, ARGS, OARG, STATUS )

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

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	HLOC			! HEADER object
      CHARACTER*(DAT__SZLOC)	INLOC			! INSTRUMENT object

      LOGICAL			DOK, FOK		! Things present?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OARG = ADI__NULLID

*  Extract argument
      MID = ARGS(2)

*  Locate HEADER structure, creating if necessary
      CALL ADI1_LOCHEAD( ARGS(1), .TRUE., HLOC, STATUS )

*  Conditional copies from ADI to HDS
      CALL ADI1_CCA2HC( MID, 'Mission', HLOC, 'OBSERVATORY', STATUS )
      CALL ADI1_CCA2HC( MID, 'Instrument', HLOC, 'INSTRUMENT', STATUS )
      CALL ADI1_CCA2HC( MID, 'Target', HLOC, 'TARGET', STATUS )
      CALL ADI1_CCA2HC( MID, 'Observer', HLOC, 'OBSERVER', STATUS )

*  Is either detector or filter specified?
      CALL ADI_THERE( MID, 'Detector', DOK, STATUS )
      CALL ADI_THERE( MID, 'Filter', FOK, STATUS )
      IF ( FOK .OR. DOK ) THEN

*    Locate INSTRUMENT structure, creating if necessary
        CALL ADI1_LOCINSTR( ARGS(1), .TRUE., ILOC, STATUS )

*    Copy detector and filter
        CALL ADI1_CCA2HC( MID, 'Detector', ILOC, 'DETECTOR', STATUS )
        CALL ADI1_CCA2HC( MID, 'Filter', ILOC, 'FILTER', STATUS )

*    Also write FILTER to header
        CALL ADI1_CCA2HC( MID, 'Filter', HLOC, 'FILTER', STATUS )

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI1_WRITE', STATUS )

      END
