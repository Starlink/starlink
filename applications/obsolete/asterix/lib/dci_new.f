      SUBROUTINE DCI_NEW( MISSION, INSTR, DET, FILTER,
     :                    TARGET, OBSRVR, DETID, STATUS )
*+
*  Name:
*     DCI_NEW

*  Purpose:
*     Create a new detector configuration object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL DCI_GETID( MISSION, INSTR, DET, FILTER, TARGET, OBSRVR, DETID, STATUS )

*  Description:
*     Returns an object describing the hardware configuration associated
*     with the specified dataset. This describes the satellite, instrument,
*     detector and filter info.

*  Arguments:
*     MISSION = CHARACTER*(*) (given)
*        The mission name, may be blank
*     INSTR = CHARACTER*(*) (given)
*        The instrument name, may be blank
*     DET = CHARACTER*(*) (given)
*        The detector name, may be blank
*     FILTER = CHARACTER*(*) (given)
*        The filter name, may be blank
*     TARGET = CHARACTER*(*) (given)
*        The target name, may be blank
*     OBSRVR = CHARACTER*(*) (given)
*        The observers name, may be blank
*     DETID = INTEGER (returned)
*        ADI identifier of detector configuration data
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
*     package:dci, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     20 Dec 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PKG'

*  Arguments Given:
      CHARACTER*(*)		MISSION, INSTR, DET, FILTER
      CHARACTER*(*)		TARGET, OBSRVR

*  Arguments Returned:
      INTEGER                   DETID                   ! Detector info

*  Status:
      INTEGER                   STATUS                  ! Global status

*  External References:
      EXTERNAL			AST_QPKGI
        LOGICAL			AST_QPKGI
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check initialised
      IF ( .NOT. AST_QPKGI( DCI__PKG ) ) CALL DCI0_INIT( STATUS )

*  Create nnew object
      CALL ADI_NEW0( 'MissionStrings', DETID, STATUS )

*  Write strings if given
      IF ( MISSION .GT. ' ' ) THEN
        CALL ADI_CPUT0C( DETID, 'Mission', MISSION, STATUS )
      END IF
      IF ( INSTR .GT. ' ' ) THEN
        CALL ADI_CPUT0C( DETID, 'Instrument', INSTR, STATUS )
      END IF
      IF ( DET .GT. ' ' ) THEN
        CALL ADI_CPUT0C( DETID, 'Detector', DET, STATUS )
      END IF
      IF ( FILTER .GT. ' ' ) THEN
        CALL ADI_CPUT0C( DETID, 'Filter', FILTER, STATUS )
      END IF
      IF ( TARGET .GT. ' ' ) THEN
        CALL ADI_CPUT0C( DETID, 'Target', TARGET, STATUS )
      END IF
      IF ( OBSRVR .GT. ' ' ) THEN
        CALL ADI_CPUT0C( DETID, 'Observer', OBSRVR, STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'DCI_NEW', STATUS )

      END
