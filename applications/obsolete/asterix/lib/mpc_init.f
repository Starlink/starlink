      SUBROUTINE MPC_INIT( VERSION, MAXCPU, ID, SLAVE, STATUS )
*+
*  Name:
*     MPC_INIT

*  Purpose:
*     Initialise for multi-processor processing

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL MPC_INIT( VERSION, MAXCPU, ID, SLAVE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     VERSION = CHARACTER*(*) (given)
*        Program version to start
*     MAXCPU = INTEGER (given)
*        Maximum number of cpu's to use
*     ID = INTEGER (returned)
*        ADI identifier of process control object
*     SLAVE = LOGICAL (returned)
*        This is a slave process
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
*     MPC Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/mpc.html

*  Keywords:
*     package:mpc, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Apr 1996 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER*(*)		VERSION
      INTEGER			MAXCPU

*  Arguments Returned:
      INTEGER			ID
      LOGICAL			SLAVE

*  Status:
      INTEGER			STATUS             	! Global status

*  Local Variables:
      INTEGER			MYCPU			! Process cpu #
      INTEGER			NCPU			! Actual # cpus
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create process control structure
      CALL ADI_NEW0( 'STRUC', ID, STATUS )

*  Initialise for multi-processing
      CALL BSP_START( MAXCPU, NCPU, MYCPU )

*  Write in data
      CALL ADI_CPUT0I( ID, 'MAXCPU', MAXCPU, STATUS )
      CALL ADI_CPUT0I( ID, 'NCPU', NCPU, STATUS )
      CALL ADI_CPUT0I( ID, 'MYCPU', MYCPU, STATUS )

*  Announce version
      SLAVE = (MYCPU.EQ.0)
      IF ( SLAVE ) THEN
        CALL MSG_SETI( 'N', MYCPU )
        CALL MSG_SETC( 'P', VERSION(:INDEX(VERSION,' ')-1) )
        CALL MSG_PRNT( 'Initialising ^P slave #^N...' )
      ELSE
        CALL MSG_PRNT( VERSION )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'MPC_INIT', STATUS )

      END
