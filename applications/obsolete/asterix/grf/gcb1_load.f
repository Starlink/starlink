      SUBROUTINE GCB1_LOAD( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     GCB1_LOAD

*  Purpose:
*     Loads Grafix Control Block from HDS file object

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL GCB1_LOAD( NARG, ARGS, OARG, STATUS )

*  Description:
*     Loads the GCB from an HDS file.

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
*     GCB Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/gcb.html

*  Keywords:
*     package:gcb, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     12 Jan 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'GCB_PAR'

*  Global Variables:
      INCLUDE 'GCB_CMN'

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	GCBLOC			! GRAFIX_CONTROL object
      CHARACTER*(DAT__SZLOC)	GLOC			! GRAFIX box

      INTEGER			GCBPTR			! Ptr to mapped GCB
      INTEGER			NBYTE			! Size of GCB

      LOGICAL			OK			! Read from file?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      OK = .FALSE.

*  GRAFIX box exists?
      CALL ADI1_LOCGRAF( ARGS(1), .FALSE., GLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*    GCB in file?
        CALL DAT_THERE( GLOC, 'GRAFIX_CONTROL', OK, STATUS )
        IF ( OK ) THEN
          CALL DAT_FIND( GLOC, 'GRAFIX_CONTROL', GCBLOC, STATUS )
          CALL DAT_SIZE( GCBLOC, NBYTE, STATUS )
          CALL DAT_MAP( GCBLOC, '_BYTE', 'READ', 1, NBYTE,
     :                                    GCBPTR, STATUS )
          CALL GCB_LOAD_SUB( %VAL(GCBPTR), %val(G_MEMPTR), STATUS )

          CALL DAT_ANNUL( GCBLOC, STATUS )

*      Read it ok?
          OK = (STATUS.EQ.SAI__OK)

        END IF

      ELSE
        CALL ERR_ANNUL( STATUS )

      END IF

*  If not ok clear GCB in memory
      IF ( .NOT. OK ) THEN
        CALL GCB_CLEAR( STATUS )
      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'GCB1_LOAD', STATUS )

      END
