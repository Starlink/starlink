      SUBROUTINE PRF1_SET( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     PRF1_SET

*  Purpose:
*     Set the value of a logical processing flag

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PRF1_SET( NARG, ARGS, OARG, STATUS )

*  Description:
*     Sets the value of a named processing flag. The name is given as
*     the HDS structure after the MORE.ASTERIX.PROCESSING object, eg.
*     CORRECTED.EXPOSURE or BGND_SUBTRACTED.

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
*     PRF Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/prf.html

*  Keywords:
*     package:prf, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     23 Jan 1996 (DJA):
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

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	FLOC			! Flag object
      CHARACTER*(DAT__SZLOC)	LOC			! File object
      CHARACTER*40		NAME			! Flag name
      CHARACTER*(DAT__SZLOC)	PLOC			! PROCESSING object
      CHARACTER*(DAT__SZLOC)	TLOC			! Cursor locator
      CHARACTER*(DAT__SZNAM)	CNAME			! Component name

      INTEGER			DPOS			! Dot position
      INTEGER			I			! Structure loop
      INTEGER			IC			! Character index

      LOGICAL                   THERE			! Component exists?
      LOGICAL                   VALUE			! Flag value
*
*    Local data :
*
      CHARACTER*(DAT__SZNAM)    SNAMES(3)               ! Sub-stucture names
      DATA                      SNAMES/'MORE','ASTERIX',
     :                                 'PROCESSING'/
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the file locator
      CALL ADI1_GETLOC( IARG(2), LOC, STATUS )

*  Get flag name and value
      CALL ADI_GET0C( IARG(3), NAME, STATUS )

*  Does PROCESSING component exist?
      CALL ADI1_FIND( LOC, 'MORE.ASTERIX.PROCESSING', PLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN

*    Ignore last status
        CALL ERR_ANNUL( STATUS )

*    Create the processing structure
        CALL DAT_CLONE( LOC, TLOC, STATUS )
        DO I = 1, 3
          IF ( I .GT. 1 ) THEN
            CALL DAT_ANNUL( TLOC, STATUS )
            TLOC = PLOC
          END IF
          CALL DAT_THERE( TLOC, SNAMES(I), THERE, STATUS )
          IF ( .NOT. THERE ) THEN
            CALL DAT_NEW( TLOC, SNAMES(I), 'EXTENSION', 0, 0, STATUS )
          END IF
          CALL DAT_FIND( TLOC, SNAMES(I), PLOC, STATUS )
        END DO
        CALL DAT_ANNUL( TLOC, STATUS )

      END IF

*  Is this a structured flag, ie. more than a sub-component of PROCESSING?
      IC = 1
      IF ( INDEX(NAME,'.') .GT. 0 ) THEN

*    Process sub-component names in the flag name
        CALL DAT_CLONE( PLOC, TLOC, STATUS )
 20     DPOS = INDEX(NAME(IC:),'.')
        IF ( DPOS .GT. 0 ) THEN
          CNAME = NAME(IC:IC+DPOS-2)
          IF ( IC .GT. 1 ) THEN
            CALL DAT_ANNUL( TLOC, STATUS )
            TLOC = FLOC
          END IF
          CALL DAT_THERE( TLOC, CNAME, THERE, STATUS )
          IF ( .NOT. THERE ) THEN
            CALL DAT_NEW( TLOC, CNAME, 'EXTENSION', 0, 0, STATUS )
          END IF
          CALL DAT_FIND( TLOC, CNAME, FLOC, STATUS )
          IC = IC + DPOS
          IF ( STATUS .EQ. SAI__OK) GOTO 20
        END IF
        CALL DAT_ANNUL( TLOC, STATUS )

*  Simple one-level flag
      ELSE
        CALL DAT_CLONE( PLOC, FLOC, STATUS )

      END IF

*  Write the flag value
      CALL ADI1_CCA2HL( IARG(4), ' ', FLOC, NAME(IC:), STATUS )

*  Free flag container structure
      CALL DAT_ANNUL( FLOC, STATUS )

*  Free PROCESSING structure
      CALL DAT_ANNUL( PLOC, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PRF1_SET', STATUS )

      END
