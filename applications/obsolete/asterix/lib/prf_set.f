      SUBROUTINE PRF_SET( ID, FLAG, VALUE, STATUS )
*+
*  Name:
*     PRF_SET

*  Purpose:
*     Set the value of a logical processing flag

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PRF_SET( ID, FLAG, VALUE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of dataset
*     FLAG = CHARACTER*(*) (given)
*        The name of the flag
*     VALUE = LOGICAL (given)
*        The value of the flag
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
*     package:prf, usage:public

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     15 Feb 1995 (DJA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          			! SAE constants
      INCLUDE 'DAT_PAR'					! HDS constants

*  Arguments Given:
      INTEGER			ID			! Dataset id
      CHARACTER*(*)		FLAG			! Flag name
      LOGICAL			VALUE			! Flag value

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Temporary !
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get locator and invoke HDS version
      CALL ADI1_GETLOC( ID, LOC, STATUS )
      CALL PRF1_SET( LOC, FLAG, VALUE, STATUS )

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PRF_SET', STATUS )

      END



*+  PRF1_SET - Set a specified PROCESSING flag
      SUBROUTINE PRF1_SET( LOC, NAME, VALUE, STATUS )
*
*    Description :
*
*     Sets the value of a named processing flag. The name is given as
*     the HDS structure after the MORE.ASTERIX.PROCESSING object, eg.
*     CORRECTED.EXPOSURE or BGND_SUBTRACTED.
*
*    Method :
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (ROSAT,BHVAD::DJA)
*
*    History :
*
*     12 Nov 93 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)	LOC			! Top-level dataset
      CHARACTER*(*)		NAME			! Processing flag name
      LOGICAL                   VALUE			! Processing flag value
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)	FLOC			! Flag object
      CHARACTER*(DAT__SZLOC)	PLOC			! PROCESSING object
      CHARACTER*(DAT__SZLOC)	TLOC			! Cursor locator
      CHARACTER*(DAT__SZNAM)	CNAME			! Component name

      INTEGER			DPOS			! Dot position
      INTEGER			I			! Structure loop
      INTEGER			IC			! Character index

      LOGICAL                   THERE			! Component exists?
*
*    Local data :
*
      CHARACTER*(DAT__SZNAM)    SNAMES(3)               ! Sub-stucture names
      DATA                      SNAMES/'MORE','ASTERIX',
     :                                 'PROCESSING'/
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Does PROCESSING component exist?
      CALL HDX_FIND( LOC, 'MORE.ASTERIX.PROCESSING', PLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN

*      Ignore last status
        CALL ERR_ANNUL( STATUS )

*      Create the processing structure
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

*    Is this a structured flag, ie. more than a sub-component of PROCESSING?
      IC = 1
      IF ( INDEX(NAME,'.') .GT. 0 ) THEN

*      Process sub-component names in the flag name
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

*    Simple one-level flag
      ELSE
        CALL DAT_CLONE( PLOC, FLOC, STATUS )

      END IF

*    Write the flag value
      CALL HDX_PUTL( FLOC, NAME(IC:), 1, VALUE, STATUS )

*    Free flag container structure
      CALL DAT_ANNUL( FLOC, STATUS )

*    Free PROCESSING structure
      CALL DAT_ANNUL( PLOC, STATUS )

*    Report any errors (these would be HDS bugs, the sensible errors having
*    been trapped above).
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PRF1_SET', STATUS )

      END
