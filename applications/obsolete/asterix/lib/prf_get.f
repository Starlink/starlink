      SUBROUTINE PRF_GET( ID, FLAG, VALUE, STATUS )
*+
*  Name:
*     PRF_GET

*  Purpose:
*     Get the value of a logical processing flag

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PRF_GET( ID, FLAG, VALUE, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     ID = INTEGER (given)
*        ADI identifier of dataset
*     FLAG = CHARACTER*(*) (given)
*        The name of the flag
*     VALUE = LOGICAL (returned)
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
      INTEGER			ID
      CHARACTER*(*)		FLAG

*  Arguments Returned:
      LOGICAL			VALUE

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*(DAT__SZLOC)	LOC			! Temporary !

      INTEGER			LID

      LOGICAL			ISHDS			! HDS object?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Derived from HDSfile?
      CALL ADI_GETLINK( ID, LID, STATUS )
      CALL ADI_DERVD( LID, 'HDSfile', ISHDS, STATUS )
      IF ( ISHDS ) THEN

*    Get locator and invoke HDS version
        CALL ADI1_GETLOC( ID, LOC, STATUS )
        CALL PRF1_GET( LOC, FLAG, VALUE, STATUS )
      ELSE
        VALUE = .FALSE.

      END IF

*  Report any errors
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PRF_GET', STATUS )

      END



*+  PRF1_GET - Is a specified PROCESSING flag set?
      SUBROUTINE PRF1_GET( LOC, NAME, VALUE, STATUS )
*
*    Description :
*
*     Returns the value of a named processing flag. The name is given as
*     the HDS structure after the MORE.ASTERIX.PROCESSING object, eg.
*     CORRECTED.EXPOSURE or BGND_SUBTRACTED.
*
*    Method :
*
*     If the flag exists, its value is returned. Otherwise the flag is
*     assumed to be false.
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
*
*    Export :
*
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
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Default is flag not set
      VALUE = .FALSE.

*    Does PROCESSING component exist?
      CALL HDX_FIND( LOC, 'MORE.ASTERIX.PROCESSING', PLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*      Does named flag exist?
        CALL HDX_FIND( PLOC, NAME, FLOC, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN

*        Get flag value
          CALL DAT_GET0L( FLOC, VALUE, STATUS )
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            VALUE = .FALSE.
          END IF

*        Free flag structure
          CALL DAT_ANNUL( FLOC, STATUS )

        ELSE
          CALL ERR_ANNUL( STATUS )
        END IF

*      Free PROCESSING structure
        CALL DAT_ANNUL( PLOC, STATUS )

      ELSE
        CALL ERR_ANNUL( STATUS )
      END IF

*    Report any errors (these would be HDS bugs, the sensible errors having
*    been trapped above).
      IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PRF1_GET', STATUS )

      END
