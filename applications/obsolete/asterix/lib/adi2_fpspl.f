      SUBROUTINE ADI2_FPSPL( FPATH, MAXW, START, STOP, NWRD, STATUS )
*+
*  Name:
*     ADI2_FPSPL

*  Purpose:
*     Split FITS file path into words

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL ADI2_FPSPL( FPATH, MAXW, START, STOP, NWRD, STATUS )

*  Description:
*     {routine_description}

*  Arguments:
*     FPATH = CHARACTER*(*) (given)
*        FITS sub-file specification
*     MAXW = INTEGER (given)
*        Maximum number of items in the path
*     START[MAXW] = INTEGER (returned)
*        Start positions of words in FPATH
*     STOP[MAXW] = INTEGER (returned)
*        Stop positions of words in FPATH
*     NWRD = INTEGER (returned)
*        Number of words read in FPATH
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
*     ADI Subroutine Guide : http://www.sr.bham.ac.uk/asterix-docs/Programmer/Guides/adi.html

*  Keywords:
*     package:adi, usage:private

*  Copyright:
*     Copyright (C) University of Birmingham, 1995

*  Authors:
*     DJA: David J. Allan (Jet-X, University of Birmingham)
*     {enter_new_authors_here}

*  History:
*     26 Sep 1995 (DJA):
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
      CHARACTER*(*)		FPATH
      INTEGER			MAXW

*  Arguments Returned:
      INTEGER			START(MAXW), STOP(MAXW), NWRD

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      INTEGER			DP, IP			! Character indices

      LOGICAL			MORE			! More bits to path?
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      NWRD = 0

*  While more FPATH
      IP = 1
      MORE = .TRUE.
      DO WHILE ( MORE )
        NWRD = NWRD + 1
        IF ( NWRD .GT. MAXW ) THEN
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'Too many FITS file path components',
     :                  STATUS )

        ELSE
          START(NWRD) = IP
          IP = IP + 1
          DP = INDEX( FPATH(IP:), '.' )
          IF ( DP .EQ. 0 ) THEN
            MORE = .FALSE.
            STOP(NWRD) = INDEX( FPATH, ' ' ) - 1
          ELSE
            STOP(NWRD) = IP + DP - 2
            IP = IP + DP
          END IF

        END IF
      END DO

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'ADI2_FPSPL', STATUS )

      END
