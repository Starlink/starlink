      SUBROUTINE PRF2_SET( NARG, ARGS, OARG, STATUS )
*+
*  Name:
*     PRF2_SET

*  Purpose:
*     Set the value of a logical processing flag

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL PRF2_SET( NARG, ARGS, OARG, STATUS )

*  Description:
*     Sets the value of a named processing flag in a generic FITS file

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

*  Arguments Given:
      INTEGER                   NARG, ARGS(*)

*  Arguments Returned:
      INTEGER                   OARG

*  Status:
      INTEGER 			STATUS             	! Global status

*  Local Variables:
      CHARACTER*50		CMNT			! Keyword comment
      CHARACTER*8		KEY			! Keyword name
      CHARACTER*40		NAME			! Flag name

      INTEGER			HDUID			! Primary HDU

      LOGICAL			VALUE			! Value of flag
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the file locator
      CALL ADI1_GETLOC( ARGS(2), LOC, STATUS )

*  Get flag name and value
      CALL ADI_GET0C( ARGS(3), NAME, STATUS )
      CALL ADI_GET0L( ARGS(4), VALUE, STATUS )

*  Switch on flags
      IF ( NAME .EQ. 'BGND_SUBTRACTED' ) THEN
        KEY = 'BGNDSUB'
        CMNT = 'Data is background subtracted?'

      ELSE IF ( NAME .EQ. 'CORRECTED.EXPOSURE', STATUS )
        KEY = 'EXPCOR'
        CMNT = 'Data is exposure corrected?'

      ELSE IF ( NAME .EQ. 'CORRECTED.VIGNETTING', STATUS )
        KEY = 'VIGCOR'
        CMNT = 'Data is corrected for vignetting?'

      ELSE IF ( NAME .EQ. 'CORRECTED.DEAD_TIME', STATUS )
        KEY = 'DTCOR'
        CMNT = 'Data is dead time corrected?'

      ELSE
        CALL MSG_SETC( 'FN', NAME )
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'WARNING : Unable to write value of '/
     :                         /'flag ^FN to FITS file', STATUS )
        CALL ERR_FLUSH( STATUS )
        GOTO 99

      END IF

*  Write flag
      CALL ADI2_FNDHDU( ARGS(2), 'PRIMARY', .TRUE., HDUID, STATUS )
      CALL ADI2_HPKYL( HDUID, KEY, VALUE, CMNT, STATUS )

*  Report any errors
 99   IF ( STATUS .NE. SAI__OK ) CALL AST_REXIT( 'PRF2_SET', STATUS )

      END
